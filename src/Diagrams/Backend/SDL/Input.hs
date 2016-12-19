{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}

module Diagrams.Backend.SDL.Input where

import           Control.Lens
import           Control.Monad.State hiding (get)
import           Data.Bits.Lens
import           Data.Default
import           Data.Map            (Map)
import           Data.Maybe          (fromMaybe, isJust)
import           Data.Set            (Set)
import           Foreign             hiding (rotate)
import           Linear
import           Linear.Affine

import qualified SDL.Raw             as SDL
import           SDL.Raw.Enum
import           SDL.Raw.Types       hiding (Point, fingerX, fingerY)

------------------------------------------------------------------------
-- Input handling
------------------------------------------------------------------------

type P2 = Point V2

data FingerInfo = FingerInfo
  { _fingerDevice :: !TouchID
  , _fingerPos    :: !(V2 Float)
  , _fingerRel    :: !(V2 Float)
  } deriving Show

makeLenses ''FingerInfo

-- | We need to keep track of the current multigesture being attempted.
--   i.e if we're currently zooming, don't also move around or rotate,
--   only zoom.
--
--   Deciding which gesture the user it attempting cannot be
--   done in one frame, we need to accumulate enough data until we can
--   pick one. This accumulation is done in UndecidedGesture. Once
--   one gesture is prominent, pick it and use it until one or more
--   fingers are removed.
data MultiGesture
  = NoGesture
  | UndecidedGesture
      { _gestureFingers :: !MultiFingers
      , _gestureZoom    :: !Float
      , _gestureTheta   :: !Float
      , _gesturePos     :: !(P2 Float)
      , _gestureMove    :: !(V2 Float)
      }

      -- The ammount of zooming from a gesture.
  | ZoomGesture
      { _gestureFingers :: !MultiFingers
      , _gestureZoom    :: !Float
      }

      -- The ammount of rotation from a gesture, in radians.
  | RotateGesture
      { _gestureFingers :: !MultiFingers
      , _guestureTheta  :: !Float
      }

      -- move gestures need to keep track of the current (avg) position
      -- of the gesture becuase the 'MultiGestureEvent' only gives the
      -- current position and we want the change in position.
  | MoveGesture
      { _gestureFingers :: !MultiFingers
      , _gesturePos     :: !(P2 Float)
      , _gestureMove    :: !(V2 Float)
      }
  deriving Show

data MultiFingers = MultiFingers
  { _multiDevice :: !TouchID
  , _finger1     :: !FingerID
  , _finger2     :: !FingerID
  }
  deriving Show

makeLenses ''MultiGesture
makeClassy ''MultiFingers

------------------------------------------------------------------------
-- Input
------------------------------------------------------------------------

data Input = Input
  { _keycodes        :: !(Set Keycode)
  , _scancodes       :: !(Set Scancode)
  , _mouseButtons    :: !(Set Word8)
  , _mousePos        :: !(V2 Int32)
  , _mouseButtonMask :: !Word32
  , _mouseRel        :: !(V2 Int32) -- relative since last reset
  , _mouseWheel      :: !(V2 Int32) -- relative since last reset
  , _multiGesture    :: !MultiGesture
  , _fingers         :: !(Map FingerID FingerInfo)
  } deriving Show

makeClassy ''Input

instance Default Input where
  def = Input
    { _keycodes        = mempty
    , _scancodes       = mempty
    , _mouseButtons    = mempty
    , _mousePos        = zero
    , _mouseButtonMask = 0
    , _mouseRel        = zero
    , _mouseWheel      = zero
    , _multiGesture    = NoGesture
    , _fingers         = mempty
    }

-- | Lens onto whether the keycode is currently pressed.
keycode :: HasInput s => Keycode -> Lens' s Bool
keycode c = keycodes . contains c

-- | Lens onto whether the scancode is currently pressed.
scancode :: HasInput s => Scancode -> Lens' s Bool
scancode c = scancodes . contains c

r2f :: (Real a, Fractional b) => a -> b
r2f = realToFrac

mkP2 :: a -> a -> P2 a
mkP2 a b = P (V2 a b)

handleInputEvent :: (MonadState s m, HasInput s) => SDL.Event -> m ()
handleInputEvent = \case

  KeyboardEvent SDL_KEYDOWN _ _ _ _ (Keysym sc kc _) -> do
    keycode kc  .= True
    scancode sc .= True

  KeyboardEvent SDL_KEYUP _ _ _ _ (Keysym sc kc _) -> do
    keycode kc  .= False
    scancode sc .= False

  MouseMotionEvent SDL_MOUSEMOTION _ _ mouse mbMask x y relx rely
    | mouse /= SDL_TOUCH_MOUSEID -> do
        mousePos        .= V2 x y
        mouseButtonMask .= mbMask
        mouseRel        += V2 relx rely

  MouseButtonEvent SDL_MOUSEBUTTONDOWN _ _ mouse button _ _ x y
    | mouse /= SDL_TOUCH_MOUSEID -> do
        mousePos .= V2 x y
        mouseButtons.contains button .= True
        mouseButtonMask.bitAt (fromIntegral button) .= True

  MouseButtonEvent SDL_MOUSEBUTTONUP _ _ mouse button _ _ x y
    | mouse /= SDL_TOUCH_MOUSEID -> do
        mousePos .= V2 x y
        mouseButtons.contains button .= False
        mouseButtonMask.bitAt (fromIntegral button) .= False

  MouseWheelEvent SDL_MOUSEWHEEL _ _ mouse x y
    | mouse /= SDL_TOUCH_MOUSEID ->
        mouseWheel += V2 x y

  MultiGestureEvent _ _ touchID
    (r2f -> dTheta)
    (r2f -> dDist)
    (r2f -> x) (r2f -> y) 2 -> do

    let -- see if a multigesture has moved enough in some parameter to
        -- decided which gesture to use
        -- note once we pick a gesture we set the ammount of that gesture
        -- to zero so there's not a sudden jerk of movement once it's
        -- picked.
        tryGesture :: MultiFingers
                   -> Float -> Float -> P2 Float -> V2 Float -> MultiGesture
        tryGesture fs z r xy dxy
          | abs z > 0.03           = ZoomGesture fs 0
          | abs r > 0.05           = RotateGesture fs 0
          | quadrance dxy > 0.0002 = MoveGesture fs xy zero
          | otherwise              = UndecidedGesture fs z r xy dxy

    gesture <- use multiGesture
    case gesture of
      NoGesture ->
        searchFingers touchID >>= \case
          Nothing -> return ()
          Just fs -> multiGesture .= UndecidedGesture fs dDist dTheta (mkP2 x y) zero
      UndecidedGesture fs z r xy dxy -> do
        confirmFingers fs $
          multiGesture .=
            tryGesture
              fs
              (z + dDist)
              (r + dTheta)
              (mkP2 x y)
              (dxy ^+^ (mkP2 x y .-. xy))
      ZoomGesture fs z ->
        confirmFingers fs $
          multiGesture .= ZoomGesture fs (z + dDist)
      MoveGesture fs xy dxy ->
        confirmFingers fs $ do
          multiGesture .= MoveGesture fs (mkP2 x y) (dxy ^+^ (mkP2 x y .-. xy))
      RotateGesture fs r ->
        confirmFingers fs $ do
          multiGesture .= RotateGesture fs (r + dTheta)

  TouchFingerEvent eType _ touchID fingerId x y dx dy _ -> do
    fingers . at fingerId %= \mfinger ->
      case eType of
        SDL_FINGERUP   -> Nothing
        _              -> Just $
          fromMaybe (FingerInfo touchID zero zero) mfinger &~ do
            fingerPos .= fmap realToFrac (V2 x y)
            fingerRel += fmap realToFrac (V2 dx dy)

  _ -> return ()

pollEvents :: MonadIO m => m [Event]
pollEvents = liftIO $ alloca $ \ep -> do
  let go = do
        n <- SDL.pollEvent ep
        if n == 0
         then return []
         else do e  <- peek ep
                 es <- go
                 return (e:es)
  go

-- Multitouch ----------------------------------------------------------

searchFingers :: (MonadState s m, HasInput s) => TouchID -> m (Maybe MultiFingers)
searchFingers touchID = uses fingers $ \fs ->
  case fs ^@.. ifolded . filtered ((==touchID) . view fingerDevice) of
    [(id1,_),(id2,_)] -> Just (MultiFingers touchID id1 id2)
    _                 -> Nothing

lookupFingers
  :: (MonadState s m, HasInput s)
  => MultiFingers
  -> m (Maybe (FingerInfo, FingerInfo))
lookupFingers fs = do
  mf1 <- use (fingers.at (fs^.finger1))
  mf2 <- use (fingers.at (fs^.finger2))
  pure $ liftM2 (,) mf1 mf2

-- Only perform a monadic action if the fingers are still touching the
-- device, otherwise set the current gesture to 'NoGesture'.
confirmFingers :: (MonadState s m, HasInput s) => MultiFingers -> m () -> m ()
confirmFingers fs s = do
  has1 <- uses (fingers.at (fs^.finger1)) isJust
  has2 <- uses (fingers.at (fs^.finger2)) isJust
  if has1 && has2
    then s
    else multiGesture .= NoGesture

-- | The movement defines the requested changes from the input and/or
--   other sources. Once all the movement is accumilated this is
--   translated into a change in the camera.
data Movement = Movement
  { _moveForward :: !Float
  , _moveRight   :: !Float
  , _moveUp      :: !Float
  , _moveZoom    :: !Float
  , _turnRight   :: !Float
  , _turnUp      :: !Float
  , _turnRoll    :: !Float
  , _orbitRight  :: !Float
  , _orbitUp     :: !Float
  } deriving (Show, Eq, Ord)

makeLenses ''Movement

instance Default Movement where
  def = Movement 0 0 0 0 0 0 0 0 0

firstPersonMovement :: (MonadState s m, HasInput s) => m Movement
firstPersonMovement = do
  i <- use input

  -- clear relative inputs
  fingers.each.fingerRel .= zero
  mouseRel .= zero
  multiGesture %= \case
    ZoomGesture fs _   -> ZoomGesture fs 0
    RotateGesture fs _ -> RotateGesture fs 0
    MoveGesture fs p _ -> MoveGesture fs p 0
    g                  -> g

  return $ def &~ do

    -- key movements
    let codes = i^.scancodes
    let whenKey k s = when (codes^.contains k) s
    whenKey SDL_SCANCODE_W     $ moveForward += 1
    whenKey SDL_SCANCODE_S     $ moveForward -= 1
    whenKey SDL_SCANCODE_A     $ moveRight   -= 1
    whenKey SDL_SCANCODE_D     $ moveRight   += 1
    whenKey SDL_SCANCODE_X     $ moveUp      -= 1
    whenKey SDL_SCANCODE_SPACE $ moveUp      += 1

    -- mouse movements
    let mouseDown = view (mouseButtons . contains SDL_BUTTON_LEFT) i
    when mouseDown $ do
      let V2 x y = views mouseRel (fmap fromIntegral) i
      turnRight .= x
      turnUp    .= y

    -- multitouch
    case i^.multiGesture of
      ZoomGesture _ z          -> moveZoom += z
      RotateGesture _ a        -> turnRoll += a
      MoveGesture _ _ (V2 x y) -> do moveRight += 50*x
                                     moveUp    -= 50*y
      _                        -> return ()


orbitalMovement :: (MonadState s m, HasInput s) => m Movement
orbitalMovement = do
  i <- use input

  -- clear relative inputs
  fingers.each.fingerRel .= zero
  mouseRel .= zero
  multiGesture %= \case
    ZoomGesture fs _   -> ZoomGesture fs 0
    RotateGesture fs _ -> RotateGesture fs 0
    MoveGesture fs p _ -> MoveGesture fs p 0
    g                  -> g

  return $ def &~ do

    -- key movements
    let codes = i^.scancodes
    let whenKey k s = when (codes^.contains k) s
    whenKey SDL_SCANCODE_W     $ moveForward += 1
    whenKey SDL_SCANCODE_S     $ moveForward -= 1
    whenKey SDL_SCANCODE_A     $ moveRight   -= 1
    whenKey SDL_SCANCODE_D     $ moveRight   += 1
    whenKey SDL_SCANCODE_X     $ moveUp      -= 1
    whenKey SDL_SCANCODE_SPACE $ moveUp      += 1

    -- mouse movements
    let mouseDown = view (mouseButtons . contains SDL_BUTTON_LEFT) i
    when mouseDown $ do
      let V2 x y = views mouseRel (fmap fromIntegral) i
      orbitRight .= x
      orbitUp    .= y

    -- multitouch
    case i^.multiGesture of
      ZoomGesture _ z          -> moveZoom += z
      RotateGesture _ a        -> turnRoll += a
      MoveGesture _ _ (V2 x y) -> do moveRight += 50*x
                                     moveUp    -= 50*y
      _                        -> return ()


