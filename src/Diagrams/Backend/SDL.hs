{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.SDL
-- Copyright   :  (c) 2016 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The SDL backend comes in a few different levels
--
-- Mainable useage takes a diagram and loads a sdl window with it.
--
--
--
module Diagrams.SDL where

import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Data.Bits.Lens
import           Data.Bool
import           Data.Default
import           Data.Fixed                 (mod')
import qualified Data.Foldable              as F
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup
import           Data.Set                   (Set)
import           Foreign                    hiding (rotate)
import           Foreign.C.String
import           Graphics.GL
import           Linear.Affine
import           System.FilePath            ((</>))


import           Control.Monad              (unless, when)
import           Control.Monad.State        hiding (get)
import qualified Data.ByteString            as BS
import           Data.Distributive
import           Data.StateVar
import           Data.Typeable
import qualified Data.Vector.Storable       as S
import           Foreign.C
import           Linear
import qualified SDL.Raw                    as SDL
import           SDL.Raw.Enum
import           SDL.Raw.Types              hiding (Point, fingerX, fingerY)

import qualified Diagrams.Prelude           as D
import qualified Diagrams.ThreeD.Attributes as D
import qualified Diagrams.ThreeD.Light      as D
import qualified Diagrams.Types.Style       as D
import qualified Geometry.ThreeD.Shapes     as D

import           Diagrams.Backend.GL
import           Diagrams.SDL.Input

-- In version 3 I matched multitouch events with fingers.
--
-- In multitouch4 I've tried to change the camera to use yaw, pitch and
-- roll but failed miserably. I'll move on to something else and come
-- back to this....

------------------------------------------------------------------------
-- Input handling
------------------------------------------------------------------------

-- Perspective camera
data Camera = Camera
  { _fovy          :: !Float
  , _cameraPos     :: !(Point V3 Float)
  , _cameraPitch   :: !Float
  , _cameraYaw     :: !Float
  , _cameraRoll    :: !Float
  , _focalDistance :: !Float
  -- ^ the focus to rotate the camera around
  } deriving (Show, Eq, Ord)

instance Default Camera where
  def = Camera 0 (P $ V3 0 0 (-2)) 0 0 0 2

makeClassy ''Camera

-- | Change the offset by modifying the camera position. Only valid for
--   non-zero distances.
cameraOffset :: HasCamera a => Lens' a (V3 Float)
cameraOffset = camera . lens g s where
  g cam = -- cam^.focalDistance *^ rotate (cam^.camQuart) (V3 0 0 1)
    (cam^.focalDistance, cam^.cameraYaw, cam^.cameraPitch) ^. from r3SphericalIso
    -- where
    --   cameraQuat = axisAngle (V3 1 0 0) (cam^.cameraYaw) * axisAngle (V3 0 1 0) (cam^.cameraPitch)

  s cam v = cam &~ do
    let (r,yaw,pitch) = v^.r3SphericalIso
    focalDistance .= r
    cameraPitch   .= pitch
    cameraYaw     .= yaw
    cameraPos     %= \p -> (p .+^ (g cam)) .-^ v

objectDistance :: HasCamera a => Lens' a Float
objectDistance = camera . lens g s where
  g cam   = cam^.focalDistance
  s cam d = cam & cameraPos %~ (\p -> p .-^ (d-n)*^v)
                & focalDistance .~ d
    where
      v = cam^.cameraOffset
      n = norm v

camQuart :: HasCamera a => Lens' a (Quaternion Float)
camQuart = camera . lens g s where
  g cam = product
    [--  axisAngle (V3 0 0 1) (cam^.cameraRoll)
      axisAngle (V3 0 1 0) (cam^.cameraYaw)
    , axisAngle (V3 1 0 0) (cam^.cameraPitch)
    ]

  s cam (Quaternion q0 (V3 q1 q2 q3)) = cam &~ do

    cameraYaw   .= asin (2*(q0*q2 - q3*q1))
    cameraPitch .= atan2 (2*(q0*q1 - q2*q3)) (1 - 2*(q1*q1+q2*q2))
    cameraRoll  .= atan2 (2*(q1*q2 - q0*q3)) (1 - 2*(q2*q2+q3*q3))

objectYaw :: HasCamera a => Lens' a Float
objectYaw = camera . lens g s where -- cameraYaw . negating
  g cam   = cam^.cameraYaw.negating
  s cam a = cam & cameraOffset . r3SphericalIso . _2 .~ -a

negating :: Num a => Iso' a a
negating = iso ((-1) *) ((-1) *)

objectPitch :: HasCamera a => Lens' a Float
objectPitch = cameraOffset . r3SphericalIso . _3 -- lens g s where -- cameraYaw . negating

r3SphericalIso :: RealFloat n => Iso' (V3 n) (n, n, n)
r3SphericalIso = zxy . iso
  (\v@(V3 x y z) -> (norm v, atan2 y x, asin (z / norm v)))
  (\(r,θ,φ)   -> V3 (r * cos θ * cos φ) (r * sin θ * cos φ) (r * sin φ))
  where
  zxy = iso (\(V3 x y z) -> V3 z x y) (\(V3 z x y) -> (V3 x y z))

data Locations = Locations
  { _mvpLocation    :: !GLint
  , _viewLocation   :: !GLint
  , _lightLocation  :: !GLint
  , _colourLocation :: !GLint
  }

makeClassy ''Locations

data Info = Info
  { _stateCamera      :: !Camera
  , _shouldClose      :: !Bool
  , _shouldRedraw     :: !Bool
  , _mousePressed     :: !Bool
  , _inFocus          :: !Bool
  , _shown            :: !Bool
  , _infoInput        :: !Input
  , _infoInfo         :: (ProgramInfo, LineProgram, RenderInfo)
  , _infoMultiGesture :: !MultiGesture
  }

makeClassy ''Info

instance HasCamera Info where
  camera = stateCamera

instance HasInput Info where
  input = infoInput

initialState :: (ProgramInfo, LineProgram, RenderInfo) -> Info
initialState info = Info
  { _stateCamera   = def
  , _shouldClose   = False
  , _shouldRedraw  = True
  , _mousePressed  = False
  , _inFocus       = True
  , _shown         = True
  , _infoInput     = def
  , _infoInfo      = info
  , _infoMultiGesture  = NoGesture
  }

searchFingers :: (MonadState s m, HasInput s, HasInfo s) => TouchID -> m (Maybe MultiFingers)
searchFingers touchID = uses fingers $ \fs ->
  case fs ^@.. ifolded . filtered ((==touchID) . view fingerDevice) of
    [(id1,f1),(id2,f2)] -> Just (MultiFingers touchID id1 id2)
    _                   -> Nothing

lookupFingers
  :: (MonadState s m, HasInput s, HasInfo s)
  => MultiFingers
  -> m (Maybe (FingerInfo, FingerInfo))
lookupFingers fs = do
  mf1 <- use (fingers.at (fs^.finger1))
  mf2 <- use (fingers.at (fs^.finger2))
  pure $ liftM2 (,) mf1 mf2

updateCamera :: (MonadState s m, HasCamera s) => Movement -> m ()
updateCamera m = do
  let movespeed  = 0.05
  let orbitspeed = 0.01

  -- movements independent of focus
  forward <- uses cameraOffset signorm
  let up         = V3 0 1 0
      right      = forward `cross` up
      moveVector :: V3 Float
      moveVector = movespeed *^ sum
        [ (50*_moveZoom m + _moveForward m) *^ forward
        , _moveRight m *^ right
        , _moveUp m *^ up
        ]
  cameraPos     %= (.+^ moveVector)
  focalDistance %= \d -> clamp (d - movespeed * (_moveZoom m + _moveForward m)) 0.1 20

  cameraOffset . r3SphericalIso %= \(r,p,y) ->
    let p' = p - orbitspeed * _orbitRight m
        y' = clamp (y - orbitspeed * _orbitUp m) (0.01-pi/2) (pi/2 - 0.01)
    in  (r, p', y')
  -- cameraRoll += turnRoll

clamp :: Ord a => a -> a -> a -> a
clamp x a b
  | x < a     = a
  | x > b     = b
  | otherwise = x

moveCamera :: (MonadState s m, HasCamera s) => Float -> V3 Float -> m ()
moveCamera dt dir = cameraPos %= \pos -> pos .+^ (movementSpeed * dt) *^ dir
  where movementSpeed = 3

-- | This is thrown in the event of an error in the @Quine.SDL@ combinators
newtype SDLException = SDLException String
  deriving (Show, Typeable)

instance Exception SDLException

-- | Treat negative return codes as prompting an error check.
err :: MonadIO m => CInt -> m ()
err e
  | e < 0 = liftIO $ do
    msg <- SDL.getError >>= peekCString
    SDL.clearError
    when (msg /= "") $ print (SDLException msg)
  | otherwise = return ()

-- | Use a GLattr as a variable
attr :: GLattr -> StateVar Int
attr a = StateVar (getAttr a) (glSetAttr a)

boolAttr :: GLattr -> StateVar Bool
boolAttr = mapStateVar fromEnum toEnum . attr

getAttr :: GLattr -> IO Int
getAttr a = alloca $ \p -> do
 SDL.glGetAttribute a p >>= err
 fromIntegral <$> peek p

glSetAttr :: GLattr -> Int -> IO ()
glSetAttr a i = SDL.glSetAttribute a (fromIntegral i) >>= err

mainSDL :: D.Diagram V3 -> IO ()
mainSDL dia = do
  SDL.init (SDL_INIT_TIMER .|. SDL_INIT_VIDEO .|. SDL_INIT_EVENTS) >>= err
  glSetAttr SDL_GL_CONTEXT_MAJOR_VERSION 4
  glSetAttr SDL_GL_CONTEXT_MINOR_VERSION 1
  glSetAttr SDL_GL_CONTEXT_PROFILE_MASK SDL_GL_CONTEXT_PROFILE_CORE

  glSetAttr SDL_GL_RED_SIZE   5
  glSetAttr SDL_GL_GREEN_SIZE 5
  glSetAttr SDL_GL_BLUE_SIZE  5
  glSetAttr SDL_GL_DEPTH_SIZE 16
  glSetAttr SDL_GL_DOUBLEBUFFER 1

  -- multisampleing
  glSetAttr SDL_GL_MULTISAMPLEBUFFERS 2
  glSetAttr SDL_GL_MULTISAMPLESAMPLES 16

  let flags =  SDL_WINDOW_OPENGL
           .|. SDL_WINDOW_SHOWN
           .|. SDL_WINDOW_RESIZABLE
           .|. SDL_WINDOW_ALLOW_HIGHDPI

  window <- withCString "shaders" $ \windowName ->
    SDL.createWindow
      windowName
      SDL_WINDOWPOS_CENTERED
      SDL_WINDOWPOS_CENTERED
      512
      512
      flags

  glContext <- SDL.glCreateContext window
  SDL.glMakeCurrent window glContext >>= err

  SDL.glSetSwapInterval 1 >>= err

  gl window dia

  glFinish
  SDL.glDeleteContext glContext
  SDL.quit

draw
  :: (MonadIO m, MonadState s m, HasCamera s, HasInfo s)
  => SDL.Window -> m ()
draw window = do
  glClearColor 0.8 0.8 0.8 1
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  cam  <- use camera

  -- let viewMat' = (identity & translation .~ (cam^.cameraPos._Point))
  --       !*! mkTransformation (cam^.camQuart) zero
  -- let viewMat = mkTransformation (cam^.camQuart) (cam^.cameraPos._Point)
  -- let viewMat  = viewMat' !*! modelMat
  let viewMat  = lookAt (cam^.cameraPos._Point)
                        -- zero
                        (view _Point $ cam^.cameraPos .+^ cam^.cameraOffset)
                        (V3 0 1 0)
  let projectionMatrix = perspective (3*pi/8) (4/3) 0.1 500 :: M44 Float
  -- let projectionMatrix = ortho (-5) 5 (-5) 5 0.1 100 :: M44 Float

  (progInfo, lprog, renderInfo) <- use infoInfo

  liftIO $ do
    drawRender viewMat projectionMatrix progInfo lprog renderInfo

  SDL.glSwapWindow window

gl :: SDL.Window -> D.Diagram V3 -> IO ()
gl window dia = do
  -- The setup

  -- let path = "/Users/christopher/Documents/gl/own/events"

  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LESS
  glEnable GL_CULL_FACE


  glClearColor 0 0 0 1
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  basicProg <- initProgram
  lineProg  <- lineProgram
  render <- runRender (toRender mempty dia)

  info <- diagramRender dia

  flip evalStateT (initialState info) $ do
    let kernel = do
          shouldRedraw .= False

          events <- pollEvents
          F.for_ events handleInputEvent
          mj <- use multiGesture
          movement <- orbitalMovement
          when (movement /= def) $ do
            updateCamera movement
            shouldRedraw .= True

          use (mouseButtons . contains SDL_BUTTON_LEFT) >>= SDL.captureMouse >>= err

          escPressed <- use (keycode SDLK_ESCAPE)
          qPressed   <- use (keycode SDLK_q)
          when (escPressed || qPressed) $ shouldClose .= True

          doRedraw <- use shouldRedraw
          if doRedraw
            then do draw window
            else liftIO $ threadDelay 16000

          unlessM (use shouldClose .||. SDL.quitRequested) kernel

    draw window
    kernel

ppMovement :: Movement -> IO ()
ppMovement m = do
  putStrLn $ "forward:   " ++ padShow (_moveForward m)
  putStrLn $ "right:     " ++ padShow (_moveRight m)
  putStrLn $ "up:        " ++ padShow (_moveUp m)
  putStrLn $ "zoom:      " ++ padShow (_moveZoom m)
  putStrLn $ "roll:      " ++ padShow (_turnRoll m)
  putStrLn $ "turn right:" ++ padShow (_orbitRight m)
  putStrLn $ "turn up:   " ++ padShow (_orbitUp    m)

ppGesture :: MultiGesture -> IO ()
ppGesture NoGesture = putStrLn "No gesture"
ppGesture (UndecidedGesture _ z t p m)
                          = putStrLn $ "Undecided: " ++ padShow z ++ padShow t
ppGesture (ZoomGesture _ z) = putStrLn $ "Zoom:      " ++ padShow z
ppGesture (RotateGesture _ r) = putStrLn $ "Rotate:    " ++ padShow r
ppGesture (MoveGesture _ (P (V2 x y)) (V2 dx dy)) = putStrLn $ "Move:      " ++ padShow x ++ padShow y ++ padShow dx ++ padShow dy

padShow :: Show a => a -> String
padShow = (++ " ") . take 4 . (++ repeat ' ') . show

(.||.) :: Monad m => m Bool -> m Bool -> m Bool
(.||.) ma mb = do
  a <- ma
  if a then return True
       else mb

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb action = do
  b <- mb
  unless b action

