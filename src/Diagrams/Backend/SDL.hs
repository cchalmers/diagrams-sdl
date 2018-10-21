{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.SDL
-- Copyright   :  (c) 2016 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The SDL backend comes in a few different levels. The simplest case
--
-- Mainable useage takes a diagram and loads a sdl window with it.
--
module Diagrams.Backend.SDL where

import           Control.Concurrent
import           Control.Lens
import           Data.Default
import qualified Data.Foldable              as F
import           Foreign                    hiding (rotate)
import           Graphics.GL
import           Linear.Affine
import qualified Options.Applicative        as OP
import qualified Options.Applicative.Types  as OP (readerAsk)
import           Control.Monad              (unless, when)
import           Control.Monad.State        hiding (get)
import           Linear

import           Geometry.Space
import           Geometry.ThreeD.Camera
import           Geometry.ThreeD.Transform
import           Geometry.TwoD.Size         as D

import qualified Diagrams.Prelude           as D
import           Diagrams.Backend
import           Diagrams.Backend.GL hiding (windowSize)
import           Diagrams.Backend.SDL.Input
import           Diagrams.Backend.SDL.Util

import qualified SDL.Raw                    as SDL
import           SDL.Raw.Enum

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

type Cam = Camera PerspectiveLens Float

data SDLState = SDLState
  { _stateCamera      :: !Cam
  , _shouldClose      :: !Bool
  , _shouldRedraw     :: !Bool
  , _mouseDownPos     :: !(Maybe (V2 Int32))
  , _windowSize       :: !(V2 Int)
  , _inFocus          :: !Bool
  , _shown            :: !Bool
  , _infoInput        :: !Input
  , _sdlDrawInfo      :: !RenderInfo
  , _infoMultiGesture :: !MultiGesture
  }

makeClassy ''SDLState

class HasCamera a where
  camera :: Lens' a Cam

instance HasCamera SDLState where
  camera = stateCamera

instance HasRenderInfo SDLState where
  renderInfo = sdlDrawInfo

instance HasInput SDLState where
  input = infoInput

mkInitialState :: V2 Int -> RenderInfo -> SDLState
mkInitialState sz i = SDLState
  { _stateCamera   = mm50Camera
      & cameraLens .~ (PerspectiveLens (3*pi/8 D.@@ D.rad) (4/3 D.@@ D.rad) 0.1 500)
  , _shouldClose   = False
  , _shouldRedraw  = True
  , _inFocus       = True
  , _mouseDownPos  = Nothing
  , _windowSize    = sz
  , _shown         = True
  , _infoInput     = def
  , _sdlDrawInfo   = i
  , _infoMultiGesture  = NoGesture
  }

------------------------------------------------------------------------
-- SDL Token
------------------------------------------------------------------------

-- | The SDL backend token.
data SDL = SDL

type instance V SDL = V3
type instance N SDL = Double

instance Backend SDL where
  type Result  SDL = IO ()
  data Options SDL = SDLOptions
    { _initialCamera :: Cam    -- ^ Surface you want to use.
    , _sdlSize       :: V2 Int -- ^ The size of the sdl window
    , _thirdPerson   :: Bool   -- ^ Indented lines for @.tex@ output.
    , _sdlTitle      :: String
    , _sdlBgColour   :: D.Colour Double
    }
  renderDiaT _ _ = (zero, mempty, pure ())
  backendInfo _ = BackendInfo
    { backendModuleName  = "diagrams-sdl"
    , backendTokenName   = "SDL"
    , backendModules     = ["Diagrams.Backend.SDL"]
    , backendNames       = []
    , backendExtensions  = []
    }

sdlTitle :: Lens' (Options SDL) String
sdlTitle f opts = f (_sdlTitle opts) <&> \t -> opts {_sdlTitle = t}

sdlSize :: Lens' (Options SDL) (V2 Int)
sdlSize f opts = f (_sdlSize opts) <&> \t -> opts {_sdlSize = t}

sdlBgColour :: Lens' (Options SDL) (D.Colour Double)
sdlBgColour f opts = f (_sdlBgColour opts) <&> \t -> opts {_sdlBgColour = t}

defaultCamera :: Camera PerspectiveLens Float
defaultCamera = mm50Camera
  & cameraLens .~ (PerspectiveLens (3*pi/8 D.@@ D.rad) (4/3 D.@@ D.rad) 0.1 500)

optionsParser :: OP.Parser (Options SDL)
optionsParser =
    SDLOptions <$> camParser
               <*> wsizeParser
               <*> thirdPersonParser
               <*> titleParser
               <*> bgParser
    where

    camParser = mkCamParser <$> pos <*> dir
    mkCamParser mpos mdir = defaultCamera &~ do
      F.for_ mpos $ \p -> cameraLoc  .= p
      F.for_ mdir $ \p -> camForward .= signorm p

    pos = OP.optional . OP.option OP.auto $ mconcat
      [ OP.long "camera-pos", OP.short 'p', OP.metavar "(FLOAT,FLOAT,FLOAT)"
      , OP.help "position of the camera" ]

    dir = OP.optional . OP.option OP.auto $ mconcat
      [ OP.long "camera-dir", OP.short 'd', OP.metavar "(FLOAT,FLOAT,FLOAT)"
      , OP.help "direction the camera is facing" ]

    -- target = mconcat
    --   [ OP.long "camera-target", OP.short 't', OP.metavar "(FLOAT,FLOAT,FLOAT)"
    --   , OP.help "direction the camera is facing" ]

    thirdPersonParser = OP.switch $ mconcat
      [ OP.long "third", OP.short 't'
      , OP.help "Use a third person camera" ]

    wsizeParser = D.specToSize 800 <$> msizeParser
    msizeParser = D.mkSizeSpec2D <$> o w <*> o h where
      o = OP.optional . OP.option OP.auto
      w = mconcat [ OP.long "width", OP.short 'w', OP.metavar "INT"
                  , OP.help "Width of the window"]
      h = mconcat [ OP.long "height", OP.short 'h', OP.metavar "INT"
                  , OP.help "Height of the window"]

    titleParser = OP.option OP.str $ mconcat
      [ OP.long "title", OP.short 't', OP.metavar "STRING"
      , OP.help "window title", OP.value "diagram" ]

    bgParser = OP.option (rc OP.<|> rh) $ mconcat
      [ OP.long "bg-colour", OP.short 'b', OP.metavar "COLOUR"
      , OP.help "the background colour", OP.value D.grey ]
    rh, rc :: OP.ReadM (D.Colour Double)
    rh = f . D.colorToSRGBA <$> (OP.readerAsk >>= readHexColor)
    rc = OP.readerAsk >>= D.readColourName
    f (r,g,b,_) = D.sRGB r g b

instance RenderOutcome SDL (D.Diagram V3) where
  type MainOpts SDL (D.Diagram V3) = Options SDL
  resultParser _ _ = optionsParser
  renderOutcome _ = mainSDL

------------------------------------------------------------------------
-- SDL rendering
------------------------------------------------------------------------

speedLimit :: Float -> V3 Float -> V3 Float
speedLimit l v
  | s > l     = (l/s) *^ v
  | otherwise = v
  where s = norm v

updateCameraFP :: (MonadState s m, HasCamera s) => Movement -> m ()
updateCameraFP m = do
  let movespeed  = 0.03
  let orbitspeed = 0.005

  (forward,right) <- uses camera camForwardRight

  let up      = V3 0 1 0
      moveVector = movespeed *^ sum
        [ (50*_moveZoom m + _moveForward m) *^ forward
        , _moveRight m *^ right
        , _moveUp m *^ up
        ]

  camera.cameraLoc     %= (.+^ speedLimit movespeed moveVector)

  camera.yaw   %= \y -> y ^-^ (orbitspeed * _turnRight m D.@@ D.rad)
  camera.pitch %= \p -> (min (pi/2) (max (-pi/2) (p^.D.rad - orbitspeed * _turnUp m)) D.@@ D.rad)
  camera.roll <>= (_turnRoll m D.@@ D.rad)

-- updateCamera :: (MonadState s m, HasCamera s) => Movement -> m ()
-- updateCamera m = do
--   let movespeed  = 0.05
--   let orbitspeed = 0.01

--   -- movements independent of focus
--   forward <- uses cameraOffset signorm
--   let up         = V3 0 1 0
--       right      = normalize (forward `cross` up)
--       moveVector :: V3 Float
--       moveVector = movespeed *^ sum
--         [ (50*_moveZoom m + _moveForward m) *^ forward
--         , _moveRight m *^ right
--         , _moveUp m *^ up
--         ]
--   cameraPos     %= (.+^ moveVector)
--   focalDistance %= \d -> clamp (d - movespeed * (_moveZoom m + _moveForward m)) 0.1 20

--   cameraOffset . r3SphericalIso %= \(r,p,y) ->
--     let p' = p - orbitspeed * _orbitRight m
--         y' = clamp (y - orbitspeed * _orbitUp m) (0.01-pi/2) (pi/2 - 0.01)
--     in  (r, p', y')
--   cameraRoll += _turnRoll m

mainSDL :: Options SDL -> D.Diagram V3 -> IO ()
mainSDL opts dia = do
  defaultSdlSetup

  window  <- defaultSdlWindow (opts^.sdlTitle) (opts^.sdlSize)
  context <- sdlGlContext window
  render  <- diagramRender dia
  let state0 = mkInitialState (opts^.sdlSize) render

  gl window state0

  glFinish
  SDL.glDeleteContext context
  SDL.quit

draw
  :: (MonadIO m, MonadState s m, HasCamera s, HasRenderInfo s)
  => SDL.Window -> V2 Int -> m ()
draw window _sz = do
  glClearColor 0.8 0.8 0.8 1
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  cam <- use camera

  sz <- liftIO $
    alloca $ \wPtr ->
    alloca $ \hPtr -> do
      SDL.getWindowSize window wPtr hPtr
      w <- peek wPtr
      h <- peek hPtr
      return (fromIntegral <$> V2 w h)

  -- print sz

  let viewMat          = cameraView cam
  let projectionMatrix = lensProjection cam
  let mats = SceneView sz viewMat projectionMatrix

  render <- use renderInfo
  liftIO $ drawScene mats render

  SDL.glSwapWindow window

gl :: SDL.Window -> SDLState -> IO ()
gl window sdlstate = do

  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LESS
  -- glEnable GL_CULL_FACE

  glClearColor 0 0 0 1
  glClear $  GL_COLOR_BUFFER_BIT
         .|. GL_STENCIL_BUFFER_BIT
         .|. GL_DEPTH_BUFFER_BIT

  flip evalStateT sdlstate $ do
    let kernel = do
          shouldRedraw .= False

          -- events
          events <- pollEvents
          F.for_ events handleInputEvent

          F.for_ events (handleRelativeMouse window)

          use (sDLState.newWindowSize) >>= \msz -> F.for_ msz $ \sz -> do
            windowSize   .= sz
            shouldRedraw .= True
            liftIO $ putStrLn $ "new size of " ++ show sz

          -- movement
          movement <- firstPersonMovement
          when (movement /= def) $ do
            updateCameraFP movement
            shouldRedraw .= True

          escPressed <- use (keycode SDLK_ESCAPE)
          qPressed   <- use (keycode SDLK_q)
          when (escPressed || qPressed) $ shouldClose .= True

          doRedraw <- use shouldRedraw
          sz <- use windowSize
          if doRedraw
            then draw window sz
            else liftIO $ threadDelay 16000

          unlessM (use shouldClose .||. SDL.quitRequested) kernel

    draw window (sdlstate^.windowSize)
    kernel

handleRelativeMouse
  :: (MonadIO m, HasSDLState s, MonadState s m)
  => SDL.Window -> SDL.Event -> m ()
handleRelativeMouse w = \case
  SDL.MouseButtonEvent SDL_MOUSEBUTTONDOWN _ _ _mouse button _ _ x y ->
    when (button == SDL_BUTTON_LEFT) $ do
       SDL.setRelativeMouseMode True >>= err
       mouseDownPos ?= V2 x y
  SDL.MouseButtonEvent SDL_MOUSEBUTTONUP _ _ _mouse button _ _ _ _ ->
    when (button == SDL_BUTTON_LEFT) $ do
       SDL.setRelativeMouseMode False >>= err
       startingMousePos <- mouseDownPos <<.= Nothing
       F.for_ startingMousePos $ \(V2 x y) ->
         SDL.warpMouseInWindow w (fromIntegral x) (fromIntegral y)

  _ -> return ()

(.||.) :: Monad m => m Bool -> m Bool -> m Bool
(.||.) ma mb = do
  a <- ma
  if a then return True
       else mb

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb action = do
  b <- mb
  unless b action

