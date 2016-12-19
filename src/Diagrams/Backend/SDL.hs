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
import           Diagrams.Backend.GL
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

mkInitialState :: RenderInfo -> SDLState
mkInitialState i = SDLState
  { _stateCamera   = mm50Camera
      & cameraLens .~ (PerspectiveLens (3*pi/8 D.@@ D.rad) (4/3 D.@@ D.rad) 0.1 500)
  , _shouldClose   = False
  , _shouldRedraw  = True
  , _inFocus       = True
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
  renderDiaT _ _ = (return (), mempty)
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

instance Parseable (Options SDL) where
  parser =
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
  resultParser _ _ = parser
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
  let orbitspeed = 0.01

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
  let state0 = mkInitialState render

  let ls = _renderLines $ view renderScene render
  liftIO (print $ length ls)

  gl window state0

  glFinish
  SDL.glDeleteContext context
  SDL.quit

draw
  :: (MonadIO m, MonadState s m, HasCamera s, HasRenderInfo s)
  => SDL.Window -> m ()
draw window = do
  glClearColor 0.8 0.8 0.8 1
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

  cam <- use camera

  let viewMat          = cameraView cam
  let projectionMatrix = lensProjection cam
  let mats = CameraMatrices viewMat projectionMatrix

  render <- use renderInfo

  SDL.glSwapWindow window

gl :: SDL.Window -> SDLState -> IO ()
gl window sdlstate = do

  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LESS
  glEnable GL_CULL_FACE

  glClearColor 0 0 0 1
  glClear $  GL_COLOR_BUFFER_BIT
         .|. GL_STENCIL_BUFFER_BIT
         .|. GL_DEPTH_BUFFER_BIT

  flip evalStateT sdlstate $ do
    let kernel = do
          shouldRedraw .= False

          events <- pollEvents
          F.for_ events handleInputEvent
          movement <- firstPersonMovement
          when (movement /= def) $ do
            updateCameraFP movement
            shouldRedraw .= True

          use (mouseButtons . contains SDL_BUTTON_LEFT) >>= SDL.captureMouse >>= err

          escPressed <- use (keycode SDLK_ESCAPE)
          qPressed   <- use (keycode SDLK_q)
          when (escPressed || qPressed) $ shouldClose .= True

          doRedraw <- use shouldRedraw
          if doRedraw
            then draw window
            else liftIO $ threadDelay 16000

          unlessM (use shouldClose .||. SDL.quitRequested) kernel

    draw window
    kernel

(.||.) :: Monad m => m Bool -> m Bool -> m Bool
(.||.) ma mb = do
  a <- ma
  if a then return True
       else mb

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb action = do
  b <- mb
  unless b action

