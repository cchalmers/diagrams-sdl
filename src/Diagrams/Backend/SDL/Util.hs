{-# LANGUAGE ViewPatterns #-}
module Diagrams.Backend.SDL.Util where

import           Control.Exception
import           Control.Monad          (when)
import           Control.Monad.IO.Class
import           Data.Typeable
import           Foreign
import           Foreign.C
import           Linear

import qualified SDL.Raw                as SDL
import           SDL.Raw.Enum

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
-- attr :: GLattr -> StateVar Int
-- attr a = StateVar (getAttr a) (glSetAttr a)

-- boolAttr :: GLattr -> StateVar Bool
-- boolAttr = mapStateVar fromEnum toEnum . attr

getAttr :: GLattr -> IO Int
getAttr a = alloca $ \p -> do
 SDL.glGetAttribute a p >>= err
 fromIntegral <$> peek p

glSetAttr :: GLattr -> Int -> IO ()
glSetAttr a i = SDL.glSetAttribute a (fromIntegral i) >>= err

-- | The default diagrams-sdl SDL2 setup. Uses
--
--   - OpenGL 4.1
--   - Core profile mask
--   - rgb size 5
--   - depth size 16
--   - double buffering
--   - multisampling with 16 samples
defaultSdlSetup :: IO ()
defaultSdlSetup = do
  SDL.init (SDL_INIT_TIMER .|. SDL_INIT_VIDEO .|. SDL_INIT_EVENTS) >>= err
  glSetAttr SDL_GL_CONTEXT_MAJOR_VERSION 4
  glSetAttr SDL_GL_CONTEXT_MINOR_VERSION 1
  glSetAttr SDL_GL_CONTEXT_PROFILE_MASK SDL_GL_CONTEXT_PROFILE_CORE

  glSetAttr SDL_GL_RED_SIZE   5
  glSetAttr SDL_GL_GREEN_SIZE 5
  glSetAttr SDL_GL_BLUE_SIZE  5
  glSetAttr SDL_GL_DEPTH_SIZE 16
  glSetAttr SDL_GL_DOUBLEBUFFER 1

  glSetAttr SDL_GL_MULTISAMPLEBUFFERS 1
  glSetAttr SDL_GL_MULTISAMPLESAMPLES 16

-- | Create a new opengl sdl window given a title and window size which
--   is initially shown, centred, resizeable and allows high DPI.
--
--   To make a window with different options see 'SDL.createWindow' from
--   "SDL2.Raw".
defaultSdlWindow :: String -> V2 Int -> IO SDL.Window
defaultSdlWindow title (fmap (fromIntegral . abs) -> V2 x y) =
  withCString title $ \windowName ->
    SDL.createWindow
      windowName
      SDL_WINDOWPOS_CENTERED
      SDL_WINDOWPOS_CENTERED
      x y
      flags
  where
    flags =  SDL_WINDOW_OPENGL
         .|. SDL_WINDOW_SHOWN
         .|. SDL_WINDOW_RESIZABLE
         .|. SDL_WINDOW_ALLOW_HIGHDPI

-- | Create a context for a window, make it the current context and set
--   the swap interval to 1.
sdlGlContext :: SDL.Window -> IO SDL.GLContext
sdlGlContext window = do
  glContext <- SDL.glCreateContext window
  SDL.glMakeCurrent window glContext >>= err
  SDL.glSetSwapInterval 1 >>= err
  return glContext

