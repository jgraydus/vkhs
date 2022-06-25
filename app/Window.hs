module Window where

import Control.Exception
import qualified Graphics.UI.GLFW as GLFW

errorCallback :: GLFW.Error -> String -> IO ()
errorCallback errorCode errorMessage = do
  putStrLn $ "ERROR: " <> errorMessage

windowWidth :: Int = 800
windowHeight :: Int = 800
windowTitle :: String = "Vulkan"

--
initWindow :: IO GLFW.Window
initWindow = do
  GLFW.setErrorCallback $ Just errorCallback

  result <- GLFW.init
  if result
  then putStrLn "GLFW initialization success"
  else putStrLn "GLFW initialization failure"

  v <- GLFW.vulkanSupported
  if v
  then putStrLn "GLFW vulkan supported"
  else putStrLn "GLFW vulkan NOT supported"

  GLFW.windowHint $ GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI
  GLFW.windowHint $ GLFW.WindowHint'Resizable True

  Just window <- GLFW.createWindow
                   windowWidth
                   windowHeight
                   windowTitle
                   Nothing -- unused (monitor for fullscreen mode)
                   Nothing -- unused (window for context object sharing)

  return window

--
cleanupWindow :: GLFW.Window -> IO ()
cleanupWindow window = do
  GLFW.destroyWindow window
  GLFW.terminate
  putStrLn "GLFW terminated"

withWindow :: (GLFW.Window -> IO a) -> IO a
withWindow = bracket initWindow cleanupWindow

