module Main where

import qualified Graphics.UI.GLFW as GLFW
import Vulkan.Core10.DeviceInitialization (Instance)

import Utils
import Vulkan
import Window

--
mainLoop :: GLFW.Window -> DrawFrame -> IO ()
mainLoop window drawFrame = untilM_ (GLFW.windowShouldClose window) $ do
  GLFW.pollEvents
  drawFrame

main :: IO ()
main =
  withWindow $ \window ->
    withVulkan window (mainLoop window)

