module VulkanInstance where

import Control.Exception (bracket)
import Data.ByteString (ByteString, packCString, isInfixOf)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Graphics.UI.GLFW as GLFW
import Vulkan.Core10.DeviceInitialization
import Vulkan.Core10.LayerDiscovery
import Vulkan.Version
import Vulkan.Zero

getValidationLayers :: IO (Vector ByteString)
getValidationLayers = do
  (_, layerProperties) <- enumerateInstanceLayerProperties
  let f layer = "validation" `isInfixOf` (layerName layer)
  let validationLayers = V.filter f layerProperties
  print validationLayers
  return $ layerName <$> validationLayers
  


getRequiredExtensions :: IO (Vector ByteString)
getRequiredExtensions = do
  tmp <- GLFW.getRequiredInstanceExtensions
  V.fromList <$> traverse packCString tmp

makeInstanceCreateInfo :: IO (InstanceCreateInfo '[])
makeInstanceCreateInfo = do
  requiredExtensions <- getRequiredExtensions
  let appInfo :: ApplicationInfo = zero { 
        applicationName = Just $ encodeUtf8 ("Learning Vulkan" :: Text),
        applicationVersion = MAKE_API_VERSION 1 0 0,
        engineName = Just $ encodeUtf8 ("No Engine" :: Text),
        engineVersion = MAKE_API_VERSION 1 0 0,
        apiVersion = MAKE_API_VERSION 1 0 0
      }
  validationLayers <- getValidationLayers
  let info :: InstanceCreateInfo '[] = zero {
        applicationInfo = Just appInfo,
        enabledLayerNames = validationLayers,
        enabledExtensionNames = requiredExtensions 
      }
  return info

withVulkanInstance :: (Instance -> IO a) -> IO a
withVulkanInstance f = do
  info <- makeInstanceCreateInfo
  withInstance info Nothing $ \create' cleanup' -> do
    let create = putStrLn "Vulkan: created instance" >> create'
    let cleanup i = putStrLn "Vulkan: destroyed instance" >> cleanup' i
    bracket create cleanup f

