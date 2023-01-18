module Vulkan where

import Buffer
import Control.Monad
import Control.Exception
import Data.Bits
import Data.ByteString
import Data.IORef
import qualified Data.StorableVector as SV
import Data.Text (Text)
import Data.Text.Encoding
import Data.Word
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr, nullPtr, Ptr)
import Foreign.Storable (peek,poke)
import GHC.Records
import qualified Graphics.UI.GLFW as GLFW
import Shaders
import Vertex
import Vulkan.Core10.APIConstants
import Vulkan.Core10.Buffer
import Vulkan.Core10.CommandBuffer
import Vulkan.Core10.CommandBufferBuilding
import Vulkan.Core10.CommandPool
import Vulkan.Core10.Device
import Vulkan.Core10.DeviceInitialization
import Vulkan.Core10.Fence
import Vulkan.Core10.Enums.Format
import Vulkan.Core10.Enums.ImageAspectFlagBits
import Vulkan.Core10.Enums.Result
import Vulkan.Core10.Enums.SharingMode
import Vulkan.Core10.FundamentalTypes
import Vulkan.Core10.Image
import Vulkan.Core10.ImageView
import Vulkan.Core10.Memory
import Vulkan.Core10.MemoryManagement
import Vulkan.Core10.Pass
import Vulkan.Core10.Shader
import Vulkan.Core10.Pipeline
import Vulkan.Core10.PipelineLayout
import Vulkan.Core10.Queue
import Vulkan.Core10.QueueSemaphore
import Vulkan.CStruct.Extends
import Vulkan.Exception
import Vulkan.Extensions.VK_KHR_surface
import Vulkan.Extensions.VK_KHR_swapchain
import Vulkan.NamedType
import Vulkan.Version
import Vulkan.Zero
import VulkanInstance

getPhysicalDevice :: Instance -> IO PhysicalDevice
getPhysicalDevice vkInstance = do
  -- TODO ensure there is a device and it meets the requirements
  (_, physicalDevices) <- enumeratePhysicalDevices vkInstance
  return $ physicalDevices ! 0

supportsGraphics :: QueueFamilyProperties -> Bool
supportsGraphics qfp = popCount (queueFlags qfp .&. QUEUE_GRAPHICS_BIT) == 1

getQueueFamilyIndex :: PhysicalDevice -> IO Word32
getQueueFamilyIndex physicalDevice = do
  queueFamilyProperties <- getPhysicalDeviceQueueFamilyProperties physicalDevice
  let Just i = V.findIndex supportsGraphics queueFamilyProperties
  return (fromIntegral $ toInteger i)

makeDeviceCreateInfo :: PhysicalDevice -> IO (DeviceCreateInfo '[], Word32)
makeDeviceCreateInfo physicalDevice = do 
  i <- getQueueFamilyIndex physicalDevice
  let deviceQueueCreateInfo :: DeviceQueueCreateInfo '[] = zero {
    queueFamilyIndex = i,
    queuePriorities = V.fromList [1.0]
  }
  let info :: DeviceCreateInfo '[] = zero {
    queueCreateInfos = V.fromList [SomeStruct deviceQueueCreateInfo],
    enabledExtensionNames = V.fromList [KHR_SWAPCHAIN_EXTENSION_NAME]
    -- TODO enabledFeatures
  }
  return (info, i)

withSurfaceKHR :: GLFW.Window -> Instance -> (SurfaceKHR -> IO a) -> IO a
withSurfaceKHR window vulkan f = bracket create cleanup f
  where
    create = do 
      alloca $ \(ptr :: Ptr Word64) -> do
        _ :: Int <- GLFW.createWindowSurface
                      (castPtr $ instanceHandle vulkan)
                      window
                      nullPtr
                      ptr
        SurfaceKHR <$> peek ptr
    cleanup surface = destroySurfaceKHR vulkan surface Nothing

makeSwapChainCreateInfoKHR :: PhysicalDevice -> SurfaceKHR -> IO (SwapchainCreateInfoKHR '[])
makeSwapChainCreateInfoKHR physicalDevice surface = do
  SurfaceCapabilitiesKHR {..} <- getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
  return $ SwapchainCreateInfoKHR {
    next = (),
    flags = SwapchainCreateFlagBitsKHR 0,
    surface = surface,
    minImageCount = minImageCount,
    -- TODO use getPhysicalDeviceSurfaceFormatsKHR to get available formats. imageFormat/colorSpace
    -- MUST match an available format
    imageFormat = FORMAT_B8G8R8A8_SRGB,
    imageColorSpace = COLOR_SPACE_SRGB_NONLINEAR_KHR,
    imageExtent = currentExtent,
    imageArrayLayers = 1,
    imageUsage = IMAGE_USAGE_COLOR_ATTACHMENT_BIT,
    imageSharingMode = SHARING_MODE_EXCLUSIVE,
    queueFamilyIndices = V.empty,
    preTransform = currentTransform,
    compositeAlpha = COMPOSITE_ALPHA_OPAQUE_BIT_KHR,
    -- TODO use getPhysicalDeviceSurfacePresentModesKHR to ensure this presentMode is available
    presentMode = PRESENT_MODE_FIFO_KHR,
    clipped = True,
    oldSwapchain = NULL_HANDLE
  }

withSwapchain :: PhysicalDevice -> Device -> SurfaceKHR -> (SwapchainKHR -> IO a) -> IO a
withSwapchain physicalDevice device surface f = do
  swapchainInfo <- makeSwapChainCreateInfoKHR physicalDevice surface
  withSwapchainKHR device swapchainInfo Nothing $ \create cleanup -> do
    bracket create cleanup f 

makeImageViewCreateInfo :: Image -> IO (ImageViewCreateInfo '[])
makeImageViewCreateInfo image = do
  let components = ComponentMapping {
    r = COMPONENT_SWIZZLE_IDENTITY,
    g = COMPONENT_SWIZZLE_IDENTITY,
    b = COMPONENT_SWIZZLE_IDENTITY,
    a = COMPONENT_SWIZZLE_IDENTITY
  }
  let subresourceRange = ImageSubresourceRange {
    aspectMask = IMAGE_ASPECT_COLOR_BIT,
    baseMipLevel = 0,
    levelCount = 1,
    baseArrayLayer = 0,
    layerCount = 1
  } 
  return $ zero {
    image = image,
    viewType = IMAGE_VIEW_TYPE_2D,
    format = FORMAT_B8G8R8A8_SRGB,
    components = components,
    subresourceRange = subresourceRange
  }

rasterizer :: PipelineRasterizationStateCreateInfo '[] = zero {
  depthClampEnable = False,
  rasterizerDiscardEnable = False,
  polygonMode = POLYGON_MODE_FILL,
  lineWidth = 1.0,
  cullMode = CULL_MODE_BACK_BIT,
  frontFace = FRONT_FACE_CLOCKWISE,
  depthBiasEnable = False,
  depthBiasConstantFactor = 0.0,
  depthBiasClamp = 0.0,
  depthBiasSlopeFactor = 0.0
}

inputAssembly :: PipelineInputAssemblyStateCreateInfo = zero {
  topology = PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
  primitiveRestartEnable = False
}

multisampling :: PipelineMultisampleStateCreateInfo '[] = zero {
  sampleShadingEnable = False,
  rasterizationSamples = SAMPLE_COUNT_1_BIT,
  minSampleShading = 1.0,
  sampleMask = V.empty,
  alphaToCoverageEnable = False,
  alphaToOneEnable = False
}

depthStencil :: PipelineDepthStencilStateCreateInfo = zero

colorBlendAttachment :: PipelineColorBlendAttachmentState = zero {
  colorWriteMask = COLOR_COMPONENT_R_BIT .|. COLOR_COMPONENT_G_BIT .|. COLOR_COMPONENT_B_BIT .|. COLOR_COMPONENT_A_BIT,
  blendEnable = False,
  srcColorBlendFactor = BLEND_FACTOR_ONE,
  dstColorBlendFactor = BLEND_FACTOR_ZERO,
  colorBlendOp = BLEND_OP_ADD,
  srcAlphaBlendFactor = BLEND_FACTOR_ONE,
  dstAlphaBlendFactor = BLEND_FACTOR_ZERO,
  alphaBlendOp = BLEND_OP_ADD
}

colorBlending :: PipelineColorBlendStateCreateInfo '[] = zero {
  logicOpEnable = False,
  logicOp = LOGIC_OP_COPY,
  attachments = V.fromList [colorBlendAttachment],
  blendConstants = (0.0, 0.0, 0.0, 0.0)
}

dynamicState' :: PipelineDynamicStateCreateInfo = zero {
  dynamicStates = V.fromList [ DYNAMIC_STATE_VIEWPORT, DYNAMIC_STATE_LINE_WIDTH ]
}

colorAttachment' :: AttachmentDescription = zero {
  format = FORMAT_B8G8R8A8_SRGB,
  samples = SAMPLE_COUNT_1_BIT,
  loadOp = ATTACHMENT_LOAD_OP_CLEAR,
  storeOp = ATTACHMENT_STORE_OP_STORE,
  stencilLoadOp = ATTACHMENT_LOAD_OP_DONT_CARE,
  stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE,
  initialLayout = IMAGE_LAYOUT_UNDEFINED,
  finalLayout = IMAGE_LAYOUT_PRESENT_SRC_KHR 
}

colorAttachmentRef :: AttachmentReference = zero {
  attachment = 0,
  layout = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
}

subpass' :: SubpassDescription = zero {
  pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS,
  colorAttachments = V.fromList [colorAttachmentRef]
}

subpassDependency :: SubpassDependency = zero {
  srcSubpass = SUBPASS_EXTERNAL,
  dstSubpass = 0,
  srcStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
  srcAccessMask = ACCESS_NONE,
  dstStageMask = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT,
  dstAccessMask = ACCESS_COLOR_ATTACHMENT_WRITE_BIT
}

renderPassCreateInfo :: RenderPassCreateInfo '[] = zero {
  attachments = V.fromList [colorAttachment'],
  subpasses = V.fromList [subpass'],
  dependencies = V.fromList [subpassDependency]
}

data DrawDeps = DrawDeps {
    swapchain  :: SwapchainKHR
  , imageViews :: Vector ImageView
  , framebuffers :: Vector Framebuffer
  , graphicsPipeline :: Pipeline
  , err :: IORef Result
  , drawFrame :: IO ()
}

createDrawDeps
  :: PhysicalDevice
  -> Device
  -> SurfaceKHR
  -> RenderPass
  -> Vector (SomeStruct PipelineShaderStageCreateInfo)
  -> PipelineLayout
  -> CommandBuffer
  -> Fence
  -> "imageAvailable" ::: Semaphore
  -> "renderFinished" ::: Semaphore
  -> Queue
  -> SubmitInfo '[]
  -> "vertexBuffer" ::: Buffer
  -> "indexBuffer"  ::: Buffer
  -> IO DrawDeps
createDrawDeps
  physicalDevice
  device
  surface
  renderPass
  shaderStages
  pipelineLayout
  commandBuffer
  inFlightFence
  imageAvailableSemaphore
  renderFinishedSemaphore
  queue
  submitInfo
  vertexBuffer
  indexBuffer = do

  SurfaceCapabilitiesKHR {..} <- getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
  let Extent2D {..} = currentExtent

  swapchainInfo <- makeSwapChainCreateInfoKHR physicalDevice surface
  swapchain <- createSwapchainKHR device swapchainInfo Nothing

  (_, swapchainImages) <- getSwapchainImagesKHR device swapchain

  imageViews <- V.forM swapchainImages $ \image -> do
    info <- makeImageViewCreateInfo image
    createImageView device info Nothing

  framebuffers <- V.iforM swapchainImages $ \i image -> do
    let info :: FramebufferCreateInfo '[] = zero {
      renderPass = renderPass,
      attachments = V.fromList [imageViews ! i],
      width = fromIntegral width,
      height = fromIntegral height,
      layers = 1
    }
    createFramebuffer device info Nothing

  let viewport :: Viewport = zero {
    x = 0.0,
    y = 0.0,
    width = fromIntegral width,
    height = fromIntegral height,
    minDepth = 0.0,
    maxDepth = 1.0
  }
  let scissor :: Rect2D = zero { offset = zero, extent = currentExtent }
  let viewportState :: PipelineViewportStateCreateInfo '[] = zero {
    viewports = V.fromList [viewport],
    scissors = V.fromList [scissor]
  }

  let pipelineInfo :: GraphicsPipelineCreateInfo '[] = zero {
    stages = shaderStages,
    vertexInputState = Just (SomeStruct vertexInputInfo),
    inputAssemblyState = Just inputAssembly,
    viewportState = Just (SomeStruct viewportState),
    rasterizationState = Just (SomeStruct rasterizer),
    multisampleState = Just (SomeStruct multisampling),
    depthStencilState = Just depthStencil,
    colorBlendState = Just (SomeStruct colorBlending),
    dynamicState = Just dynamicState',
    layout = pipelineLayout,
    renderPass = renderPass,
    subpass = 0
  }

  (_, pipelines) <- createGraphicsPipelines
                        device
                        NULL_HANDLE
                        (V.fromList [SomeStruct pipelineInfo])
                        Nothing

  let graphicsPipeline = pipelines ! 0
 
  let recordCommandBuffer imageIndex = do
        useCommandBuffer commandBuffer zero $ do
          cmdSetViewport commandBuffer 0 (V.fromList [viewport])
          let renderPassBeginInfo = RenderPassBeginInfo {
            next = (),
            renderPass = renderPass,
            framebuffer = framebuffers ! imageIndex,
            renderArea = zero { offset = zero, extent = currentExtent },
            clearValues = V.fromList [zero]
          }
          cmdUseRenderPass commandBuffer renderPassBeginInfo SUBPASS_CONTENTS_INLINE $ do
            cmdBindPipeline commandBuffer PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline
            cmdBindVertexBuffers commandBuffer 0 (V.fromList [vertexBuffer]) (V.fromList [0])
            cmdBindIndexBuffer commandBuffer indexBuffer 0 INDEX_TYPE_UINT16
            cmdDrawIndexed commandBuffer 6 1 0 0 0

  err <- newIORef SUCCESS

  let fences = V.fromList [inFlightFence]

  let drawFrame' :: IO () = do
       waitForFences device fences True (maxBound :: Word64)
       resetFences device fences
       (_, imageIndex) <- acquireNextImageKHR device swapchain (maxBound :: Word64) imageAvailableSemaphore NULL_HANDLE
       resetCommandBuffer commandBuffer COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
       recordCommandBuffer (fromIntegral imageIndex)
       queueSubmit queue (V.fromList [SomeStruct submitInfo]) inFlightFence
       let presentInfo = zero {
         waitSemaphores = V.fromList [renderFinishedSemaphore],
         swapchains = V.fromList [swapchain],
         imageIndices = V.fromList [imageIndex]
       }
       queuePresentKHR queue presentInfo
       return ()

  let drawFrame = drawFrame' `catch` \(e :: VulkanException) -> writeIORef err (vulkanExceptionResult e)

  return $ DrawDeps {..}

destroyDrawDeps :: Device -> DrawDeps -> IO ()
destroyDrawDeps device DrawDeps {..} = do
  destroyPipeline device graphicsPipeline Nothing
  V.forM_ framebuffers $ \framebuffer -> destroyFramebuffer device framebuffer Nothing
  V.forM_ imageViews $ \imageView -> destroyImageView device imageView Nothing
  destroySwapchainKHR device swapchain Nothing
  return ()

copyBuffer
  :: Device
  -> Queue
  -> CommandPool
  -> "srcBuffer" ::: Buffer
  -> "dstBuffer" ::: Buffer
  -> "size" ::: DeviceSize
  -> IO ()
copyBuffer device queue commandPool srcBuffer dstBuffer size = do
  cbs <- allocateCommandBuffers device $ zero {
    level = COMMAND_BUFFER_LEVEL_PRIMARY,
    commandPool = commandPool,
    commandBufferCount = 1
  }
  let copyRegion :: BufferCopy = zero {
    srcOffset = 0,
    dstOffset = 0,
    size = size
  }
  let cbbi = CommandBufferBeginInfo {
    next = (),
    flags = COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
    inheritanceInfo = Nothing
  }
  useCommandBuffer (cbs ! 0) cbbi $ do
    cmdCopyBuffer (cbs ! 0) srcBuffer dstBuffer (V.fromList [copyRegion])
  let submitInfo :: SubmitInfo '[] = zero { commandBuffers = commandBufferHandle <$> cbs }
  queueSubmit queue (V.fromList [ SomeStruct submitInfo ]) NULL_HANDLE
  queueWaitIdle queue
  freeCommandBuffers device commandPool cbs
  return ()
  

type DrawFrame = IO ()

withVulkan :: GLFW.Window -> (DrawFrame -> IO a) -> IO a
withVulkan window f = withVulkanInstance $ \vkInstance -> do
  physicalDevice <- getPhysicalDevice vkInstance
  withSurfaceKHR window vkInstance $ \surface -> do
    (info, i) <- makeDeviceCreateInfo physicalDevice
    surfaceSupported <- getPhysicalDeviceSurfaceSupportKHR physicalDevice i surface
    when (not surfaceSupported) (error "surface not supported")
    withDevice physicalDevice info Nothing $ \create' cleanup' -> do
      let create = putStrLn "Vulkan: created device" >> create'
      let cleanup i = putStrLn "Vulkan: destroyed device" >> cleanup' i
      bracket create cleanup $ \device -> do
        queue <- getDeviceQueue device i 0

        -- shaders
        vertexShaderModule <- createShaderModule device (zero { code = exampleVertexShader }) Nothing
        fragmentShaderModule <- createShaderModule device (zero { code = exampleFragmentShader }) Nothing

        let shaderStages :: Vector (SomeStruct PipelineShaderStageCreateInfo) = 
              V.fromList [
                SomeStruct $ zero {
                  stage = SHADER_STAGE_VERTEX_BIT,
                  module' = vertexShaderModule,
                  name = "main"
                },
                SomeStruct $ zero {
                  stage = SHADER_STAGE_FRAGMENT_BIT,
                  module' = fragmentShaderModule,
                  name = "main"
                }
              ]

        -- command pool
        let commandPoolCreateInfo = CommandPoolCreateInfo {
          flags = COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT,
          queueFamilyIndex = i
        }

        commandPool <- createCommandPool
                          device
                          commandPoolCreateInfo
                          Nothing

        -- vertex buffer
        (vertexBuffer, vertexBufferMemory) <- do
          (stagingBuffer, stagingBufferMemory) <- makeBuffer
             physicalDevice
             device
             vertexBufferSize
             BUFFER_USAGE_TRANSFER_SRC_BIT
             (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
  
          (vertexBuffer, vertexBufferMemory) <- makeBuffer
             physicalDevice
             device
             vertexBufferSize
             (BUFFER_USAGE_TRANSFER_DST_BIT .|. BUFFER_USAGE_VERTEX_BUFFER_BIT)
             MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  
          ptr <- mapMemory device stagingBufferMemory 0 vertexBufferSize zero
          SV.poke (castPtr ptr :: Ptr Vertex) vertices
          unmapMemory device stagingBufferMemory
  
          copyBuffer
            device
            queue
            commandPool
            stagingBuffer
            vertexBuffer
            vertexBufferSize

          releaseBuffer device stagingBuffer stagingBufferMemory
          return (vertexBuffer, vertexBufferMemory)

        -- index buffer
        (indexBuffer, indexBufferMemory) <- do
          (stagingBuffer, stagingBufferMemory) <- makeBuffer
             physicalDevice
             device
             indexBufferSize
             BUFFER_USAGE_TRANSFER_SRC_BIT
             (MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. MEMORY_PROPERTY_HOST_COHERENT_BIT)
  
          (indexBuffer, indexBufferMemory) <- makeBuffer
             physicalDevice
             device
             indexBufferSize
             (BUFFER_USAGE_TRANSFER_DST_BIT .|. BUFFER_USAGE_INDEX_BUFFER_BIT)
             MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  
          ptr <- mapMemory device stagingBufferMemory 0 indexBufferSize zero
          SV.poke (castPtr ptr :: Ptr Word16) indices
          unmapMemory device stagingBufferMemory
  
          copyBuffer
            device
            queue
            commandPool
            stagingBuffer
            indexBuffer
            indexBufferSize

          releaseBuffer device stagingBuffer stagingBufferMemory
          return (indexBuffer, indexBufferMemory)
  
        -- 
        pipelineLayout <- createPipelineLayout device zero Nothing
        renderPass <- createRenderPass device renderPassCreateInfo Nothing

        let allocInfo :: CommandBufferAllocateInfo = zero {
          commandPool = commandPool,
          level = COMMAND_BUFFER_LEVEL_PRIMARY,
          commandBufferCount = 1
        }

        commandBuffers <- allocateCommandBuffers device allocInfo

        let commandBuffer = commandBuffers ! 0

        imageAvailableSemaphore <- createSemaphore device zero Nothing
        renderFinishedSemaphore <- createSemaphore device zero Nothing
        inFlightFence <- createFence device (FenceCreateInfo { next = (), flags = FENCE_CREATE_SIGNALED_BIT }) Nothing

        let submitInfo = zero {
          waitSemaphores = V.fromList [imageAvailableSemaphore],
          waitDstStageMask = V.fromList [PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT],
          commandBuffers = V.fromList [commandBufferHandle commandBuffer],
          signalSemaphores = V.fromList [renderFinishedSemaphore]
        }

        let makeDrawDeps = createDrawDeps 
                             physicalDevice
                             device
                             surface
                             renderPass
                             shaderStages
                             pipelineLayout
                             commandBuffer
                             inFlightFence
                             imageAvailableSemaphore
                             renderFinishedSemaphore
                             queue
                             submitInfo
                             vertexBuffer
                             indexBuffer

        drawDepsRef <- makeDrawDeps >>= newIORef

        let draw :: IO () = do
             d@DrawDeps {..} <- readIORef drawDepsRef
             e <- readIORef err
             if e == SUCCESS
             then do
               drawFrame
             else do
               deviceWaitIdle device
               destroyDrawDeps device d
               drawDeps <- makeDrawDeps
               writeIORef drawDepsRef drawDeps
               writeIORef err SUCCESS

        result <- f draw

        -- cleanup
        deviceWaitIdle device
        drawDeps <- readIORef drawDepsRef
        destroyDrawDeps device drawDeps
        destroyFence device inFlightFence Nothing
        destroySemaphore device imageAvailableSemaphore Nothing
        destroySemaphore device renderFinishedSemaphore Nothing
        freeCommandBuffers device commandPool commandBuffers 
        destroyCommandPool device commandPool Nothing
        destroyRenderPass device renderPass Nothing
        destroyPipelineLayout device pipelineLayout Nothing
        releaseBuffer device vertexBuffer vertexBufferMemory
        releaseBuffer device indexBuffer indexBufferMemory
        destroyShaderModule device fragmentShaderModule Nothing
        destroyShaderModule device vertexShaderModule Nothing

        return result
  
