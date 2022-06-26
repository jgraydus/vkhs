{-# LANGUAGE FlexibleContexts #-}
module Buffer where

import Data.Bits (testBit, popCount, (.&.))
import Data.Vector ((!), Vector)
import Data.Word (Word32)
import qualified Data.Vector as V
import Foreign.Ptr (Ptr)
import GHC.Records
import Vulkan.Core10.Buffer (
  Buffer,
  BufferCreateInfo(..),
  BufferUsageFlags,
  createBuffer,
  destroyBuffer,
  SharingMode(..))
import Vulkan.Core10.Device (Device)
import Vulkan.Core10.DeviceInitialization (
  getPhysicalDeviceMemoryProperties,
  MemoryPropertyFlags,
  MemoryType(..),
  PhysicalDevice,
  PhysicalDeviceMemoryProperties(..))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Memory (
  allocateMemory,
  freeMemory,
  mapMemory,
  MemoryAllocateInfo(..),
  unmapMemory)
import Vulkan.Core10.MemoryManagement (
  bindBufferMemory,
  getBufferMemoryRequirements,
  MemoryRequirements(..))
import Vulkan.NamedType ((:::))
import Vulkan.Zero (zero)

findMemoryTypeIndex
  :: PhysicalDeviceMemoryProperties
  -> MemoryRequirements
  -> MemoryPropertyFlags
  -> Word32
findMemoryTypeIndex
  memoryProperties
  memoryRequirements
  memoryPropertyFlags = fromIntegral j
    where
      Just j = V.findIndex cond (V.indexed memoryTypes)
      memoryTypes = getField @"memoryTypes" memoryProperties
      cond (idx, memoryType) = cond1 idx && cond2 memoryType
      -- the ith memoryType is only available if the ith bit is set in memoryTypeBits
      cond1 idx = testBit (getField @"memoryTypeBits" memoryRequirements) idx
      -- ensure the required flags are set 
      cond2 memoryType = (getField @"propertyFlags" memoryType .&. memoryPropertyFlags) == memoryPropertyFlags

makeBuffer
  :: PhysicalDevice
  -> Device
  -> "bufferSize" ::: DeviceSize
  -> BufferUsageFlags
  -> MemoryPropertyFlags
  -> IO (Buffer, DeviceMemory, Ptr ())
makeBuffer physicalDevice device bufferSize bufferUsageFlags memoryPropertyFlags = do
  let bufferCreateInfo :: BufferCreateInfo '[] = zero {
    size = bufferSize,
    usage = bufferUsageFlags,
    sharingMode = SHARING_MODE_EXCLUSIVE
  }

  buffer <- createBuffer device bufferCreateInfo Nothing
  memoryRequirements <- getBufferMemoryRequirements device buffer
  memoryProperties <- getPhysicalDeviceMemoryProperties physicalDevice

  let allocInfo :: MemoryAllocateInfo '[] = zero {
    allocationSize = getField @"size" memoryRequirements,
    memoryTypeIndex = findMemoryTypeIndex memoryProperties memoryRequirements memoryPropertyFlags
  }

  bufferMemory <- allocateMemory device allocInfo Nothing
  bindBufferMemory device buffer bufferMemory 0
  ptr <- mapMemory device bufferMemory 0 (getField @"size" memoryRequirements) zero

  return (buffer, bufferMemory, ptr)

releaseBuffer :: Device -> Buffer -> DeviceMemory -> IO ()
releaseBuffer device buffer bufferMemory = do
  unmapMemory device bufferMemory
  freeMemory device bufferMemory Nothing
  destroyBuffer device buffer Nothing

