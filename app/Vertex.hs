{-# OPTIONS_GHC -DGSTORABLE_SUMTYPES #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Vertex (
  indices,
  indexBufferSize,
  Vertex,
  vertexBufferSize,
  vertexInputInfo,
  vertices
)where

import Data.Data
import Data.Map.Strict ((!), Map)
import qualified Data.Map.Strict as M
import qualified Data.StorableVector as SV
import qualified Data.Vector as V
import Data.Vector.Fixed (mk2, mk4)
import Data.Vector.Fixed.Storable
import Data.Word (Word16, Word64)
import Foreign.Storable.Generic
import Foreign.Storable.Generic.Internal
import Foreign.Storable.Generic.Tools (calcOffsets)
import GHC.Generics
import Vulkan.Core10.Buffer
import Vulkan.Core10.Enums.Format
import Vulkan.Core10.Pipeline
import Vulkan.Zero

data Vertex = Vertex
  { position :: {-# UNPACK #-} !(Vec2 Float)
  , color    :: {-# UNPACK #-} !(Vec4 Float)
  } deriving (Data, Generic, Show, Typeable)

instance GStorable Vertex

fieldOffsets :: Map String Int
fieldOffsets = M.fromList $ zip names offsets
  where
    -- collect the field names
    names = head . map constrFields . dataTypeConstrs . dataTypeOf $ (undefined :: Vertex)
    -- calculate the offset of each field
    offsets = calcOffsets $ zip (glistSizeOf' rep) (glistAlignment' rep)
    rep = from (undefined :: Vertex)

vertices :: SV.Vector Vertex = SV.pack
  [ Vertex { position = mk2 (-0.5) (-0.5), color = mk4 1.0 0.0 0.0 0.0 }
  , Vertex { position = mk2   0.5  (-0.5), color = mk4 0.0 1.0 0.0 0.0 }
  , Vertex { position = mk2   0.5    0.5,  color = mk4 0.0 0.0 1.0 0.0 }
  , Vertex { position = mk2 (-0.5)   0.5,  color = mk4 1.0 1.0 1.0 0.0 } 
  ]

v = Vertex { position = mk2   0.5   0.5,  color = mk4 0.0 1.0 0.0 0.0 }

bindingDesc :: VertexInputBindingDescription = zero {
  binding = 0,
  stride = fromIntegral $ sizeOf (undefined :: Vertex),
  inputRate = VERTEX_INPUT_RATE_VERTEX
}

posDesc :: VertexInputAttributeDescription = zero {
  binding = 0,
  location = 0,
  format = FORMAT_R32G32_SFLOAT,
  offset = fromIntegral $ fieldOffsets ! "position"
}

colorDesc :: VertexInputAttributeDescription = zero {
  binding = 0,
  location = 1,
  format = FORMAT_R32G32B32A32_SFLOAT,
  offset = fromIntegral $ fieldOffsets ! "color"
}

vertexInputInfo :: PipelineVertexInputStateCreateInfo '[] = zero {
  vertexBindingDescriptions = V.fromList [bindingDesc],
  vertexAttributeDescriptions = V.fromList [posDesc, colorDesc]
}

vertexBufferSize :: Word64 = fromIntegral $ sizeOf (undefined :: Vertex) * SV.length vertices

indices :: SV.Vector Word16 = SV.pack [ 0, 1, 2, 2, 3, 0 ]

indexBufferSize :: Word64 = fromIntegral $ sizeOf (undefined :: Word16) * SV.length indices

