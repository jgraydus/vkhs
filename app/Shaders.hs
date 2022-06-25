{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Shaders where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang

exampleVertexShader = $(compileShaderQ Nothing "vert" Nothing [glsl|
  #version 450

  layout(location = 0) in vec2 inPosition;
  layout(location = 1) in vec3 inColor;

  layout(location = 0) out vec3 fragColor;

  void main() {
      gl_Position = vec4(inPosition, 0.0, 1.0);
      fragColor = inColor;
  }
|])

exampleFragmentShader = $(compileShaderQ Nothing "frag" Nothing [glsl|
  #version 450

  layout(location = 0) in vec3 fragColor;

  layout(location = 0) out vec4 outColor;

  void main() {
      outColor = vec4(fragColor, 1.0);
  }
|])

