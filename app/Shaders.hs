{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Shaders where

import Vulkan.Utils.ShaderQQ.GLSL.Glslang

exampleVertexShader = $(compileShaderQ Nothing "vert" Nothing [glsl|
  #version 450

  vec2 positions[3] = vec2[](
      vec2(0.0, -0.5),
      vec2(0.5, 0.5),
      vec2(-0.5, 0.5)
  );

  vec3 colors[3] = vec3[](
      vec3(1.0, 0.0, 0.0),
      vec3(0.0, 1.0, 0.0),
      vec3(0.0, 0.0, 1.0)
  );

  layout(location = 0) out vec3 fragColor;

  void main() {
      gl_Position = vec4(positions[gl_VertexIndex], 0.0, 1.0);
      fragColor = colors[gl_VertexIndex];
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

