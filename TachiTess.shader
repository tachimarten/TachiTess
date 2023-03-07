// A surface tessellation shader using the Walton & Meek technique [1].
//
// This is just a surface shader, so you can add any surface shader features you want by simply
// tweaking the `#pragma surface` line. See the Unity documentation [2] for more information.
//
// Copyright (c) 2023 Tachi. Licensed under either the Apache 2.0 license or MIT license, at your
// option.
//
// [1]: D.J. Walton and D.S. Meek. "A triangular G1 patch from boundary curves." *Computer-Aided
// Design* 28, no. 2 (1996): 113-123.
//
// [2]: https://docs.unity3d.com/Manual/SL-SurfaceShaders.html

Shader "Tachi/Tess" {
    // *INDENT-OFF*
    Properties {
        _Color("Color", Color) = (1, 1, 1, 1)
        _MainTex("Albedo (RGB)", 2D) = "white" {}
        _NormalMap("Normal Map", 2D) = "bump" {}
        _Glossiness("Smoothness", Range(0, 1)) = 0.5
        _Metallic("Metallic", Range(0, 1)) = 0.0

        _TessFactor("Tess. Factor", Range(50, 100)) = 75
        _MaxTessLevel("Max Tess. Level", Range(1, 6)) = 6
    }
    // *INDENT-ON*

    SubShader {
        Tags {
            "RenderType" = "Opaque"
        }

        LOD 200

        CGPROGRAM

// Physically based Standard lighting model, and enable shadows on all light types.
//
// See the Unity documentation [2] for more things you can put here.
//
// You must include `tessellate:mainTessControl vertex:fakeVert` here for tessellation to work.
//
// [2]: https://docs.unity3d.com/Manual/SL-SurfaceShaders.html
#pragma surface mainSurface Standard fullforwardshadows tessellate:mainTessControl vertex:fakeVert

// Use shader model 4.6 target for tessellation.
#pragma target 4.6

        // This must go *before* `TachiTess.cginc` is included.
        void fakeVert(inout appdata_full unusedArg) {
            // Unused.
        }

#include "TachiTess.cginc"

        sampler2D _MainTex;
        sampler2D _NormalMap;

        struct Input {
            float2 uv_MainTex;
            float2 uv_NormalMap;
        };

        half _Glossiness;
        half _Metallic;
        fixed4 _Color;

        UNITY_INSTANCING_BUFFER_START(Props)
        // Per-instance properties go here.
        UNITY_INSTANCING_BUFFER_END(Props)

        // The basic surface shader.
        void mainSurface(Input input, inout SurfaceOutputStandard inoutOutput) {
            // Albedo comes from a texture tinted by color.
            fixed4 albedo = tex2D(_MainTex, input.uv_MainTex) * _Color;
            inoutOutput.Albedo = albedo.rgb;
            // Metallic and smoothness come from slider variables.
            inoutOutput.Metallic = _Metallic;
            inoutOutput.Smoothness = _Glossiness;
            inoutOutput.Alpha = albedo.a;
            inoutOutput.Normal = UnpackNormal(tex2D(_NormalMap, input.uv_NormalMap));
        }

        ENDCG
    }

    FallBack "Standard"
}
