// A surface tessellation shader using the Walton & Meek technique [1].
//
// This is just a surface shader, so you can add any surface shader features you want by simply
// tweaking the `#pragma surface` line. See the Unity documentation [2] for more information.
//
// Copyright (c) 2023 Tachi. Licensed under either the Apache 2.0 license or MIT license, at your
// option.
//
// [1]: DJ Walton and DS Meek. "A triangular G1 patch from boundary curves." *Computer-Aided Design*
// 28, no. 2 (1996): 113-123.
//
// [2]: https://docs.unity3d.com/Manual/SL-SurfaceShaders.html

Shader "Tachi/Tess" {
    // *INDENT-OFF*
    Properties {
        _Color("Color", Color) = (1, 1, 1, 1)
        _MainTex("Albedo (RGB)", 2D) = "white" {}
        _Glossiness("Smoothness", Range(0, 1)) = 0.5
        _Metallic("Metallic", Range(0, 1)) = 0.0
        _EdgeLength("Tess. Edge Length", Range(2, 50)) = 15
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
// [2]: https://docs.unity3d.com/Manual/SL-SurfaceShaders.html
#pragma surface mainSurface Standard fullforwardshadows tessellate:mainTessControl vertex:fakeVert

// Use shader model 4.6 target for tessellation.
#pragma target 4.6

#include "Tessellation.cginc"

        sampler2D _MainTex;

        struct Input {
            float2 uv_MainTex;
        };

        float _EdgeLength;
        half _Glossiness;
        half _Metallic;
        fixed4 _Color;

        UNITY_INSTANCING_BUFFER_START(Props)
        // Per-instance properties go here.
        UNITY_INSTANCING_BUFFER_END(Props)

        // Walton & Meek tessellation

        // Interpolates 3-vectors with the given weights.
        float3 interpolateFloat3(float3 a, float3 b, float3 c, float3 lambda) {
            return a * lambda.x + b * lambda.y + c * lambda.z;
        }

        // Normalizes a 3-vector, with reasonable behavior with lengths close to zero.
        float3 safeNormalize(float3 v) {
            float scale = min(abs(v.x), min(abs(v.y), abs(v.z)));
            if (scale != 0.0)
                v /= scale;
            float len = length(v);
            if (len != 0.0)
                v /= len;
            return v;
        }

        // Projects vector a onto b, with reasonable behavior around lengths close to zero.
        float3 safeProject(float3 a, float3 b) {
            float3 c = dot(a, b);
            float sqLen = dot(b, b);
            return sqLen > 0.00001 ? c / sqLen : c;
        }

        // Swaps two 3-vectors.
        void swap3(inout float3 inoutA, inout float3 inoutB) {
            float3 tmp = inoutA;
            inoutA = inoutB;
            inoutB = tmp;
        }

        // Returns true if v0 is lexicographically less than v3.
        bool edgeWinding(float3 v0, float3 v3) {
            return (v0.x < v3.x || (v0.x == v3.x && v0.y < v3.y) ||
                    (v0.x == v3.x && v0.y == v3.y && v0.z < v3.z));
        }

        // Computes the cubic control points for the boundary curves.
        void computeEdgeControlPoints(float3 v0, float3 v3, float3 n0, float3 n3,
                                      out float3 outV1, out float3 outV2) {
            bool winding = edgeWinding(v0, v3);
            if (!winding) {
                swap3(v0, v3);
                swap3(n0, n3);
            }

            float d = length(v3 - v0);
            float3 gamma = safeNormalize(v3 - v0);
            float a = dot(n0, n3);
            float2 a01 = float2(dot(n0, gamma), dot(n3, gamma));
            float2 rhoSigma = 6.0 * (2.0 * a01.xy + a * a01.yx) / (4.0 - a * a);

            float3 v1 = v0 + d * (6.0 * gamma - 2.0 * rhoSigma.x * n0 + rhoSigma.y * n3) / 18.0;
            float3 v2 = v3 - d * (6.0 * gamma + rhoSigma.x * n0 - 2.0 * rhoSigma.y * n3) / 18.0;

            outV1 = winding ? v1 : v2;
            outV2 = winding ? v2 : v1;
        }

        // Evaluates a cubic Bézier curve.
        float3 evalEdgePoint(float3 v0, float3 v1, float3 v2, float3 v3, float t) {
            if (!edgeWinding(v0, v3)) {
                swap3(v0, v3);
                swap3(v1, v2);
                t = 1.0 - t;
            }

            float3 u1 = lerp(v1, v2, t);
            return lerp(lerp(lerp(v0, v1, t), u1, t), lerp(u1, lerp(v2, v3, t), t), t);
        }

        // Degree elevates a cubic Bézier curve to a quartic one.
        void degreeElevate(float3 p0, float3 p1, float3 p2, float3 p3,
                           out float3 outQP1, out float3 outQP2, out float3 outQP3) {
            outQP1 = lerp(p0, p1, 0.75);
            outQP2 = lerp(p1, p2, 0.50);
            outQP3 = lerp(p2, p3, 0.25);
        }

        // Computes the face control points for one edge of a Gregory patch.
        void computeFaceControlPoints(float3 v0, float3 v1, float3 v2, float3 v3,
                                      float3 l1, float3 l2, float3 l3, float3 lPrev,
                                      float3 lNext, float3 n0, float3 n1,
                                      out float3 outG1, out float3 outG2) {
            float3 w0 = v1 - v0, w1 = v2 - v1, w2 = v3 - v2;

            float3 d0 = lerp(lPrev - v0, lPrev - l1, 0.5); // in the plane of n0
            float3 d3 = lerp(lNext - v3, lNext - l3, 0.5); // in the plane of n1

            // All of these are normalized:
            float3 a0 = cross(n0, safeNormalize(w0)); // bitangent at v0
            float3 a2 = cross(n1, safeNormalize(w2)); // bitangent at v3
            float3 a1 = safeNormalize(a0 + a2); // average bitangent

            float lambda0 = safeProject(d0, w0);
            float lambda1 = safeProject(d3, w2);
            float mu0 = dot(d0, a0);
            float mu1 = dot(d3, a2);

            outG1 = lerp(l1, l2, 0.5) + lerp(lambda0 * w1, lambda1 * w0, 1.0 / 3.0) +
                    lerp(mu0 * a1, mu1 * a0, 1.0 / 3.0);
            outG2 = lerp(l2, l3, 0.5) + lerp(lambda0 * w2, lambda1 * w1, 2.0 / 3.0) +
                    lerp(mu0 * a2, mu1 * a1, 2.0 / 3.0);
        }

        // Generalized de Casteljau subdivision.
        float3 evalBezierTriangle4(float3 p400, float3 p310, float3 p301, float3 p220,
                                   float3 p211, float3 p202, float3 p130, float3 p121,
                                   float3 p112, float3 p103, float3 p040, float3 p031,
                                   float3 p022, float3 p013, float3 p004,
                                   float3 lambda) {
            float3 q030 = interpolateFloat3(p130, p040, p031, lambda);
            float3 q021 = interpolateFloat3(p121, p031, p022, lambda);
            float3 q120 = interpolateFloat3(p220, p130, p121, lambda);
            float3 q012 = interpolateFloat3(p112, p022, p013, lambda);
            float3 q111 = interpolateFloat3(p211, p121, p112, lambda);
            float3 q210 = interpolateFloat3(p310, p220, p211, lambda);
            float3 q003 = interpolateFloat3(p103, p013, p004, lambda);
            float3 q102 = interpolateFloat3(p202, p112, p103, lambda);
            float3 q201 = interpolateFloat3(p301, p211, p202, lambda);
            float3 q300 = interpolateFloat3(p400, p310, p301, lambda);

            float3 r020 = interpolateFloat3(q120, q030, q021, lambda);
            float3 r011 = interpolateFloat3(q111, q021, q012, lambda);
            float3 r110 = interpolateFloat3(q210, q120, q111, lambda);
            float3 r002 = interpolateFloat3(q102, q012, q003, lambda);
            float3 r101 = interpolateFloat3(q201, q111, q102, lambda);
            float3 r200 = interpolateFloat3(q300, q210, q201, lambda);

            float3 s010 = interpolateFloat3(r110, r020, r011, lambda);
            float3 s001 = interpolateFloat3(r101, r011, r002, lambda);
            float3 s100 = interpolateFloat3(r200, r110, r101, lambda);

            return interpolateFloat3(s100, s010, s001, lambda);
        }

        // Evaluates a quartic Gregory patch.
        float3 evalGregoryPatch(float3 p004, float3 p013, float3 p022, float3 p031,
                                float3 p040, float3 p130, float3 p220, float3 p310,
                                float3 p400, float3 p301, float3 p202, float3 p103,
                                float3 g01, float3 g02, float3 g11, float3 g12,
                                float3 g21, float3 g22, float3 tessCoord) {
            float u = tessCoord.x, v = tessCoord.y, w = tessCoord.z;
            float3 p112 = w > 0.99 ? g22 : (u * g22 + v * g01) / (u + v);
            float3 p121 = v > 0.99 ? g02 : (w * g02 + u * g11) / (w + u);
            float3 p211 = u > 0.99 ? g12 : (v * g12 + w * g21) / (v + w);

            return evalBezierTriangle4(p400, p310, p301, p220, p211, p202, p130, p121,
                                       p112, p103, p040, p031, p022, p013, p004,
                                       tessCoord);
        }

        // Evaluates tessellation using the Walton & Meek approach.
        void mainTessEval(inout appdata_full inoutVertex,
                          float3 p400,
                          float3 p040,
                          float3 p004,
                          float3 n400,
                          float3 n040,
                          float3 n004,
                          float3 tessCoord) {
            // These come in unnormalized, so normalize them.
            n400 = safeNormalize(n400);
            n040 = safeNormalize(n040);
            n004 = safeNormalize(n004);

            // Compute boundary curves.
            float3 p012_3, p021_3, p120_3, p210_3, p201_3, p102_3;
            computeEdgeControlPoints(p004, p040, n004, n040, p012_3, p021_3);
            computeEdgeControlPoints(p040, p400, n040, n400, p120_3, p210_3);
            computeEdgeControlPoints(p400, p004, n400, n004, p201_3, p102_3);

            // Degree elevate cubic to quartic.
            float3 p013, p022, p031, p130, p220, p310, p301, p202, p103;
            degreeElevate(p004, p012_3, p021_3, p040, p013, p022, p031);
            degreeElevate(p040, p120_3, p210_3, p400, p130, p220, p310);
            degreeElevate(p400, p201_3, p102_3, p004, p301, p202, p103);

            // Compute face points.
            float3 g01, g02, g11, g12, g21, g22;
            computeFaceControlPoints(p004, p012_3, p021_3, p040, p013, p022, p031,
                                     p103, p130, n004, n040, g01, g02);
            computeFaceControlPoints(p040, p120_3, p210_3, p400, p130, p220, p310,
                                     p031, p301, n040, n400, g11, g12);
            computeFaceControlPoints(p400, p201_3, p102_3, p004, p301, p202, p103,
                                     p310, p013, n400, n004, g21, g22);

            inoutVertex.vertex.w = 1.0;

            if (tessCoord.x == 0.0 || tessCoord.y == 0.0 || tessCoord.z == 0.0) {
                float3 v0, v1, v2, v3;
                float t;
                if (tessCoord.x == 0.0) {
                    v0 = p004;
                    v1 = p012_3;
                    v2 = p021_3;
                    v3 = p040;
                    t = tessCoord.y;
                } else if (tessCoord.y == 0.0) {
                    v0 = p400;
                    v1 = p201_3;
                    v2 = p102_3;
                    v3 = p004;
                    t = tessCoord.z;
                } else {
                    v0 = p040;
                    v1 = p120_3;
                    v2 = p210_3;
                    v3 = p400;
                    t = tessCoord.x;
                }
                inoutVertex.vertex.xyz = evalEdgePoint(v0, v1, v2, v3, t);
                return;
            }

            // Evaluate Gregory patch.
            inoutVertex.vertex.xyz = evalGregoryPatch(p004,
                                                      p013,
                                                      p022,
                                                      p031,
                                                      p040,
                                                      p130,
                                                      p220,
                                                      p310,
                                                      p400,
                                                      p301,
                                                      p202,
                                                      p103,
                                                      g01,
                                                      g02,
                                                      g11,
                                                      g12,
                                                      g21,
                                                      g22,
                                                      tessCoord);
        }

        // Other shader code follows.

        // Entry point for computation of tessellation factors.
        float4 mainTessControl(appdata_full v0, appdata_full v1, appdata_full v2) {
            return UnityEdgeLengthBasedTess(v0.vertex, v1.vertex, v2.vertex, _EdgeLength);
        }

        // This is unused. It's just here to shut up the compiler.
        void fakeVert(inout appdata_full unusedArg) {
            // Unused.
        }

        // The basic surface shader.
        void mainSurface(Input input, inout SurfaceOutputStandard inoutOutput) {
            // Albedo comes from a texture tinted by color.
            fixed4 albedo = tex2D(_MainTex, input.uv_MainTex) * _Color;
            inoutOutput.Albedo = albedo.rgb;
            // Metallic and smoothness come from slider variables.
            inoutOutput.Metallic = _Metallic;
            inoutOutput.Smoothness = _Glossiness;
            inoutOutput.Alpha = albedo.a;
        }

        // A preprocessor hack that allows us to capture individual untessellated vertices in our
        // domain shader, which Unity surface shaders don't ordinarily allow us to do.
#define fakeVert(v) \
    mainTessEval(v, \
                 vi[0].vertex.xyz, vi[1].vertex.xyz, vi[2].vertex.xyz, \
                 vi[0].normal, vi[1].normal, vi[2].normal, \
                 bary)

        ENDCG
    }

    FallBack "Diffuse"
}
