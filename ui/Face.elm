module Face
    exposing
        ( Vertex
        , Face
        , makeFace
        , vertexShader
        , fragmentShader
        )

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import WebGL exposing (Drawable(..), Renderable, Shader)


type alias Vertex =
    { position : Vec3
    , color : Vec4
    }


type alias Face =
    List ( Vertex, Vertex, Vertex )


makeFace : ( ( Vec3, Vec3, Vec3, Vec3 ), Vec4 ) -> Face
makeFace ( ( p1, p2, p3, p4 ), color ) =
    [ ( Vertex p1 color
      , Vertex p2 color
      , Vertex p3 color
      )
    , ( Vertex p3 color
      , Vertex p2 color
      , Vertex p4 color
      )
    ]



-- Vertex shader for the Face.


vertexShader :
    Shader
        { attr
            | position : Vec3
            , color : Vec4
        }
        { unif
            | perspective : Mat4
            , modelView : Mat4
        }
        { vcolor : Vec4 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec4 color;
uniform mat4 perspective;
uniform mat4 modelView;
varying vec4 vcolor;

void main (void) {
    gl_Position = perspective * modelView * vec4(position, 1.0);
    vcolor = color;
}

  |]



-- Fragment shader for the Face.


fragmentShader : Shader {} u { vcolor : Vec4 }
fragmentShader =
    [glsl|

precision mediump float;

varying vec4 vcolor;

void main (void) {
    gl_FragColor = vcolor;
}

    |]
