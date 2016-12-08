module Brick exposing (Brick, vertexShader, fragmentShader)

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Transform exposing (pitch)
import WebGL exposing (..)


type alias Brick =
    { mesh : Drawable Vertex
    , coord : Vec3
    , pitch : Float
    , modelView : Mat4
    }


type alias Vertex =
    { position : Vec3
    , color : Vec4
    }



-- Vertex shader for the Brick.


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



-- Fragment shader for the Brick.


fragmentShader : Shader {} u { vcolor : Vec4 }
fragmentShader =
    [glsl|

precision mediump float;

varying vec4 vcolor;

void main (void) {
    gl_FragColor = vcolor;
}

    |]
