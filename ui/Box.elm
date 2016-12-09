module Box
    exposing
        ( Box
        , makeBox
        , yaw
        , render
        )

import List exposing (concatMap)
import Math.Matrix4 exposing (Mat4, identity, mul)
import Math.Vector3 exposing (Vec3, vec3, getX, getY, getZ)
import Math.Vector4 exposing (Vec4, vec4)
import Transform as T
import WebGL exposing (Drawable(..), Renderable, Shader)


type alias Box =
    { mesh : Drawable Vertex
    , coord : Vec3
    , pitch : Float
    , yaw : Float
    , modelView : Mat4
    }


type alias Vertex =
    { position : Vec3
    , color : Vec4
    }


type alias Face =
    List ( Vertex, Vertex, Vertex )


makeBox : Vec3 -> Box
makeBox coord =
    let
        box =
            { mesh = mesh
            , coord = coord
            , pitch = 0
            , yaw = 0
            , modelView = Math.Matrix4.identity
            }

        modelView =
            makeModelView box
    in
        { box | modelView = modelView }


yaw : Float -> Box -> Box
yaw theta box =
    let
        newBox =
            { box | yaw = theta }

        modelView =
            makeModelView newBox
    in
        { newBox | modelView = modelView }


render : Mat4 -> Box -> Renderable
render perspective box =
    WebGL.render vertexShader fragmentShader box.mesh { perspective = perspective, modelView = box.modelView }


mesh : Drawable Vertex
mesh =
    Triangle <|
        concatMap makeFace
            [ -- Front
              ( ( vec3 -1 1 1, vec3 1 1 1, vec3 -1 -1 1, vec3 1 -1 1 ), vec4 1 0 0 1 )
              -- Left
            , ( ( vec3 -1 1 -1, vec3 -1 1 1, vec3 -1 -1 -1, vec3 -1 -1 1 ), vec4 0 1 0 1 )
              -- Right
            , ( ( vec3 1 1 1, vec3 1 1 -1, vec3 1 -1 1, vec3 1 -1 -1 ), vec4 0 0 1 1 )
              -- Back
            , ( ( vec3 1 1 -1, vec3 -1 1 -1, vec3 1 -1 -1, vec3 -1 -1 -1 ), vec4 1 1 0 1 )
              -- Top
            , ( ( vec3 -1 1 -1, vec3 1 1 -1, vec3 -1 1 1, vec3 1 1 1 ), vec4 0.5 0 0 1 )
              -- Bottom
            , ( ( vec3 -1 -1 1, vec3 1 -1 1, vec3 -1 -1 -1, vec3 1 -1 -1 ), vec4 0.5 0 0 1 )
            ]


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


makeModelView : Box -> Mat4
makeModelView box =
    mul (T.moveTo box.coord) <| mul (T.yaw box.yaw) (T.pitch box.pitch)



-- Vertex shader for the Box.


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



-- Fragment shader for the Box.


fragmentShader : Shader {} u { vcolor : Vec4 }
fragmentShader =
    [glsl|

precision mediump float;

varying vec4 vcolor;

void main (void) {
    gl_FragColor = vcolor;
}

    |]
