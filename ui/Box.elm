module Box
    exposing
        ( Box
        , makeBox
        , yaw
        , render
        )

import Face exposing (..)
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
    }


makeBox : Vec3 -> Box
makeBox coord =
    { mesh = mesh
    , coord = coord
    , pitch = 0
    , yaw = 0
    }


yaw : Float -> Box -> Box
yaw theta box =
    { box | yaw = theta }


render : Mat4 -> Box -> Renderable
render perspective box =
    WebGL.render vertexShader
        fragmentShader
        box.mesh
        { perspective = perspective
        , modelView = makeModelView box
        }


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


makeModelView : Box -> Mat4
makeModelView box =
    mul (T.moveTo box.coord) <| mul (T.yaw box.yaw) (T.pitch box.pitch)
