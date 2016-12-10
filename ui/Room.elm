module Room exposing (Room, makeRoom, render)

import Face exposing (..)
import List exposing (concatMap)
import Math.Matrix4 exposing (Mat4, makeScale, mul)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Transform as T
import WebGL exposing (Drawable(..), Renderable, Shader)


type alias Room =
    { mesh : Drawable Vertex
    , coord : Vec3
    , scale : Vec3
    }


makeRoom : Vec3 -> Vec3 -> Room
makeRoom coord scale =
    { mesh = mesh
    , coord = coord
    , scale = scale
    }


mesh : Drawable Vertex
mesh =
    Triangle <|
        concatMap makeFace
            [ -- Far wall.
              ( ( vec3 -1 1 -1, vec3 1 1 -1, vec3 -1 -1 -1, vec3 1 -1 -1 ), vec4 1 0 0 1 )
              -- Floor
            , ( ( vec3 -1 -1 -1, vec3 1 -1 -1, vec3 -1 -1 1, vec3 1 -1 1 ), vec4 0 1 0 1 )
            ]


makeModelView : Room -> Mat4
makeModelView room =
    mul (T.moveTo room.coord) (makeScale room.scale)


render : Mat4 -> Room -> Renderable
render perspective room =
    WebGL.render vertexShader
        fragmentShader
        room.mesh
        { perspective = perspective
        , modelView = makeModelView room
        }
