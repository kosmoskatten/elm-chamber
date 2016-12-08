module Chamber exposing (Model, Msg, init, update, view, subscriptions)

import AnimationFrame exposing (diffs)
import Brick exposing (Brick, makeBrick, yaw)
import Html exposing (..)
import Html.Attributes as Attr
import Math.Matrix4 exposing (Mat4, makePerspective)
import Math.Vector3 exposing (vec3)
import Time exposing (Time, inSeconds)
import WebGL exposing (..)


type alias Model =
    { perspective : Mat4
    , brick : Brick
    , floor : List Brick
    }


type Msg
    = Animate Time


init : ( Model, Cmd Msg )
init =
    ( { perspective =
            makePerspective 45 (toFloat sceneWidth / toFloat sceneHeight) 0.1 100
      , brick = makeBrick <| vec3 0 10 -50
      , floor = makeFloor
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            ( { model | brick = Brick.yaw (inSeconds dt * (pi / 4) + model.brick.yaw) model.brick }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "Chamber Scenery" ]
        , viewScene model
        ]


viewScene : Model -> Html Msg
viewScene model =
    WebGL.toHtml [ Attr.width sceneWidth, Attr.height sceneHeight ] <|
        List.map (renderBrick model.perspective) [ model.brick ]
            ++ List.map (renderBrick model.perspective) model.floor


subscriptions : Model -> Sub Msg
subscriptions model =
    diffs Animate


renderBrick : Mat4 -> Brick -> Renderable
renderBrick perspective brick =
    render Brick.vertexShader Brick.fragmentShader brick.mesh { perspective = perspective, modelView = brick.modelView }


makeFloor : List Brick
makeFloor =
    List.concatMap (makeFloorStrip -100 0 -10) <| List.range -10 10


makeFloorStrip : Int -> Int -> Float -> Int -> List Brick
makeFloorStrip far near y x =
    List.map (\z -> makeBrick <| vec3 (toFloat x) y (toFloat z)) <| List.range far near


sceneWidth : Int
sceneWidth =
    500


sceneHeight : Int
sceneHeight =
    400
