module Chamber exposing (Model, Msg, init, update, view, subscriptions)

import AnimationFrame exposing (diffs)
import Box exposing (Box, makeBox, yaw)
import Html exposing (..)
import Html.Attributes as Attr
import Math.Matrix4 exposing (Mat4, makePerspective)
import Math.Vector3 exposing (vec3)
import Time exposing (Time, inSeconds)
import WebGL exposing (..)


type alias Model =
    { perspective : Mat4
    , box : Box
    }


type Msg
    = Animate Time


init : ( Model, Cmd Msg )
init =
    ( { perspective =
            makePerspective 45 (toFloat sceneWidth / toFloat sceneHeight) 0.1 100
      , box = makeBox <| vec3 0 10 -50
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            ( { model | box = Box.yaw (inSeconds dt * (pi / 4) + model.box.yaw) model.box }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "Chamber Scenery" ]
        , viewScene model
        ]


viewScene : Model -> Html Msg
viewScene model =
    WebGL.toHtml [ Attr.width sceneWidth, Attr.height sceneHeight ] <|
        List.map (Box.render model.perspective) [ model.box ]


subscriptions : Model -> Sub Msg
subscriptions model =
    diffs Animate


sceneWidth : Int
sceneWidth =
    500


sceneHeight : Int
sceneHeight =
    400
