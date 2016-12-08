module Chamber exposing (Model, Msg, init, update, view, subscriptions)

import Brick exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import WebGL exposing (..)


type alias Model =
    Int


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( 1, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text "Chamber Scenery" ]


viewScene : Model -> Html Msg
viewScene model =
    WebGL.toHtml [ Attr.width sceneWidth, Attr.height sceneHeight ] []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


sceneWidth : Int
sceneWidth =
    500


sceneHeight : Int
sceneHeight =
    400
