module Chamber exposing (Model, Msg, init, update, view, subscriptions)

import AnimationFrame exposing (diffs)
import Box exposing (Box, makeBox, yaw)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Evts
import Math.Matrix4 exposing (Mat4, makePerspective)
import Math.Vector3 exposing (Vec3, vec3, getX, getY, getZ)
import Room exposing (Room, makeRoom)
import Time exposing (Time, inSeconds)
import WebGL exposing (..)


type alias Model =
    { perspective : Mat4
    , box : Box
    , room : Room
    }


type Msg
    = Animate Time
    | BoxAddX
    | BoxDecX
    | BoxAddY
    | BoxDecY
    | BoxAddZ
    | BoxDecZ


init : ( Model, Cmd Msg )
init =
    ( { perspective =
            makePerspective 45 (toFloat sceneWidth / toFloat sceneHeight) 0.1 100
      , box = makeBox <| vec3 0 0 0
      , room = makeRoom (vec3 0 -5 -20) (vec3 2 2 3)
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Animate dt ->
            ( { model
                | box =
                    Box.yaw
                        (inSeconds dt
                            * (pi / 4)
                            + model.box.yaw
                        )
                        model.box
              }
            , Cmd.none
            )

        BoxAddX ->
            ( { model | box = Box.move (vec3 1 0 0) model.box }, Cmd.none )

        BoxDecX ->
            ( { model | box = Box.move (vec3 -1 0 0) model.box }, Cmd.none )

        BoxAddY ->
            ( { model | box = Box.move (vec3 0 1 0) model.box }, Cmd.none )

        BoxDecY ->
            ( { model | box = Box.move (vec3 0 -1 0) model.box }, Cmd.none )

        BoxAddZ ->
            ( { model | box = Box.move (vec3 0 0 1) model.box }, Cmd.none )

        BoxDecZ ->
            ( { model | box = Box.move (vec3 0 0 -1) model.box }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ Attr.style [ ( "width", "500px" ) ] ]
        [ viewScene model
        , viewBoxControl model
        ]


viewScene : Model -> Html Msg
viewScene model =
    WebGL.toHtml [ Attr.width sceneWidth, Attr.height sceneHeight ] <|
        List.map (Box.render model.perspective) [ model.box ]
            ++ List.map (Room.render model.perspective) [ model.room ]


viewBoxControl : Model -> Html Msg
viewBoxControl model =
    div [ Attr.style [ ( "border-style", "solid" ) ] ]
        [ p [] [ text "Box Controls" ]
        , table []
            [ controlElement model "x: " BoxDecX BoxAddX (\m -> getX m.box.coord)
            , controlElement model "y: " BoxDecY BoxAddY (\m -> getY m.box.coord)
            , controlElement model "z: " BoxDecZ BoxAddZ (\m -> getZ m.box.coord)
            ]
        ]


controlElement : Model -> String -> Msg -> Msg -> (Model -> Float) -> Html Msg
controlElement model label dec add getFrom =
    tr []
        [ td [] [ button [ Evts.onClick dec ] [ text "-" ] ]
        , td [] [ text <| label ++ toString (floor <| getFrom model) ]
        , td [] [ button [ Evts.onClick add ] [ text "+" ] ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    diffs Animate


sceneWidth : Int
sceneWidth =
    500


sceneHeight : Int
sceneHeight =
    400
