module Chamber exposing (Model, Msg, init, update, view, subscriptions)

import AnimationFrame exposing (diffs)
import Box exposing (Box, makeBox, yaw)
import Char exposing (fromCode)
import Html exposing (..)
import Html.Attributes as Attr


--import Html.Events as Evts

import Keyboard exposing (KeyCode, downs)
import Math.Matrix4 exposing (Mat4, makePerspective, makeLookAt, mul)
import Math.Vector3 exposing (Vec3, vec3, getX, getY, getZ)
import Platform.Sub exposing (batch)
import Room exposing (Room, makeRoom)
import Time exposing (Time, inSeconds)
import WebGL exposing (..)


type alias Model =
    { perspective : Mat4
    , cameraFocus : Vec3
    , cameraEye : Vec3
    , box : Box
    , room : Room
    }


type Msg
    = Animate Time
    | KeyPressed KeyCode


init : ( Model, Cmd Msg )
init =
    ( { perspective =
            makePerspective 45 (toFloat sceneWidth / toFloat sceneHeight) 0.1 100
      , cameraFocus = vec3 0 0 -100
      , cameraEye = vec3 0 0 0
      , box = makeBox <| vec3 0 0 0
      , room = makeRoom (vec3 0 0 0) (vec3 1 1 1)
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

        KeyPressed code ->
            ( handleKeyboard code model, Cmd.none )


handleKeyboard : KeyCode -> Model -> Model
handleKeyboard code model =
    case fromCode code of
        'Q' ->
            { model | box = Box.move (vec3 -1 0 0) model.box }

        'W' ->
            { model | box = Box.move (vec3 1 0 0) model.box }

        'A' ->
            { model | box = Box.move (vec3 0 -1 0) model.box }

        'S' ->
            { model | box = Box.move (vec3 0 1 0) model.box }

        'Z' ->
            { model | box = Box.move (vec3 0 0 -1) model.box }

        'X' ->
            { model | box = Box.move (vec3 0 0 1) model.box }

        'E' ->
            { model | room = Room.move (vec3 -1 0 0) model.room }

        'R' ->
            { model | room = Room.move (vec3 1 0 0) model.room }

        'D' ->
            { model | room = Room.move (vec3 0 -1 0) model.room }

        'F' ->
            { model | room = Room.move (vec3 0 1 0) model.room }

        'C' ->
            { model | room = Room.move (vec3 0 0 -1) model.room }

        'V' ->
            { model | room = Room.move (vec3 0 0 1) model.room }

        'Y' ->
            { model | room = Room.scale (vec3 -1 0 0) model.room }

        'U' ->
            { model | room = Room.scale (vec3 1 0 0) model.room }

        'H' ->
            { model | room = Room.scale (vec3 0 -1 0) model.room }

        'J' ->
            { model | room = Room.scale (vec3 0 1 0) model.room }

        'N' ->
            { model | room = Room.scale (vec3 0 0 -1) model.room }

        'M' ->
            { model | room = Room.scale (vec3 0 0 1) model.room }

        _ ->
            model


view : Model -> Html Msg
view model =
    div [ Attr.style [ ( "width", "500px" ) ] ]
        [ viewScene model
        , viewBoxNavigation model
        , viewRoomNavigation model
        , viewRoomScaling model
        ]


viewScene : Model -> Html Msg
viewScene model =
    let
        viewerPerspective_ =
            viewerPerspective model
    in
        WebGL.toHtml [ Attr.width sceneWidth, Attr.height sceneHeight ] <|
            List.map (Box.render viewerPerspective_) [ model.box ]
                ++ List.map (Room.render viewerPerspective_) [ model.room ]


viewBoxNavigation : Model -> Html Msg
viewBoxNavigation model =
    div [ Attr.style [ ( "border-style", "solid" ), ( "margin", "1px" ) ] ]
        [ p [] [ text "Box Navigation" ]
        , p []
            [ text <|
                "Position (X, Y, Z): "
                    ++ "("
                    ++ (toString <| getX model.box.coord)
                    ++ ", "
                    ++ (toString <| getY model.box.coord)
                    ++ ", "
                    ++ (toString <| getZ model.box.coord)
                    ++ ")"
            ]
        , p [] [ text "'q' => -X || 'w' => +X" ]
        , p [] [ text "'a' => -Y || 's' => +Y" ]
        , p [] [ text "'z' => -Z || 'x' => +Z" ]
        ]


viewRoomNavigation : Model -> Html Msg
viewRoomNavigation model =
    div [ Attr.style [ ( "border-style", "solid" ), ( "margin", "1px" ) ] ]
        [ p [] [ text "Room Navigation" ]
        , p []
            [ text <|
                "Position (X, Y, Z): "
                    ++ "("
                    ++ (toString <| getX model.room.coord)
                    ++ ", "
                    ++ (toString <| getY model.room.coord)
                    ++ ", "
                    ++ (toString <| getZ model.room.coord)
                    ++ ")"
            ]
        , p [] [ text "'e' => -X || 'r' => +X" ]
        , p [] [ text "'d' => -Y || 'f' => +Y" ]
        , p [] [ text "'c' => -Z || 'v' => +Z" ]
        ]


viewRoomScaling : Model -> Html Msg
viewRoomScaling model =
    div [ Attr.style [ ( "border-style", "solid" ), ( "margin", "1px" ) ] ]
        [ p [] [ text "Room Scaling" ]
        , p []
            [ text <|
                "Scale factors (X, Y, Z): "
                    ++ "("
                    ++ (toString <| getX model.room.scale)
                    ++ ", "
                    ++ (toString <| getY model.room.scale)
                    ++ ", "
                    ++ (toString <| getZ model.room.scale)
                    ++ ")"
            ]
        , p [] [ text "'y' => -X || 'u' => +X" ]
        , p [] [ text "'h' => -Y || 'j' => +Y" ]
        , p [] [ text "'n' => -Z || 'm' => +Z" ]
        ]


viewerPerspective : Model -> Mat4
viewerPerspective model =
    let
        camera =
            makeLookAt model.cameraEye model.cameraFocus <| vec3 0 1 0
    in
        mul model.perspective camera


subscriptions : Model -> Sub Msg
subscriptions model =
    batch [ diffs Animate, downs KeyPressed ]


sceneWidth : Int
sceneWidth =
    500


sceneHeight : Int
sceneHeight =
    400
