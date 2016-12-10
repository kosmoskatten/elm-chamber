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
      -- Box navigation.
    | BoxAddPosX
    | BoxDecPosX
    | BoxAddPosY
    | BoxDecPosY
    | BoxAddPosZ
    | BoxDecPosZ
      -- Room navigation.
    | RoomAddPosX
    | RoomDecPosX
    | RoomAddPosY
    | RoomDecPosY
    | RoomAddPosZ
    | RoomDecPosZ
      -- Room scaling.
    | RoomAddScaleX
    | RoomDecScaleX
    | RoomAddScaleY
    | RoomDecScaleY
    | RoomAddScaleZ
    | RoomDecScaleZ


init : ( Model, Cmd Msg )
init =
    ( { perspective =
            makePerspective 45 (toFloat sceneWidth / toFloat sceneHeight) 0.1 100
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

        BoxAddPosX ->
            ( { model | box = Box.move (vec3 1 0 0) model.box }, Cmd.none )

        BoxDecPosX ->
            ( { model | box = Box.move (vec3 -1 0 0) model.box }, Cmd.none )

        BoxAddPosY ->
            ( { model | box = Box.move (vec3 0 1 0) model.box }, Cmd.none )

        BoxDecPosY ->
            ( { model | box = Box.move (vec3 0 -1 0) model.box }, Cmd.none )

        BoxAddPosZ ->
            ( { model | box = Box.move (vec3 0 0 1) model.box }, Cmd.none )

        BoxDecPosZ ->
            ( { model | box = Box.move (vec3 0 0 -1) model.box }, Cmd.none )

        RoomAddPosX ->
            ( { model | room = Room.move (vec3 1 0 0) model.room }, Cmd.none )

        RoomDecPosX ->
            ( { model | room = Room.move (vec3 -1 0 0) model.room }, Cmd.none )

        RoomAddPosY ->
            ( { model | room = Room.move (vec3 0 1 0) model.room }, Cmd.none )

        RoomDecPosY ->
            ( { model | room = Room.move (vec3 0 -1 0) model.room }, Cmd.none )

        RoomAddPosZ ->
            ( { model | room = Room.move (vec3 0 0 1) model.room }, Cmd.none )

        RoomDecPosZ ->
            ( { model | room = Room.move (vec3 0 0 -1) model.room }, Cmd.none )

        RoomAddScaleX ->
            ( { model | room = Room.scale (vec3 1 0 0) model.room }, Cmd.none )

        RoomDecScaleX ->
            ( { model | room = Room.scale (vec3 -1 0 0) model.room }, Cmd.none )

        RoomAddScaleY ->
            ( { model | room = Room.scale (vec3 0 1 0) model.room }, Cmd.none )

        RoomDecScaleY ->
            ( { model | room = Room.scale (vec3 0 -1 0) model.room }, Cmd.none )

        RoomAddScaleZ ->
            ( { model | room = Room.scale (vec3 0 0 1) model.room }, Cmd.none )

        RoomDecScaleZ ->
            ( { model | room = Room.scale (vec3 0 0 -1) model.room }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ Attr.style [ ( "width", "500px" ) ] ]
        [ viewScene model
        , viewBoxPosControl model
        , viewRoomPosControl model
        , viewRoomScaleControl model
        ]


viewScene : Model -> Html Msg
viewScene model =
    WebGL.toHtml [ Attr.width sceneWidth, Attr.height sceneHeight ] <|
        List.map (Box.render model.perspective) [ model.box ]
            ++ List.map (Room.render model.perspective) [ model.room ]


viewBoxPosControl : Model -> Html Msg
viewBoxPosControl model =
    div [ Attr.style [ ( "border-style", "solid" ) ] ]
        [ p [] [ text "Box Position" ]
        , table []
            [ controlElement model "x: " BoxDecPosX BoxAddPosX (\m -> getX m.box.coord)
            , controlElement model "y: " BoxDecPosY BoxAddPosY (\m -> getY m.box.coord)
            , controlElement model "z: " BoxDecPosZ BoxAddPosZ (\m -> getZ m.box.coord)
            ]
        ]


viewRoomPosControl : Model -> Html Msg
viewRoomPosControl model =
    div [ Attr.style [ ( "border-style", "solid" ) ] ]
        [ p [] [ text "Room Position" ]
        , table []
            [ controlElement model "x: " RoomDecPosX RoomAddPosX (\m -> getX m.room.coord)
            , controlElement model "y: " RoomDecPosY RoomAddPosY (\m -> getY m.room.coord)
            , controlElement model "z: " RoomDecPosZ RoomAddPosZ (\m -> getZ m.room.coord)
            ]
        ]


viewRoomScaleControl : Model -> Html Msg
viewRoomScaleControl model =
    div [ Attr.style [ ( "border-style", "solid" ) ] ]
        [ p [] [ text "Room Scale" ]
        , table []
            [ controlElement model "x: " RoomDecScaleX RoomAddScaleX (\m -> getX m.room.scale)
            , controlElement model "y: " RoomDecScaleY RoomAddScaleY (\m -> getY m.room.scale)
            , controlElement model "z: " RoomDecScaleZ RoomAddScaleZ (\m -> getZ m.room.scale)
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
