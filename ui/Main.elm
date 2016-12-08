module Main exposing (main)

import Chamber exposing (..)
import Html as Html


main : Program Never Chamber.Model Chamber.Msg
main =
    Html.program
        { init = Chamber.init
        , view = Chamber.view
        , update = Chamber.update
        , subscriptions = Chamber.subscriptions
        }
