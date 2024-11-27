module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Kdl.Parse exposing (parse, getErrorMessage, messageToString)
import Kdl.Serialize exposing (serialize)
import Kdl.Util exposing (k)

main = Browser.sandbox
    { init = "myNode 1 2 3 key=\"val\" {child;}"
    , update = k
    , view = view
    }

view : String -> Html String
view s =
    let
        rightText = case parse s of
            Ok kdl -> serialize kdl
            Err e -> getErrorMessage s e |> messageToString
    in div
        [ style "display" "grid"
        , style "grid-template-columns" "1fr 1fr"
        , style "height" "100vh"
        ]
        [ textarea
            [ onInput identity
            , style "font-family" "mono"
            ]
            [ text s
            ]
        , textarea
            [ readonly True
            , style "font-family" "mono"
            ]
            [ text rightText
            ]
        ]
