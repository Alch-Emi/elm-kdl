module Main exposing (main)

import Browser exposing (Document)
import Dict exposing (Dict)
import Html exposing (Html, div, details, h3, summary, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onInput)
import Html.Keyed as Keyed
import Http exposing (emptyBody, expectString, stringResolver, Response(..))
import Kdl.Parse exposing (getErrorMessage, messageToString, parse, Problem)
import Kdl.Serialize exposing (serialize)
import Kdl.Util exposing (flip, k, maybe, result)
import List exposing (filter, map)
import Platform.Cmd as Cmd
import Platform.Sub
import String exposing (lines, trim)
import Task exposing (Task)
import Tuple exposing (pair)

testIndexUrl     = "./tests/index.txt"
testExpectedBase = "./tests/expected_kdl/"
testInputBase    = "./tests/input/"

main = Browser.document
    { init = \() -> (RunningTests Dict.empty, Http.get {url = testIndexUrl, expect=expectString <| result (k (OopsiePoopsie "Failed to load test index")) IndexLoaded})
    , update = update
    , view = view
    , subscriptions = k Sub.none
    }

type Model
    = RunningTests (Dict String (Maybe (String, Maybe String, Result Problem String)))
    | Failed String

type Msg
    = OopsiePoopsie String
    | IndexLoaded String
    | TestDataLoaded String (String, Maybe String)

isNone : Maybe a -> Bool
isNone m = case m of
    Nothing -> True
    _ -> False

getNextTest : Dict String (Maybe a) -> Maybe String
getNextTest =
    Dict.toList
    >> filter (Tuple.second >> isNone)
    >> map Tuple.first
    >> List.head

loadTestDataTask : String -> Task Msg (String, Maybe String)
loadTestDataTask testName = Task.map2 pair
    (
        Http.task
        { method = "GET"
        , headers = []
        , url = testInputBase ++ testName
        , body = emptyBody
        , resolver = stringResolver (\res -> case res of
                GoodStatus_ _ body -> Ok <| trim body
                _ -> Err <| OopsiePoopsie ("Failed to load test data for the test " ++ testName)
            )
        , timeout = Just <| 1000.0 * 30.0
        }
    )
    (
        Http.task
        { method = "GET"
        , headers = []
        , url = testExpectedBase ++ testName
        , body = emptyBody
        , resolver = stringResolver (\res -> case res of
                GoodStatus_ _ body -> Ok (Just <| trim body)
                BadStatus_ _ _ -> Ok Nothing
                _ -> Err <| OopsiePoopsie ("Failed to load test data for the test " ++ testName)
            )
        , timeout = Just <| 1000.0 * 30.0
        }
    )

loadTestData : String -> Cmd Msg
loadTestData s = Task.attempt (result identity (TestDataLoaded s)) (loadTestDataTask s)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    OopsiePoopsie s -> (Failed s, Cmd.none)
    IndexLoaded index ->
        let
            indexLines = lines index |> filter (not << String.isEmpty)
            testEntries = indexLines
                |> map (flip pair Nothing) 
                |> Dict.fromList
            firstTestToFetch = getNextTest testEntries
        in (RunningTests testEntries, maybe Cmd.none loadTestData firstTestToFetch)
    TestDataLoaded testName (input, expected) -> case model of
        RunningTests tests ->
            let
                parseResult = parse input |> Result.map (serialize >> trim)
                updatedTests = Dict.insert testName (Just (input, expected, parseResult)) tests
                nextTestToLoad = getNextTest updatedTests
                loadNextTest = maybe Cmd.none loadTestData nextTestToLoad
            in (RunningTests updatedTests, loadNextTest)
        _ -> (model, Cmd.none)

viewPanel : String -> String -> Html msg
viewPanel name value = div
    []
    [ h3 [] [text name]
    , div
        [ style "white-space" "pre"
        , style "font-family" "mono"
        ]
        [ text value
        ]
    ]

viewTest : (String, Maybe (String, Maybe String, Result Problem String)) -> (String, Html msg)
viewTest (testName, result) = pair testName <|
    details
        []
        (
            case result of
                Just (inputData, expectedResult, actualResult) ->
                    let
                        expectedResultString = case expectedResult of
                            Just exp -> exp
                            Nothing -> "<parse should not succeed>"
                        actualResultString = case actualResult of
                            Ok res -> res
                            Err e -> "<parse error>\n\n" ++ (getErrorMessage inputData e |> messageToString)
                        testSuccess = expectedResult == (Result.toMaybe actualResult)
                    in
                        [ summary []
                            [ text (if testSuccess then "✓" else "✗")
                            , text testName
                            ]
                        , viewPanel "Input Data" inputData
                        , viewPanel "Expected Result" expectedResultString
                        , viewPanel "Actual Result" actualResultString
                        ]
                Nothing ->
                        [ summary []
                            [ text "⋯"
                            , text testName
                            ]
                        ]
        )

view : Model -> Document msg
view m =
    case m of
        Failed s ->
            { title = "Fatal Error while running tests!"
            , body =
                [ div
                    []
                    [text s]
                ]
            }
        RunningTests tests ->
            { title = "Elm KDL test suite"
            , body = List.singleton <| Keyed.node "div" []
                (
                    tests
                        |> Dict.toList
                        |> List.map viewTest
                )
            }
