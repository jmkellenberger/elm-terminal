module Main exposing (Mode, Model, Msg, main)

import Browser
import Html exposing (Html, div, li, p, text, ul)
import Html.Attributes as Attributes
import Keyboard exposing (Key(..), KeyChange(..))
import VitePluginHelper


type Mode
    = Command
    | Input


type alias Model =
    { pressedKeys : List Key
    , keyChanges : List Key
    , inputBuffer : List String
    , commandBuffer : List String
    , history : List String
    , mode : Mode
    }


enterPressed : Model -> Bool
enterPressed model =
    List.member Enter model.pressedKeys


escapePressed : Model -> Bool
escapePressed model =
    List.member Escape model.pressedKeys


shiftPressed : Model -> Bool
shiftPressed model =
    List.member Shift model.pressedKeys


init : () -> ( Model, Cmd Msg )
init _ =
    ( { pressedKeys = []
      , keyChanges = []
      , inputBuffer = []
      , commandBuffer = []
      , history = [ "Welcome, User." ]
      , mode = Command
      }
    , Cmd.none
    )


type Msg
    = KeyboardMsg Keyboard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            let
                ( pressedKeys, maybeKeyChange ) =
                    Keyboard.updateWithKeyChange Keyboard.anyKeyOriginal keyMsg model.pressedKeys

                keyChanges : List Key
                keyChanges =
                    case maybeKeyChange of
                        Just keyChange ->
                            case keyChange of
                                KeyDown key ->
                                    key :: model.keyChanges

                                _ ->
                                    []

                        Nothing ->
                            []
            in
            ( keyHandler
                { model
                    | pressedKeys = pressedKeys
                    , keyChanges = keyChanges
                }
            , Cmd.none
            )


keyHandler : Model -> Model
keyHandler model =
    case ( model.mode, List.head model.keyChanges ) of
        ( Input, Just key ) ->
            case key of
                Character c ->
                    if shiftPressed model then
                        { model | inputBuffer = String.toUpper c :: model.inputBuffer }

                    else
                        { model | inputBuffer = c :: model.inputBuffer }

                Spacebar ->
                    { model | inputBuffer = " " :: model.inputBuffer }

                Delete ->
                    { model | inputBuffer = [] }

                Enter ->
                    { model
                        | inputBuffer = []
                        , history =
                            List.foldl
                                (++)
                                ""
                                model.inputBuffer
                                :: model.history
                    }

                Backspace ->
                    if shiftPressed model then
                        { model | inputBuffer = [] }

                    else
                        { model | inputBuffer = List.drop 1 model.inputBuffer }

                Escape ->
                    { model | mode = Command }

                _ ->
                    model

        ( Command, Just key ) ->
            case key of
                Character "i" ->
                    { model | mode = Input }

                _ ->
                    model

        ( _, Nothing ) ->
            model


view : Model -> Html msg
view model =
    div [ Attributes.class "crt", Attributes.id "terminal" ]
        [ div [ Attributes.id "panels" ]
            [ div [ Attributes.id "left-panel" ]
                [ viewHistory model
                , viewInputBuffer model
                ]
            , div [ Attributes.id "right-panel" ]
                [ div [ Attributes.id "upper-right-panel" ] [ Html.img [ Attributes.id "map", Attributes.src <| VitePluginHelper.asset "/src/assets/map.svg?" ] [] ]
                , div [ Attributes.id "bottom-right-panel" ]
                    [ p [] [ text ("Enter: " ++ Debug.toString (enterPressed model)) ]
                    , p [] [ text ("Escape: " ++ Debug.toString (escapePressed model)) ]
                    , p [] [ text ("Shift: " ++ Debug.toString (shiftPressed model)) ]
                    , p [] [ text "Currently pressed:" ]
                    , ul []
                        (List.map (\key -> li [] [ text (Debug.toString key) ]) model.pressedKeys)
                    ]
                ]
            ]
        , div [ Attributes.id "status-bar" ]
            [ viewModeIndicator model
            , div [ Attributes.id "status-center" ] []
            , div [ Attributes.id "status-right" ] []
            ]
        ]


viewHistory : Model -> Html msg
viewHistory model =
    div []
        (List.map
            (\command -> p [] [ text command ])
         <|
            List.reverse model.history
        )


viewInputBuffer : Model -> Html msg
viewInputBuffer model =
    let
        input : String
        input =
            List.foldl (++) "" model.inputBuffer
    in
    div [ Attributes.id "prompt" ] [ text ("~/ > " ++ input) ]


viewModeIndicator : Model -> Html msg
viewModeIndicator model =
    case model.mode of
        Command ->
            div [ Attributes.id "status-left" ] [ text "Mode: COMMAND" ]

        Input ->
            div [ Attributes.id "status-left" ] [ text "Mode: INPUT" ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map KeyboardMsg Keyboard.subscriptions


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
