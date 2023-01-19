module Main exposing (Mode, Model, Msg, main)

import Browser
import Html exposing (Html, div, li, p, text, ul)
import Html.Attributes as Attributes
import Keyboard exposing (Key(..))


type Mode
    = Command
    | Input


type alias Model =
    { pressedKeys : List Key
    , buffer : List String
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
      , buffer = []
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
            ( { model
                | pressedKeys = Keyboard.update keyMsg model.pressedKeys
              }
                |> keyHandler
            , Cmd.none
            )


keyHandler : Model -> Model
keyHandler model =
    case ( model.mode, List.head model.pressedKeys ) of
        ( Input, Just key ) ->
            case key of
                Character c ->
                    { model | buffer = c :: model.buffer }

                Spacebar ->
                    { model | buffer = " " :: model.buffer }

                Delete ->
                    { model | buffer = [] }

                Enter ->
                    { model | buffer = [] }

                Backspace ->
                    { model | buffer = List.drop 1 model.buffer }

                Escape ->
                    { model | mode = Command }

                _ ->
                    model

        ( Command, Just key ) ->
            case key of
                Character "I" ->
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
                [ text "Welcome, User."
                , viewBuffer model
                ]
            , div [ Attributes.id "right-panel" ]
                [ div [ Attributes.id "upper-right-panel" ] [ text "PLACEHOLDER TEXT" ]
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


viewBuffer : Model -> Html msg
viewBuffer model =
    let
        input : String
        input =
            List.foldl (++) "" model.buffer
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
