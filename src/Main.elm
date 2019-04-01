module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyPress)
import Css exposing (..)
import Game exposing (Object(..), PlayerAction, Position)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Input
import Json.Decode as Decode



---- MODEL ----


type alias Model =
    { gameState : Game.State
    , inputState : Input.State
    }


room width height =
    [ List.range 0 width |> List.map (\x -> ( ( x, 0 ), Wall ))
    , List.range 0 width |> List.map (\x -> ( ( x, height ), Wall ))
    , List.range 0 height |> List.map (\y -> ( ( 0, y ), Wall ))
    , List.range 0 height |> List.map (\y -> ( ( width, y ), Wall ))
    ]
        |> List.concat


init : ( Model, Cmd Msg )
init =
    ( { gameState = { objects = ( ( 3, 3 ), Player ) :: room 10 10 }
      , inputState = {}
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Input Input.Event
    | NoOp


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map (Input.KeyPress >> Input) (Decode.field "key" Decode.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input event ->
            let
                ( inputState, playerAction ) =
                    Input.update event model.inputState
            in
            case playerAction of
                Maybe.Nothing ->
                    ( { model | inputState = inputState }, Cmd.none )

                Maybe.Just action ->
                    ( { model
                        | gameState = Game.update action model.gameState
                        , inputState = inputState
                      }
                    , Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


characterForObject : Object -> String
characterForObject o =
    case o of
        Wall ->
            "#"

        Floor ->
            "."

        Player ->
            "@"


fst ( a, _ ) =
    a


snd ( _, b ) =
    b


objectView : ( Position, Object ) -> Html Msg
objectView ( ( x, y ), object ) =
    div
        [ css
            [ position absolute
            , top (Css.em <| toFloat y)
            , left (Css.em <| toFloat x)
            ]
        ]
        [ text (characterForObject object)
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ p [] [ text "Welcome to Elm RogueLike" ]
        , p [] [ text "A,S,D,F to move around" ]
        , div [] (List.map objectView model.gameState.objects)
        ]



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress keyDecoder



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
