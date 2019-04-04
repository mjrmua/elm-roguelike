module Input exposing (Event(..), State, update)

import Game exposing (Direction(..), PlayerAction(..))
import Maybe


type alias State =
    {}


type Event
    = KeyPress String


toDirection : Event -> Maybe Direction
toDirection event =
    case event of
        KeyPress "a" ->
            Just Left

        KeyPress "d" ->
            Just Right

        KeyPress "w" ->
            Just Up

        KeyPress "s" ->
            Just Down

        _ ->
            Nothing


update : Event -> State -> ( State, Maybe PlayerAction )
update event state =
    ( state, toDirection event |> Maybe.map Move )
