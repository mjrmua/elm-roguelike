module Main exposing (..)

import Browser
import Html.Styled.Attributes exposing (css)
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Json.Decode as Decode
import Browser.Events exposing (onKeyPress)

---- MODEL ----
type Object = 
      Player
    | Wall
    | Floor

type alias Position = (Int, Int)

type alias Model =
    {
        objects: List (Position, Object)
    }

room width height = 
    [
         List.range 0 width |> List.map (\x->((x,0),Wall))
        ,List.range 0 width |> List.map (\x->((x,height),Wall))
        ,List.range 0 height |> List.map (\y->((0,y),Wall))
        ,List.range 0 height |> List.map (\y->((width,y),Wall))
    ]
    |> List.concat 

init : ( Model, Cmd Msg )
init =
    ( {
        objects =  ((3,3),Player) :: (room 10 10)
    }, Cmd.none )

---- UPDATE ----
type Direction 
    = Up
    | Down
    | Left
    | Right

type Msg
    = Move Direction
    | NoOp

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map 
    (toDirection >> Maybe.map Move >> Maybe.withDefault NoOp) 
    (Decode.field "key" Decode.string)

toDirection : String -> Maybe Direction
toDirection string =
  case string of
    "a" -> Maybe.Just Left
    "d" -> Maybe.Just Right
    "w" -> Maybe.Just Up
    "s" -> Maybe.Just Down
    _ -> Maybe.Nothing

getNeighbour dir (x,y) =
    case dir of 
        Left    -> (x-1,y)
        Right   -> (x+1,y)
        Up      -> (x,y-1)
        Down    -> (x,y+1)

isPassable pos model = 
    model.objects 
        |> List.any (\(p,obj)->p==pos && obj==Wall) 
        |> not

movePlayer dir model = 
    {
        objects = model.objects |> List.map (\(pos,obj)->
            if (obj==Player) 
                then 
                    let targetTile = getNeighbour dir pos in
                    if (isPassable targetTile model) 
                        then (getNeighbour dir pos,obj)
                        else (pos,obj)
                else (pos,obj)
        )
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        Move dir -> ( movePlayer dir model, Cmd.none )
        NoOp -> ( model, Cmd.none )
        
---- VIEW ----

characterForObject: Object -> String
characterForObject o = 
    case o of
        Wall    -> "#"
        Floor   -> "."
        Player  -> "@"

fst (a,_) = a
snd (_,b) = b

objectView: (Position, Object) -> Html Msg
objectView ((x,y),object) =   
    div [
        css [
              position absolute
            , top (Css.em <| toFloat y)
            , left (Css.em <| toFloat x)
        ]] 
        [
            text (characterForObject object)
        ]

view : Model -> Html Msg
view model =
    div [
    ]
        [
             p [] [text "Welcome to Elm RogueLike"]
            ,p [] [text "A,S,D,F to move around"]
            ,div [] (List.map objectView model.objects)
        ]


--- SUBSCRIPTIONS ---

subscriptions : Model -> Sub Msg
subscriptions model = onKeyPress keyDecoder


---- PROGRAM ----


main : Program () Model Msg 
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
