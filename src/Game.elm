module Game exposing (Direction(..), Object(..), PlayerAction(..), Position, State, update)


type alias State =
    { objects : List ( Position, Object )
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type PlayerAction
    = Move Direction


type Object
    = Player
    | Wall
    | Floor


type alias Position =
    ( Int, Int )


getNeighbour dir ( x, y ) =
    case dir of
        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )

        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )


isPassable pos model =
    model.objects
        |> List.any (\( p, obj ) -> p == pos && obj == Wall)
        |> not


movePlayer dir model =
    { objects =
        model.objects
            |> List.map
                (\( pos, obj ) ->
                    let
                        targetTile =
                            getNeighbour dir pos
                    in
                    if obj == Player && isPassable targetTile model then
                        ( getNeighbour dir pos, obj )

                    else
                        ( pos, obj )
                )
    }


update : PlayerAction -> State -> State
update action state =
    case action of
        Move dir ->
            movePlayer dir state
