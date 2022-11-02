module Monster exposing
    ( AliveMonsterState
    , DeadMonsterState
    , Hit
    , Location
    , Monster(..)
    , TravelPath
    , findAliveMonster
    , generateMonsterTravelPaths
    , init
    , killMonster
    , pointLocation
    , respawnMonster
    , respawnTime
    , shortestPath
    , updateAliveMonster
    , weightedXyRange
    , xyRange
    )

import Color exposing (Color)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Random exposing (Generator)
import Vector3d


type alias Hit =
    { amount : Int
    , disappearTime : Int
    }


type Monster
    = AliveMonster AliveMonsterState
    | DeadMonster DeadMonsterState


type alias Location =
    Point3d Meters Meters


type alias TravelPath =
    List Location


type alias AliveMonsterState =
    { id : Int
    , name : String
    , color : Color
    , respawnLocation : Location
    , location : Location
    , health : Int
    , maxHealth : Int
    , hits : List Hit
    , travelPath : TravelPath
    }


type alias DeadMonsterState =
    { id : Int
    , name : String
    , color : Color
    , respawnLocation : Location
    , maxHealth : Int
    , respawnAt : Int
    }


respawnTime : Int
respawnTime =
    -- 20 seconds
    20000


init : List Monster
init =
    [ Point3d.meters -3 3 0
    , Point3d.meters -3 -3 0
    , Point3d.meters 3 3 0
    , Point3d.meters 3 -3 0
    ]
        |> List.indexedMap initGoblin


initGoblin : Int -> Location -> Monster
initGoblin id location =
    AliveMonster
        { id = id
        , name = "Goblin (level 2)"
        , color = Color.darkPurple
        , respawnLocation = location
        , location = location
        , health = 3
        , maxHealth = 3
        , hits = []
        , travelPath = []
        }


findAliveMonster : Int -> List Monster -> Maybe AliveMonsterState
findAliveMonster monsterId monsters =
    List.filterMap
        (\monster ->
            case monster of
                AliveMonster aliveMonster ->
                    if aliveMonster.id == monsterId then
                        Just aliveMonster

                    else
                        Nothing

                DeadMonster _ ->
                    Nothing
        )
        monsters
        |> List.head


updateAliveMonster : Int -> (AliveMonsterState -> Monster) -> List Monster -> List Monster
updateAliveMonster monsterId updater monsters =
    List.map
        (\monster ->
            case monster of
                AliveMonster aliveMonster ->
                    if aliveMonster.id == monsterId then
                        updater aliveMonster

                    else
                        AliveMonster aliveMonster

                DeadMonster deadMonster ->
                    DeadMonster deadMonster
        )
        monsters


respawnMonster : Int -> DeadMonsterState -> Monster
respawnMonster time monster =
    if time >= monster.respawnAt then
        AliveMonster
            { id = monster.id
            , name = monster.name
            , color = monster.color
            , health = monster.maxHealth
            , maxHealth = monster.maxHealth
            , respawnLocation = monster.respawnLocation
            , location = monster.respawnLocation
            , hits = []
            , travelPath = []
            }

    else
        DeadMonster monster


killMonster : Int -> AliveMonsterState -> DeadMonsterState
killMonster time monster =
    { id = monster.id
    , name = monster.name
    , color = monster.color
    , maxHealth = monster.maxHealth
    , respawnAt = time + respawnTime
    , respawnLocation = monster.respawnLocation
    }


addPoints : Location -> Location -> Location
addPoints p1 p2 =
    Vector3d.plus (Vector3d.from Point3d.origin p1) (Vector3d.from Point3d.origin p2)
        |> Vector3d.toMeters
        |> Point3d.fromMeters


xyRange : Int -> Int -> List ( Float, Float )
xyRange low high =
    List.range low high
        |> List.concatMap (\y -> List.range low high |> List.map (\x -> ( toFloat x, toFloat y )))


weightedXyRange : Int -> Int -> List ( Float, ( Float, Float ) )
weightedXyRange low high =
    xyRange low high |> List.map (\xy -> ( 1, xy ))


pointLocation : ( Float, Float ) -> Location
pointLocation ( x, y ) =
    Point3d.meters x y 0


generateMonsterTravelPaths : List Monster -> Generator (List (Maybe TravelPath))
generateMonsterTravelPaths monsters =
    Random.weighted ( 300, Nothing ) (weightedXyRange -1 1 |> List.map (Tuple.mapSecond Just))
        |> Random.list (List.length monsters)
        |> Random.map (List.map (Maybe.map pointLocation))
        |> Random.map
            (\maybePoints ->
                List.map2
                    (\maybePoint m ->
                        case ( maybePoint, m ) of
                            ( Just point, AliveMonster monster ) ->
                                Just (shortestPath monster.location (addPoints point monster.respawnLocation))

                            _ ->
                                Nothing
                    )
                    maybePoints
                    monsters
            )


shortestPath : Location -> Location -> TravelPath
shortestPath start destination =
    if start == destination then
        []

    else
        let
            xDiff =
                (destination |> Point3d.toMeters |> .x)
                    - (start |> Point3d.toMeters |> .x)
                    |> clamp -1 1

            yDiff =
                (destination |> Point3d.toMeters |> .y)
                    - (start |> Point3d.toMeters |> .y)
                    |> clamp -1 1

            newStart =
                Vector3d.plus
                    (Vector3d.from Point3d.origin (Point3d.meters xDiff yDiff 0))
                    (Vector3d.from Point3d.origin start)
                    |> Vector3d.toMeters
                    |> Point3d.fromMeters
        in
        newStart :: shortestPath newStart destination
