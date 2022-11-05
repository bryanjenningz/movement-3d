module GameMap exposing (Obstacle(..), gameWallEntities, shortestGamePath, shortestPath, tiles, unwalkableEdges)

import Color exposing (Color)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Queue exposing (Queue)
import Scene3d
import Scene3d.Material as Material
import Set exposing (Set)
import Vector3d



-- Tiles --


type alias Tile =
    { color : Color
    , x : Float
    , y : Float
    , z : Float
    , xLength : Float
    , yLength : Float
    }


gameTiles : List Tile
gameTiles =
    [ { color = Color.darkGreen
      , x = 0
      , y = 0
      , z = -0.02
      , xLength = 17
      , yLength = 15
      }
    , { color = Color.darkGray
      , x = 0
      , y = 0
      , z = -0.01
      , xLength = 3
      , yLength = 15
      }
    , { color = Color.darkGray
      , x = 5
      , y = 0
      , z = -0.01
      , xLength = 7
      , yLength = 3
      }
    ]


tiles : List (Scene3d.Entity Meters)
tiles =
    List.map viewTile gameTiles


viewTile : Tile -> Scene3d.Entity Meters
viewTile { color, xLength, yLength, x, y, z } =
    let
        point =
            Point3d.meters x y z
    in
    Scene3d.quad (Material.color color)
        (Point3d.translateBy (Vector3d.meters -(xLength / 2) -(yLength / 2) 0) point)
        (Point3d.translateBy (Vector3d.meters (xLength / 2) -(yLength / 2) 0) point)
        (Point3d.translateBy (Vector3d.meters (xLength / 2) (yLength / 2) 0) point)
        (Point3d.translateBy (Vector3d.meters -(xLength / 2) (yLength / 2) 0) point)



-- Walls --


type alias Xy =
    ( Int, Int )


type alias WallLength =
    Int


type Obstacle
    = HorizontalWall Xy WallLength
    | VerticalWall Xy WallLength


gameWallEntities : List (Scene3d.Entity Meters)
gameWallEntities =
    List.map obstacleToEntity gameWalls


gameWalls : List Obstacle
gameWalls =
    viewBuilding ( 5, 6 )


viewBuilding : Xy -> List Obstacle
viewBuilding ( x, y ) =
    [ VerticalWall ( x, y ) 4
    , HorizontalWall ( x, y ) 3
    , VerticalWall ( x + 3, y ) 4
    , HorizontalWall ( x, y - 4 ) 1
    , HorizontalWall ( x + 2, y - 4 ) 1
    ]


unwalkableEdges : List Obstacle -> List ( Xy, Xy )
unwalkableEdges obstacles =
    List.concatMap obstacleEdges obstacles


obstacleEdges : Obstacle -> List ( Xy, Xy )
obstacleEdges obstacle =
    case obstacle of
        HorizontalWall ( x, y ) length ->
            List.range x (x + length - 1)
                |> List.concatMap
                    (\newX ->
                        [ ( ( newX, y ), ( newX - 1, y + 1 ) )
                        , ( ( newX, y ), ( newX, y + 1 ) )
                        , ( ( newX, y ), ( newX + 1, y + 1 ) )
                        , ( ( newX, y + 1 ), ( newX - 1, y ) )
                        , ( ( newX, y + 1 ), ( newX, y ) )
                        , ( ( newX, y + 1 ), ( newX + 1, y ) )
                        ]
                    )

        VerticalWall ( x, y ) length ->
            List.range (y - length + 1) y
                |> List.concatMap
                    (\newY ->
                        [ ( ( x - 1, newY ), ( x, newY - 1 ) )
                        , ( ( x - 1, newY ), ( x, newY ) )
                        , ( ( x - 1, newY ), ( x, newY + 1 ) )
                        , ( ( x, newY ), ( x - 1, newY - 1 ) )
                        , ( ( x, newY ), ( x - 1, newY ) )
                        , ( ( x, newY ), ( x - 1, newY + 1 ) )
                        ]
                    )


obstacleToEntity : Obstacle -> Scene3d.Entity Meters
obstacleToEntity obstacle =
    case obstacle of
        HorizontalWall ( x, y ) length ->
            Scene3d.quad (Material.color Color.darkBrown)
                (Point3d.meters (toFloat x - 0.5) (toFloat y + 0.5) 1)
                (Point3d.meters (toFloat x - 0.5) (toFloat y + 0.5) 0)
                (Point3d.meters (toFloat x + toFloat length - 0.5) (toFloat y + 0.5) 0)
                (Point3d.meters (toFloat x + toFloat length - 0.5) (toFloat y + 0.5) 1)

        VerticalWall ( x, y ) length ->
            Scene3d.quad (Material.color Color.darkBrown)
                (Point3d.meters (toFloat x - 0.5) (toFloat y + 0.5) 1)
                (Point3d.meters (toFloat x - 0.5) (toFloat y + 0.5) 0)
                (Point3d.meters (toFloat x - 0.5) (toFloat y - toFloat length + 0.5) 0)
                (Point3d.meters (toFloat x - 0.5) (toFloat y - toFloat length + 0.5) 1)


shortestGamePath : Xy -> Xy -> Maybe (List Xy)
shortestGamePath =
    shortestPath gameWalls


shortestPath : List Obstacle -> Xy -> Xy -> Maybe (List Xy)
shortestPath obstacles start end =
    shortestPath_
        (unwalkableEdges obstacles
            |> List.concatMap (\( xy1, xy2 ) -> [ ( xy1, xy2 ), ( xy2, xy1 ) ])
            |> Set.fromList
        )
        (Queue.fromList [ ( start, [] ) ])
        (Set.fromList [ start ])
        end


shortestPath_ : Set ( Xy, Xy ) -> Queue ( Xy, List Xy ) -> Set Xy -> Xy -> Maybe (List Xy)
shortestPath_ unwalkableParts queue visited end =
    case Queue.remove queue of
        ( Nothing, _ ) ->
            Nothing

        ( Just ( xy, path ), newQueue ) ->
            if xy == end then
                Just (List.reverse (end :: path))

            else
                let
                    newNeighbors =
                        neighbors xy
                            |> List.filter
                                (\neighbor ->
                                    (not <| Set.member neighbor visited)
                                        && (not <| Set.member ( neighbor, xy ) unwalkableParts)
                                        && (not <| Set.member ( xy, neighbor ) unwalkableParts)
                                )
                in
                shortestPath_ unwalkableParts
                    (List.foldl
                        (\neighbor newQueue_ -> Queue.add ( neighbor, xy :: path ) newQueue_)
                        newQueue
                        newNeighbors
                    )
                    (List.foldl Set.insert visited newNeighbors)
                    end


neighbors : Xy -> List Xy
neighbors ( x, y ) =
    [ ( x, y - 1 )
    , ( x, y + 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y - 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y + 1 )
    , ( x - 1, y + 1 )
    ]
