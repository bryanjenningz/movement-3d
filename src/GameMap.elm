module GameMap exposing (Obstacle(..), gameWalls, tiles, unwalkableEdges)

import Color exposing (Color)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Material as Material
import Vector3d


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


type alias Wall =
    { x1 : Float
    , y1 : Float
    , x2 : Float
    , y2 : Float
    }


gameWalls : List (Scene3d.Entity Meters)
gameWalls =
    viewBuilding (Point3d.meters 4 2 0)


viewWall : Wall -> Scene3d.Entity Meters
viewWall { x1, y1, x2, y2 } =
    let
        wallHeight =
            1
    in
    Scene3d.quad (Material.color Color.darkBrown)
        (Point3d.meters x1 y1 wallHeight)
        (Point3d.meters x1 y1 0)
        (Point3d.meters x2 y2 0)
        (Point3d.meters x2 y2 wallHeight)


viewBuilding : Point3d Meters Meters -> List (Scene3d.Entity Meters)
viewBuilding location =
    let
        { x, y } =
            Point3d.toMeters location
    in
    [ { x1 = x + 0.5, y1 = y + 4.5, x2 = x + 0.5, y2 = y + 0.5 }
    , { x1 = x + 0.5, y1 = y + 4.5, x2 = x + 3.5, y2 = y + 4.5 }
    , { x1 = x + 3.5, y1 = y + 4.5, x2 = x + 3.5, y2 = y + 0.5 }
    , { x1 = x + 2.5, y1 = y + 0.5, x2 = x + 3.5, y2 = y + 0.5 }
    , { x1 = x + 0.5, y1 = y + 0.5, x2 = x + 1.5, y2 = y + 0.5 }
    ]
        |> List.map viewWall


type alias Xy =
    ( Int, Int )


type alias WallLength =
    Int


type Obstacle
    = HorizontalWall Xy WallLength


unwalkableEdges : List Obstacle -> List ( Xy, Xy )
unwalkableEdges obstacles =
    List.concatMap obstacleEdges obstacles


obstacleEdges : Obstacle -> List ( Xy, Xy )
obstacleEdges obstacle =
    case obstacle of
        HorizontalWall ( x, y ) length ->
            List.range x (x + length - 1) |> List.map (\newX -> ( ( newX, y ), ( newX, y + 1 ) ))
