module GameMap exposing (tiles)

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
      , xLength = 13
      , yLength = 13
      }
    , { color = Color.darkGray
      , x = 0
      , y = 0
      , z = -0.01
      , xLength = 3
      , yLength = 13
      }
    , { color = Color.darkGray
      , x = 4
      , y = 0
      , z = -0.01
      , xLength = 5
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
