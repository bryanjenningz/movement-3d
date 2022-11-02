module GameMap exposing (tiles)

import Color exposing (Color)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Material as Material
import Vector3d


type GameMapTile
    = GrassTile
    | RoadTile


gameMap : List String
gameMap =
    [ "GGGGRRGGGG"
    , "GGGGRRGGGG"
    , "GGGGRRGGGG"
    , "GGGGRRGGGG"
    , "GGGGRRGGGG"
    , "GGGGRRRRRR"
    , "GGGGRRRRRR"
    , "GGGGRRGGGG"
    , "GGGGRRGGGG"
    , "GGGGRRGGGG"
    ]


tiles : List (Scene3d.Entity Meters)
tiles =
    gameMap
        |> List.map (String.split "" >> List.map (toGameMapTile GrassTile))
        |> List.indexedMap
            (\y tileRow ->
                List.indexedMap
                    (\x tile ->
                        let
                            tileColor =
                                case tile of
                                    GrassTile ->
                                        Color.darkGreen

                                    RoadTile ->
                                        Color.darkGray
                        in
                        viewTile tileColor (Point3d.meters (toFloat x - gameMapOffset) (toFloat y - gameMapOffset) -0.01)
                    )
                    tileRow
            )
        |> List.concat


viewTile : Color -> Point3d Meters Meters -> Scene3d.Entity Meters
viewTile color point =
    Scene3d.quad (Material.color color)
        (Point3d.translateBy (Vector3d.meters -0.5 -0.5 0) point)
        (Point3d.translateBy (Vector3d.meters 0.5 -0.5 0) point)
        (Point3d.translateBy (Vector3d.meters 0.5 0.5 0) point)
        (Point3d.translateBy (Vector3d.meters -0.5 0.5 0) point)


gameMapOffset : Float
gameMapOffset =
    (List.length gameMap |> toFloat) / 2


toGameMapTile : GameMapTile -> String -> GameMapTile
toGameMapTile defaultTile tileStr =
    case tileStr of
        "G" ->
            GrassTile

        "R" ->
            RoadTile

        _ ->
            defaultTile
