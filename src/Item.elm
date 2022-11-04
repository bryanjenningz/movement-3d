module Item exposing (GroundItem, Item(..))

import Length exposing (Meters)
import Point3d exposing (Point3d)


type Item
    = Coins Int


type alias GroundItem =
    { item : Item
    , location : Point3d Meters Meters
    }
