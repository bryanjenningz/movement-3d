module Item exposing (GroundItem, Item(..), dropItem, pickUpItem)

import Length exposing (Meters)
import Point3d exposing (Point3d)


type alias Location =
    Point3d Meters Meters


type Item
    = Coins Int


type alias GroundItem =
    { item : Item
    , location : Location
    , disappearsAt : Int
    }


dropItem : Location -> Int -> Item -> GroundItem
dropItem location time item =
    { item = item
    , location = location
    , disappearsAt = time + groundItemDisappearTime
    }


pickUpItem : GroundItem -> Item
pickUpItem groundItem =
    groundItem.item


groundItemDisappearTime : Int
groundItemDisappearTime =
    -- Ground items disappear after 20 seconds
    20000
