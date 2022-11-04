module Inventory exposing (GroundItem, Inventory, Item(..), dropItem, fromItems, init, pickUpItem, toItems)

import Length exposing (Meters)
import List.Extra as List
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


type Inventory
    = Inventory (List Item)


init : Inventory
init =
    Inventory []


fromItems : List Item -> Inventory
fromItems items =
    Inventory items


toItems : Inventory -> List Item
toItems (Inventory items) =
    items


dropItem : Location -> Int -> Int -> Inventory -> ( Maybe GroundItem, Inventory )
dropItem location time itemIndex (Inventory items) =
    case List.getAt itemIndex items of
        Nothing ->
            ( Nothing, Inventory items )

        Just item ->
            ( Just
                { item = item
                , location = location
                , disappearsAt = time + groundItemDisappearTime
                }
            , Inventory (List.removeAt itemIndex items)
            )


pickUpItem : GroundItem -> Inventory -> Inventory
pickUpItem groundItem (Inventory items) =
    case groundItem.item of
        Coins amount ->
            case List.findIndex isCoins items of
                Nothing ->
                    if List.length items >= maxInventoryItems then
                        Inventory items

                    else
                        Inventory (items ++ [ groundItem.item ])

                Just coinsIndex ->
                    Inventory (List.updateAt coinsIndex (addCoins amount) items)


isCoins : Item -> Bool
isCoins item =
    case item of
        Coins _ ->
            True


addCoins : Int -> Item -> Item
addCoins addAmount item =
    case item of
        Coins amount ->
            Coins (amount + addAmount)


maxInventoryItems : Int
maxInventoryItems =
    28


groundItemDisappearTime : Int
groundItemDisappearTime =
    -- Ground items disappear after 20 seconds
    20000
