module InventoryTests exposing (..)

import Expect
import Inventory exposing (GroundItem, Item(..), dropItem, fromItems, init, pickUpItem, toItems)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Test exposing (Test, describe, test)


groundItemLocation : Point3d Meters Meters
groundItemLocation =
    Point3d.meters 0 0 0


groundItem : GroundItem
groundItem =
    { item = Coins 1, location = groundItemLocation, disappearsAt = 20000 }


dropItemTests : Test
dropItemTests =
    describe "dropItem"
        [ test "Drops an item from the inventory" <|
            \_ ->
                Expect.equalLists
                    [ dropItem groundItemLocation 0 0 (fromItems [ Coins 1 ]) ]
                    [ ( Just groundItem, init ) ]
        ]


pickUpItemTests : Test
pickUpItemTests =
    describe "pickUpItem"
        [ test "Adds an item to the inventory if there's space" <|
            \_ ->
                Expect.equalLists
                    [ init |> pickUpItem groundItem |> toItems
                    , init |> pickUpItem groundItem |> pickUpItem groundItem |> toItems
                    ]
                    [ [ Coins 1 ]
                    , [ Coins 2 ]
                    ]
        ]
