module InventoryTests exposing (..)

import Expect
import Inventory exposing (GroundItem, Item(..), dropItem, fromItems, init, pickUpItem, toItems)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Test exposing (Test, describe, test)


groundItemLocation : Point3d Meters Meters
groundItemLocation =
    Point3d.meters 0 0 0


groundCoins : GroundItem
groundCoins =
    { item = Coins 1, location = groundItemLocation, disappearsAt = 20000 }


groundBronzeDagger : GroundItem
groundBronzeDagger =
    { item = BronzeDagger, location = groundItemLocation, disappearsAt = 20000 }


dropItemTests : Test
dropItemTests =
    describe "dropItem"
        [ test "Drops an item from the inventory" <|
            \_ ->
                Expect.equalLists
                    [ dropItem groundItemLocation 0 0 (fromItems [ Coins 1 ]) ]
                    [ ( Just groundCoins, init ) ]
        ]


pickUpItemTests : Test
pickUpItemTests =
    describe "pickUpItem"
        [ test "Adds an item to the inventory if there's space" <|
            \_ ->
                Expect.equalLists
                    [ init |> pickUpItem groundCoins |> toItems
                    , init |> pickUpItem groundCoins |> pickUpItem groundCoins |> toItems
                    , init |> pickUpItem groundCoins |> pickUpItem groundBronzeDagger |> pickUpItem groundCoins |> toItems
                    , init |> pickUpItem groundBronzeDagger |> pickUpItem groundCoins |> pickUpItem groundBronzeDagger |> toItems
                    ]
                    [ [ Coins 1 ]
                    , [ Coins 2 ]
                    , [ Coins 2, BronzeDagger ]
                    , [ BronzeDagger, Coins 1, BronzeDagger ]
                    ]
        ]
