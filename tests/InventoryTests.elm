module InventoryTests exposing (..)

import Expect
import Inventory exposing (GroundItem, Item(..), dropItem, fromItems, init, pickUpItem, toItems, viewInventory)
import Length exposing (Meters)
import Point3d exposing (Point3d)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, classes)


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
                    [ dropItem groundItemLocation 0 0 (fromItems [ Coins 1 ])
                    , dropItem groundItemLocation 0 1 (fromItems [ Coins 1 ])
                    , dropItem groundItemLocation 0 0 init
                    , dropItem groundItemLocation 0 0 (fromItems [ BronzeDagger, Coins 1, BronzeDagger ])
                    , dropItem groundItemLocation 0 2 (fromItems [ BronzeDagger, Coins 1, BronzeDagger ])
                    ]
                    [ ( Just groundCoins, init )
                    , ( Nothing, fromItems [ Coins 1 ] )
                    , ( Nothing, init )
                    , ( Just groundBronzeDagger, fromItems [ Coins 1, BronzeDagger ] )
                    , ( Just groundBronzeDagger, fromItems [ BronzeDagger, Coins 1 ] )
                    ]
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
                    , fromItems (List.repeat 28 BronzeDagger) |> pickUpItem groundBronzeDagger |> toItems
                    ]
                    [ [ Coins 1 ]
                    , [ Coins 2 ]
                    , [ Coins 2, BronzeDagger ]
                    , [ BronzeDagger, Coins 1, BronzeDagger ]
                    , List.repeat 28 BronzeDagger
                    ]
        ]


viewInventoryTests : Test
viewInventoryTests =
    describe "viewInventory"
        [ test "Shows an inventory" <|
            \_ ->
                viewInventory init
                    |> Query.fromHtml
                    |> Query.has [ class "inventory" ]
        , test "Shows an inventory with 28 item boxes" <|
            \_ ->
                viewInventory init
                    |> Query.fromHtml
                    |> Query.findAll [ class "item-box" ]
                    |> Query.count (Expect.equal 28)
        ]
