module Inventory exposing
    ( GroundItem
    , Inventory
    , Item(..)
    , dropItem
    , fromItems
    , groundItemDisappearTime
    , init
    , itemToString
    , pickUpItem
    , toItems
    , viewGroundItem
    , viewInventory
    )

import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Length exposing (Meters)
import List.Extra as List
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Material as Material
import Vector3d


type alias Location =
    Point3d Meters Meters


type Item
    = Coins Int
    | BronzeDagger


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
                    appendItem groundItem.item (Inventory items)

                Just coinsIndex ->
                    Inventory (List.updateAt coinsIndex (addCoins amount) items)

        BronzeDagger ->
            appendItem groundItem.item (Inventory items)


appendItem : Item -> Inventory -> Inventory
appendItem item (Inventory items) =
    if List.length items >= maxInventoryItems then
        Inventory items

    else
        Inventory (items ++ [ item ])


viewInventory : Inventory -> Html msg
viewInventory (Inventory items) =
    let
        paddedItems : List (Maybe Item)
        paddedItems =
            List.take maxInventoryItems
                (List.map Just items ++ List.repeat maxInventoryItems Nothing)
    in
    div [ class "inventory" ] (List.map viewItem paddedItems)


viewItem : Maybe Item -> Html msg
viewItem maybeItem =
    div [ class "item-box" ] <|
        case maybeItem of
            Nothing ->
                []

            Just item ->
                [ text (itemToString item) ]


itemToString : Item -> String
itemToString item =
    case item of
        Coins amount ->
            "Coins (" ++ String.fromInt amount ++ ")"

        BronzeDagger ->
            "Bronze dagger"


viewGroundItem : GroundItem -> Scene3d.Entity Meters
viewGroundItem groundItem =
    let
        color =
            case groundItem.item of
                Coins _ ->
                    Color.yellow

                BronzeDagger ->
                    Color.darkBrown
    in
    Scene3d.quad (Material.color color)
        (Point3d.translateBy (Vector3d.meters -0.5 -0.5 0) groundItem.location)
        (Point3d.translateBy (Vector3d.meters 0.5 -0.5 0) groundItem.location)
        (Point3d.translateBy (Vector3d.meters 0.5 0.5 0) groundItem.location)
        (Point3d.translateBy (Vector3d.meters -0.5 0.5 0) groundItem.location)


isCoins : Item -> Bool
isCoins item =
    case item of
        Coins _ ->
            True

        _ ->
            False


addCoins : Int -> Item -> Item
addCoins addAmount item =
    case item of
        Coins amount ->
            Coins (amount + addAmount)

        _ ->
            item


maxInventoryItems : Int
maxInventoryItems =
    28


groundItemDisappearTime : Int
groundItemDisappearTime =
    -- Ground items disappear after 20 seconds
    20000
