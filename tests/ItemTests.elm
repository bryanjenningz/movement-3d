module ItemTests exposing (..)

import Expect
import Item exposing (Item(..), dropItem)
import Point3d
import Test exposing (Test, describe, test)


dropItemTests : Test
dropItemTests =
    describe "dropItem"
        [ test "Creates a ground item" <|
            \_ ->
                Expect.equalLists
                    [ dropItem (Point3d.meters 0 0 0) 0 (Coins 1) ]
                    [ { item = Coins 1, location = Point3d.meters 0 0 0, disappearsAt = 20000 } ]
        ]
