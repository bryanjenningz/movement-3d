module GameMapTests exposing (..)

import Expect
import GameMap exposing (unwalkableEdges)
import Test exposing (Test, describe, test)


unwalkableEdgesTests : Test
unwalkableEdgesTests =
    describe "unwalkableEdges"
        [ test "Creates a list of unwalkable edges" <|
            \_ ->
                Expect.equalLists
                    [ unwalkableEdges [] ]
                    [ [] ]
        ]
