module GameMapTests exposing (..)

import Expect
import GameMap exposing (Obstacle(..), unwalkableEdges)
import Test exposing (Test, describe, test)


unwalkableEdgesTests : Test
unwalkableEdgesTests =
    describe "unwalkableEdges"
        [ test "Creates a list of unwalkable edges" <|
            \_ ->
                Expect.equalLists
                    [ unwalkableEdges []

                    -- Wall between ( 0, 0 ) and ( 0, 1 )
                    , unwalkableEdges [ HorizontalWall ( 0, 0 ) 1 ]
                    , unwalkableEdges [ HorizontalWall ( 0, 0 ) 2 ]

                    -- Wall between ( 0, 0 ) and ( -1, 0 )
                    , unwalkableEdges [ VerticalWall ( 0, 0 ) 1 ]
                    , unwalkableEdges [ VerticalWall ( 0, 0 ) 2 ]
                    ]
                    [ []
                    , [ ( ( 0, 0 ), ( 0, 1 ) ) ]
                    , [ ( ( 0, 0 ), ( 0, 1 ) ), ( ( 1, 0 ), ( 1, 1 ) ) ]
                    , [ ( ( -1, 0 ), ( 0, 0 ) ) ]
                    , [ ( ( -1, -1 ), ( 0, -1 ) ), ( ( -1, 0 ), ( 0, 0 ) ) ]
                    ]
        ]
