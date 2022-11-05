module GameMapTests exposing (..)

import Expect
import GameMap exposing (Obstacle(..), shortestPath, unwalkableEdges)
import Set
import Test exposing (Test, describe, test)


unwalkableEdgesTests : Test
unwalkableEdgesTests =
    describe "unwalkableEdges"
        [ test "Creates a list of unwalkable edges" <|
            \_ ->
                Expect.equalLists
                    [ unwalkableEdges [] |> Set.fromList

                    -- Wall between ( 0, 0 ) and ( 0, 1 )
                    , unwalkableEdges [ HorizontalWall ( 0, 0 ) 1 ] |> Set.fromList
                    , unwalkableEdges [ HorizontalWall ( 0, 0 ) 2 ] |> Set.fromList

                    -- Wall between ( 0, 0 ) and ( -1, 0 )
                    , unwalkableEdges [ VerticalWall ( 0, 0 ) 1 ] |> Set.fromList
                    , unwalkableEdges [ VerticalWall ( 0, 0 ) 2 ] |> Set.fromList
                    ]
                    [ Set.fromList []
                    , Set.fromList
                        [ ( ( 0, 0 ), ( -1, 1 ) )
                        , ( ( 0, 0 ), ( 0, 1 ) )
                        , ( ( 0, 0 ), ( 1, 1 ) )
                        , ( ( 0, 1 ), ( -1, 0 ) )
                        , ( ( 0, 1 ), ( 0, 0 ) )
                        , ( ( 0, 1 ), ( 1, 0 ) )
                        ]
                    , Set.fromList
                        [ ( ( 0, 0 ), ( -1, 1 ) )
                        , ( ( 0, 0 ), ( 0, 1 ) )
                        , ( ( 0, 0 ), ( 1, 1 ) )
                        , ( ( 0, 1 ), ( -1, 0 ) )
                        , ( ( 0, 1 ), ( 0, 0 ) )
                        , ( ( 0, 1 ), ( 1, 0 ) )
                        , ( ( 1, 0 ), ( 0, 1 ) )
                        , ( ( 1, 0 ), ( 1, 1 ) )
                        , ( ( 1, 0 ), ( 2, 1 ) )
                        , ( ( 1, 1 ), ( 0, 0 ) )
                        , ( ( 1, 1 ), ( 1, 0 ) )
                        , ( ( 1, 1 ), ( 2, 0 ) )
                        ]
                    , Set.fromList
                        [ ( ( -1, 0 ), ( 0, -1 ) )
                        , ( ( -1, 0 ), ( 0, 0 ) )
                        , ( ( -1, 0 ), ( 0, 1 ) )
                        , ( ( 0, 0 ), ( -1, -1 ) )
                        , ( ( 0, 0 ), ( -1, 0 ) )
                        , ( ( 0, 0 ), ( -1, 1 ) )
                        ]
                    , Set.fromList
                        [ ( ( -1, -1 ), ( 0, -2 ) )
                        , ( ( -1, -1 ), ( 0, -1 ) )
                        , ( ( -1, -1 ), ( 0, 0 ) )
                        , ( ( -1, 0 ), ( 0, -1 ) )
                        , ( ( -1, 0 ), ( 0, 0 ) )
                        , ( ( -1, 0 ), ( 0, 1 ) )
                        , ( ( 0, -1 ), ( -1, -2 ) )
                        , ( ( 0, -1 ), ( -1, -1 ) )
                        , ( ( 0, -1 ), ( -1, 0 ) )
                        , ( ( 0, 0 ), ( -1, -1 ) )
                        , ( ( 0, 0 ), ( -1, 0 ) )
                        , ( ( 0, 0 ), ( -1, 1 ) )
                        ]
                    ]
        ]


shortestPathTests : Test
shortestPathTests =
    describe "shortestPath"
        [ test "Gives the shortest path between 2 points" <|
            \_ ->
                Expect.equalLists
                    [ shortestPath [] ( 0, 0 ) ( 0, 0 )
                    , shortestPath [] ( 0, 0 ) ( 0, 1 )
                    , shortestPath [] ( 0, 0 ) ( 0, 2 )
                    , shortestPath [] ( 0, 0 ) ( 2, 2 )
                    , shortestPath [] ( 0, 0 ) ( 3, 2 )
                    ]
                    [ Just [ ( 0, 0 ) ]
                    , Just [ ( 0, 0 ), ( 0, 1 ) ]
                    , Just [ ( 0, 0 ), ( 0, 1 ), ( 0, 2 ) ]
                    , Just [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ]
                    , Just [ ( 0, 0 ), ( 1, 0 ), ( 2, 1 ), ( 3, 2 ) ]
                    ]
        ]
