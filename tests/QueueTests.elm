module QueueTests exposing (..)

import Expect
import Queue exposing (add, empty, fromList, remove, toList)
import Test exposing (Test, describe, test)


queueTests : Test
queueTests =
    describe "Queue"
        [ test "Add, remove, toList, fromList, empty work" <|
            \_ ->
                Expect.equalLists
                    [ empty |> toList
                    , fromList [] |> toList
                    , empty |> remove |> Tuple.second |> toList
                    , fromList [ 1, 2, 3 ] |> toList
                    , empty |> add 1 |> add 2 |> add 3 |> remove |> Tuple.second |> toList
                    , fromList [ 1, 2, 3 ] |> remove |> Tuple.second |> remove |> Tuple.second |> toList
                    ]
                    [ []
                    , []
                    , []
                    , [ 1, 2, 3 ]
                    , [ 2, 3 ]
                    , [ 3 ]
                    ]
        , test "Removes the oldest element and gives the new queue" <|
            \_ ->
                Expect.equalLists
                    [ fromList [ 1, 2, 3 ] |> remove
                    , fromList [ 1, 2, 3 ] |> remove |> Tuple.second |> remove
                    , fromList [ 1, 2, 3 ] |> remove |> Tuple.second |> remove |> Tuple.second |> remove
                    , empty |> add 3 |> add 1 |> remove
                    ]
                    [ ( Just 1, fromList [ 2, 3 ] )
                    , ( Just 2, fromList [ 3 ] )
                    , ( Just 3, fromList [] )
                    , ( Just 3, fromList [ 1 ] )
                    ]
        , test "Adds newest and removes oldest" <|
            \_ ->
                Expect.equal
                    (empty |> add 3 |> add 1 |> remove |> Tuple.second |> add 2 |> remove |> Tuple.second |> add 5 |> toList)
                    [ 2, 5 ]
        ]
