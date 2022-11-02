module MonsterTests exposing (..)

import Expect
import MainTests exposing (allGoblins, deadGoblin, goblin, goblin2, goblin3, goblin4)
import Monster
import Point3d
import Test exposing (Test, describe, test)


shortestPath : Test
shortestPath =
    describe "shortestPath"
        [ test "Gives an empty path if the start and end points are the same" <|
            \_ ->
                Expect.equalLists
                    [ Monster.shortestPath (Point3d.meters 0 0 0) (Point3d.meters 0 0 0)
                    , Monster.shortestPath (Point3d.meters 1.1 0 0) (Point3d.meters 1.1 0 0)
                    , Monster.shortestPath (Point3d.meters 1.1 2.2 0) (Point3d.meters 1.1 2.2 0)
                    , Monster.shortestPath (Point3d.meters -1.1 -2.2 0) (Point3d.meters -1.1 -2.2 0)
                    ]
                    [ [], [], [], [] ]
        , test "Gives the shortest path between 2 points 1 space away" <|
            \_ ->
                Expect.equalLists
                    [ Monster.shortestPath (Point3d.meters 0 0 0) (Point3d.meters 0 1 0)
                    , Monster.shortestPath (Point3d.meters 1.1 0 0) (Point3d.meters 1.1 1 0)
                    , Monster.shortestPath (Point3d.meters 1.1 2.2 0) (Point3d.meters 1.1 3.2 0)
                    , Monster.shortestPath (Point3d.meters -1.1 -2.2 0) (Point3d.meters -1.1 -1.2 0)
                    ]
                    [ [ Point3d.fromMeters { x = 0, y = 1, z = 0 } ]
                    , [ Point3d.fromMeters { x = 1.1, y = 1, z = 0 } ]
                    , [ Point3d.fromMeters { x = 1.1, y = 3.2, z = 0 } ]
                    , [ Point3d.fromMeters { x = -1.1, y = -1.2000000000000002, z = 0 }
                      , Point3d.fromMeters { x = -1.1, y = -1.2, z = 0 }
                      ]
                    ]
        , test "Gives the shortest path between 2 points 1 diagonal space away" <|
            \_ ->
                Expect.equalLists
                    [ Monster.shortestPath (Point3d.meters 0 0 0) (Point3d.meters 1 1 0)
                    , Monster.shortestPath (Point3d.meters 1.1 0 0) (Point3d.meters 2.1 1 0)
                    , Monster.shortestPath (Point3d.meters 1.1 2.2 0) (Point3d.meters 2.1 3.2 0)
                    , Monster.shortestPath (Point3d.meters -1.1 -2.2 0) (Point3d.meters -0.1 -1.2 0)
                    ]
                    [ [ Point3d.fromMeters { x = 1, y = 1, z = 0 } ]
                    , [ Point3d.fromMeters { x = 2.1, y = 1, z = 0 } ]
                    , [ Point3d.fromMeters { x = 2.1, y = 3.2, z = 0 } ]
                    , [ Point3d.fromMeters { x = -0.10000000000000009, y = -1.2000000000000002, z = 0 }
                      , Point3d.fromMeters { x = -0.1, y = -1.2, z = 0 }
                      ]
                    ]
        , test "Gives the shortest path between 2 points less than 1 space away" <|
            \_ ->
                Expect.equalLists
                    [ Monster.shortestPath (Point3d.meters 0 0 0) (Point3d.meters 0 0.5 0)
                    , Monster.shortestPath (Point3d.meters 1.1 0 0) (Point3d.meters 1.1 0.3 0)
                    , Monster.shortestPath (Point3d.meters 1.1 2.2 0) (Point3d.meters 1 2.3 0)
                    , Monster.shortestPath (Point3d.meters -1.1 -2.2 0) (Point3d.meters -1.2 -2 0)
                    ]
                    [ [ Point3d.fromMeters { x = 0, y = 0.5, z = 0 } ]
                    , [ Point3d.fromMeters { x = 1.1, y = 0.3, z = 0 } ]
                    , [ Point3d.fromMeters { x = 1, y = 2.3, z = 0 } ]
                    , [ Point3d.fromMeters { x = -1.2, y = -2, z = 0 } ]
                    ]
        ]


findAliveMonster : Test
findAliveMonster =
    describe "findAliveMonster"
        [ test "Finds the alive monster with the id you pass in" <|
            \_ ->
                Expect.equalLists
                    [ Monster.findAliveMonster 0 []
                    , Monster.findAliveMonster 0 allGoblins
                    , Monster.findAliveMonster 1 allGoblins
                    , Monster.findAliveMonster 2 allGoblins
                    , Monster.findAliveMonster 3 allGoblins
                    , Monster.findAliveMonster 4 allGoblins
                    , Monster.findAliveMonster 0 (List.reverse allGoblins)
                    ]
                    [ Nothing, Just goblin, Just goblin2, Just goblin3, Just goblin4, Nothing, Just goblin ]
        ]


updateAliveMonster : Test
updateAliveMonster =
    describe "updateAliveMonster"
        [ test "Updates the alive monster with the id you pass in" <|
            \_ ->
                let
                    decrementHealth : Monster.AliveMonsterState -> Monster.Monster
                    decrementHealth monster =
                        Monster.AliveMonster { monster | health = monster.health - 1 }
                in
                Expect.equalLists
                    [ Monster.updateAliveMonster 0 decrementHealth []
                    , Monster.updateAliveMonster 5 decrementHealth allGoblins
                    , Monster.updateAliveMonster 0 decrementHealth allGoblins
                    , Monster.updateAliveMonster 0 decrementHealth (List.reverse allGoblins)
                    ]
                    [ []
                    , allGoblins
                    , [ decrementHealth goblin ] ++ List.drop 1 allGoblins
                    , List.take 3 (List.reverse allGoblins) ++ [ decrementHealth goblin ]
                    ]
        ]


respawnMonster : Test
respawnMonster =
    describe "respawnMonster"
        [ test "Respawns monster if the respawn time has passed" <|
            \_ ->
                Expect.equalLists
                    [ Monster.respawnMonster 99 deadGoblin
                    , Monster.respawnMonster 100 deadGoblin
                    , Monster.respawnMonster 1000 deadGoblin
                    ]
                    [ Monster.DeadMonster deadGoblin
                    , Monster.AliveMonster { goblin | id = 4 }
                    , Monster.AliveMonster { goblin | id = 4 }
                    ]
        ]


xyRange : Test
xyRange =
    describe "xyRange"
        [ test "Gives a range of xy tuples based on the low and high values you pass in" <|
            \_ ->
                Expect.equal (Monster.xyRange -1 1)
                    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
        ]


weightedXyRange : Test
weightedXyRange =
    describe "weightedXyRange"
        [ test "Gives a range of weighted xy tuples" <|
            \_ ->
                Expect.equal (Monster.weightedXyRange -1 1)
                    [ ( 1, ( -1, -1 ) )
                    , ( 1, ( 0, -1 ) )
                    , ( 1, ( 1, -1 ) )
                    , ( 1, ( -1, 0 ) )
                    , ( 1, ( 0, 0 ) )
                    , ( 1, ( 1, 0 ) )
                    , ( 1, ( -1, 1 ) )
                    , ( 1, ( 0, 1 ) )
                    , ( 1, ( 1, 1 ) )
                    ]
        ]


pointLocation : Test
pointLocation =
    describe "pointLocation"
        [ test "Converts a 2d point into a 3d location" <|
            \_ ->
                Expect.equal (Monster.pointLocation ( 1, 2 )) (Point3d.meters 1 2 0)
        ]
