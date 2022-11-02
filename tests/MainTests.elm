module MainTests exposing (..)

import Angle
import Color
import Expect
import Length exposing (Meters)
import Main
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection
import Set
import Test exposing (Test, describe, test)


goblin =
    { health = 3
    , id = 0
    , respawnLocation = Point3d.fromMeters { x = -3, y = 3, z = 0 }
    , location = Point3d.fromMeters { x = -3, y = 3, z = 0 }
    , maxHealth = 3
    , hits = []
    , name = "Goblin (level 2)"
    , color = Color.darkPurple
    , travelPath = []
    }


goblin2 =
    { goblin
        | id = 1
        , respawnLocation = Point3d.fromMeters { x = -3, y = -3, z = 0 }
        , location = Point3d.fromMeters { x = -3, y = -3, z = 0 }
    }


goblin3 =
    { goblin
        | id = 2
        , respawnLocation = Point3d.fromMeters { x = 3, y = 3, z = 0 }
        , location = Point3d.fromMeters { x = 3, y = 3, z = 0 }
    }


goblin4 =
    { goblin
        | id = 3
        , respawnLocation = Point3d.fromMeters { x = 3, y = -3, z = 0 }
        , location = Point3d.fromMeters { x = 3, y = -3, z = 0 }
    }


allGoblins =
    [ Main.AliveMonster goblin
    , Main.AliveMonster goblin2
    , Main.AliveMonster goblin3
    , Main.AliveMonster goblin4
    ]


init : Test
init =
    describe "init"
        [ test "Gives an initial model with no side effects" <|
            \_ ->
                Expect.equal (Main.init ())
                    ( { location = Point3d.meters 0 0 0
                      , appearance = Main.Standing
                      , health = 10
                      , maxHealth = 10
                      , hits = []
                      , travelPath = []
                      , cameraAngle = Angle.turns 0
                      , keysDown = Set.empty
                      , monsters =
                            [ Main.AliveMonster goblin
                            , Main.AliveMonster goblin2
                            , Main.AliveMonster goblin3
                            , Main.AliveMonster goblin4
                            ]
                      , now = -1
                      , attackStyle = Main.AccuracyStyle
                      , accuracyXp = 0
                      , strengthXp = 0
                      , defenseXp = 0
                      }
                    , Cmd.none
                    )
        ]


initialModel : Main.Model
initialModel =
    Main.init () |> Tuple.first


toMousePoint : Main.Model -> Point3d Meters Meters -> Point2d Pixels Meters
toMousePoint model point =
    Point3d.Projection.toScreenSpace (Main.getCamera model) Main.screen point


mouseDown : Test
mouseDown =
    describe "MouseDown msg"
        [ test "Gives a travelPath when you click on the ground" <|
            \_ ->
                Expect.equal (Main.update (Main.MouseDown (Point2d.pixels 300 200)) initialModel)
                    ( { initialModel
                        | travelPath =
                            [ Point3d.fromMeters { x = -1, y = -1, z = 0 }
                            , Point3d.fromMeters { x = -2, y = -2, z = 0 }
                            , Point3d.fromMeters { x = -3, y = -2, z = 0 }
                            ]
                      }
                    , Cmd.none
                    )
        , test "Always go to the nearest tile" <|
            \_ ->
                let
                    startModel =
                        { initialModel | location = startLocation }

                    startLocation =
                        Point3d.fromMeters { x = 0.4, y = 0.4, z = 0 }
                in
                Expect.equal (Main.update (Main.MouseDown (toMousePoint startModel startLocation)) startModel)
                    ( { startModel | travelPath = [ Point3d.fromMeters { x = 0, y = 0, z = 0 } ] }
                    , Cmd.none
                    )
        , test "Gives a travelPath and Attacking state when you click on a monster" <|
            \_ ->
                Expect.equal (Main.update (Main.MouseDown (Point2d.pixels 200 200)) initialModel)
                    ( { initialModel
                        | travelPath =
                            [ Point3d.fromMeters { x = -1, y = -1, z = 0 }
                            , Point3d.fromMeters { x = -2, y = -2, z = 0 }
                            , Point3d.fromMeters { x = -2, y = -3, z = 0 }
                            ]
                        , appearance = Main.Attacking (Main.AliveMonster goblin2)
                      }
                    , Cmd.none
                    )
        , test "Gives an Attacking state when you click on a monster right next to you" <|
            \_ ->
                let
                    modelRightNextToMonster =
                        { initialModel | location = Point3d.fromMeters { x = -3, y = -2, z = 0 } }

                    mousePoint =
                        toMousePoint modelRightNextToMonster (Point3d.fromMeters { x = -3, y = -3, z = 0 })
                in
                Expect.equal (Main.update (Main.MouseDown mousePoint) modelRightNextToMonster)
                    ( { modelRightNextToMonster | appearance = Main.Attacking (Main.AliveMonster goblin2) }
                    , Cmd.none
                    )
        , test "Moves player to the monster's side if the player attacks while on top of the monster" <|
            \_ ->
                let
                    modelOnTopOfMonster =
                        { initialModel | location = Point3d.fromMeters { x = -3, y = -3, z = 0 } }

                    mousePoint =
                        toMousePoint modelOnTopOfMonster (Point3d.fromMeters { x = -3, y = -3, z = 0 })
                in
                Expect.equal (Main.update (Main.MouseDown mousePoint) modelOnTopOfMonster)
                    ( { modelOnTopOfMonster
                        | travelPath = [ Point3d.fromMeters { x = -2, y = -3, z = 0 } ]
                        , appearance = Main.Attacking (Main.AliveMonster goblin2)
                      }
                    , Cmd.none
                    )
        , test "If the player is walking, then you click, it should continue the next step and use that as the start of the next path" <|
            \_ ->
                let
                    startModel =
                        { initialModel
                            | travelPath =
                                [ Point3d.fromMeters { x = 0, y = 1, z = 0 }
                                , Point3d.fromMeters { x = 0, y = 2, z = 0 }
                                ]
                        }

                    mousePoint =
                        toMousePoint startModel (Point3d.fromMeters { x = -3, y = -3, z = 0 })
                in
                Expect.equal (Main.update (Main.MouseDown mousePoint) startModel)
                    ( { startModel
                        | travelPath =
                            [ Point3d.fromMeters { x = 0, y = 1, z = 0 }
                            , Point3d.fromMeters { x = -1, y = 0, z = 0 }
                            , Point3d.fromMeters { x = -2, y = -1, z = 0 }
                            , Point3d.fromMeters { x = -2, y = -2, z = 0 }
                            , Point3d.fromMeters { x = -2, y = -3, z = 0 }
                            ]
                        , appearance = Main.Attacking (Main.AliveMonster goblin2)
                      }
                    , Cmd.none
                    )
        ]


animationFrame : Test
animationFrame =
    describe "AnimationFrame msg"
        [ test "Switches state to Fighting when the state is Attacking and travelPath is empty" <|
            \_ ->
                let
                    attackingModel =
                        { initialModel
                            | location = Point3d.fromMeters { x = -3, y = -2, z = 0 }
                            , appearance = Main.Attacking (Main.AliveMonster attackingMonster)
                            , now = 0
                        }

                    attackingMonster =
                        { goblin | id = 1, location = Point3d.fromMeters { x = -3, y = -3, z = 0 } }
                in
                Expect.equal (Main.update (Main.AnimationFrame 0) attackingModel |> Tuple.first)
                    { attackingModel | appearance = Main.Fighting (Main.AliveMonster attackingMonster) }
        , test "Nothing changes when state is Fighting" <|
            \_ ->
                let
                    attackingModel =
                        { initialModel
                            | location = Point3d.fromMeters { x = -3, y = -2, z = 0 }
                            , appearance = Main.Fighting (Main.AliveMonster attackingMonster)
                            , now = 0
                        }

                    attackingMonster =
                        { goblin | id = 1, location = Point3d.fromMeters { x = -3, y = -3, z = 0 } }
                in
                Expect.equal (Main.update (Main.AnimationFrame 0) attackingModel |> Tuple.first)
                    attackingModel
        , test "Monster moves if it has a travelPath" <|
            \_ ->
                let
                    modelWithMonsterTravelPath =
                        { initialModel | monsters = [ Main.AliveMonster travelingGoblin ] }

                    travelingGoblin =
                        { goblin | travelPath = [ Point3d.meters 0 3 0 ] }
                in
                Expect.equal (Main.update (Main.AnimationFrame 0) modelWithMonsterTravelPath |> Tuple.first)
                    { modelWithMonsterTravelPath
                        | now = 0
                        , monsters = [ Main.AliveMonster { travelingGoblin | location = Point3d.meters -2.95 3 0 } ]
                    }
        ]


shortestPath : Test
shortestPath =
    describe "shortestPath"
        [ test "Gives an empty path if the start and end points are the same" <|
            \_ ->
                Expect.equalLists
                    [ Main.shortestPath (Point3d.meters 0 0 0) (Point3d.meters 0 0 0)
                    , Main.shortestPath (Point3d.meters 1.1 0 0) (Point3d.meters 1.1 0 0)
                    , Main.shortestPath (Point3d.meters 1.1 2.2 0) (Point3d.meters 1.1 2.2 0)
                    , Main.shortestPath (Point3d.meters -1.1 -2.2 0) (Point3d.meters -1.1 -2.2 0)
                    ]
                    [ [], [], [], [] ]
        , test "Gives the shortest path between 2 points 1 space away" <|
            \_ ->
                Expect.equalLists
                    [ Main.shortestPath (Point3d.meters 0 0 0) (Point3d.meters 0 1 0)
                    , Main.shortestPath (Point3d.meters 1.1 0 0) (Point3d.meters 1.1 1 0)
                    , Main.shortestPath (Point3d.meters 1.1 2.2 0) (Point3d.meters 1.1 3.2 0)
                    , Main.shortestPath (Point3d.meters -1.1 -2.2 0) (Point3d.meters -1.1 -1.2 0)
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
                    [ Main.shortestPath (Point3d.meters 0 0 0) (Point3d.meters 1 1 0)
                    , Main.shortestPath (Point3d.meters 1.1 0 0) (Point3d.meters 2.1 1 0)
                    , Main.shortestPath (Point3d.meters 1.1 2.2 0) (Point3d.meters 2.1 3.2 0)
                    , Main.shortestPath (Point3d.meters -1.1 -2.2 0) (Point3d.meters -0.1 -1.2 0)
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
                    [ Main.shortestPath (Point3d.meters 0 0 0) (Point3d.meters 0 0.5 0)
                    , Main.shortestPath (Point3d.meters 1.1 0 0) (Point3d.meters 1.1 0.3 0)
                    , Main.shortestPath (Point3d.meters 1.1 2.2 0) (Point3d.meters 1 2.3 0)
                    , Main.shortestPath (Point3d.meters -1.1 -2.2 0) (Point3d.meters -1.2 -2 0)
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
                    [ Main.findAliveMonster 0 []
                    , Main.findAliveMonster 0 allGoblins
                    , Main.findAliveMonster 1 allGoblins
                    , Main.findAliveMonster 2 allGoblins
                    , Main.findAliveMonster 3 allGoblins
                    , Main.findAliveMonster 4 allGoblins
                    , Main.findAliveMonster 0 (List.reverse allGoblins)
                    ]
                    [ Nothing, Just goblin, Just goblin2, Just goblin3, Just goblin4, Nothing, Just goblin ]
        ]


updateAliveMonster : Test
updateAliveMonster =
    describe "updateAliveMonster"
        [ test "Updates the alive monster with the id you pass in" <|
            \_ ->
                let
                    decrementHealth : Main.AliveMonsterState -> Main.Monster
                    decrementHealth monster =
                        Main.AliveMonster { monster | health = monster.health - 1 }
                in
                Expect.equalLists
                    [ Main.updateAliveMonster 0 decrementHealth []
                    , Main.updateAliveMonster 5 decrementHealth allGoblins
                    , Main.updateAliveMonster 0 decrementHealth allGoblins
                    , Main.updateAliveMonster 0 decrementHealth (List.reverse allGoblins)
                    ]
                    [ []
                    , allGoblins
                    , [ decrementHealth goblin ] ++ List.drop 1 allGoblins
                    , List.take 3 (List.reverse allGoblins) ++ [ decrementHealth goblin ]
                    ]
        ]


xyRange : Test
xyRange =
    describe "xyRange"
        [ test "Gives a range of xy tuples based on the low and high values you pass in" <|
            \_ ->
                Expect.equal (Main.xyRange -1 1)
                    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 0, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
        ]


weightedXyRange : Test
weightedXyRange =
    describe "weightedXyRange"
        [ test "Gives a range of weighted xy tuples" <|
            \_ ->
                Expect.equal (Main.weightedXyRange -1 1)
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
                Expect.equal (Main.pointLocation ( 1, 2 )) (Point3d.meters 1 2 0)
        ]
