module MainTests exposing (..)

import Angle
import Color
import Expect
import Inventory
import Length exposing (Meters)
import Main
import Monster
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection
import Set
import Test exposing (Test, describe, test)


goblin : Monster.AliveMonsterState
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


goblin2 : Monster.AliveMonsterState
goblin2 =
    { goblin
        | id = 1
        , respawnLocation = Point3d.fromMeters { x = -3, y = -3, z = 0 }
        , location = Point3d.fromMeters { x = -3, y = -3, z = 0 }
    }


goblin3 : Monster.AliveMonsterState
goblin3 =
    { goblin
        | id = 2
        , respawnLocation = Point3d.fromMeters { x = 3, y = 3, z = 0 }
        , location = Point3d.fromMeters { x = 3, y = 3, z = 0 }
    }


goblin4 : Monster.AliveMonsterState
goblin4 =
    { goblin
        | id = 3
        , respawnLocation = Point3d.fromMeters { x = 3, y = -3, z = 0 }
        , location = Point3d.fromMeters { x = 3, y = -3, z = 0 }
    }


deadGoblin : Monster.DeadMonsterState
deadGoblin =
    { id = 4
    , name = goblin.name
    , color = goblin.color
    , respawnLocation = goblin.respawnLocation
    , maxHealth = goblin.maxHealth
    , respawnAt = 100
    }


allGoblins =
    [ Monster.AliveMonster goblin
    , Monster.AliveMonster goblin2
    , Monster.AliveMonster goblin3
    , Monster.AliveMonster goblin4
    ]


init : Test
init =
    describe "init"
        [ test "Gives an initial model with no side effects" <|
            \_ ->
                Expect.equal (Main.init ())
                    ( { location = Point3d.meters 0 0 0
                      , appearance = Main.Standing
                      , sidePanel = Main.InventoryPanel
                      , health = 10
                      , maxHealth = 10
                      , hits = []
                      , travelPath = []
                      , cameraAngle = Angle.turns 0
                      , keysDown = Set.empty
                      , monsters =
                            [ Monster.AliveMonster goblin
                            , Monster.AliveMonster goblin2
                            , Monster.AliveMonster goblin3
                            , Monster.AliveMonster goblin4
                            ]
                      , now = -1
                      , attackStyle = Main.AccuracyStyle
                      , accuracyXp = 0
                      , strengthXp = 0
                      , defenseXp = 0
                      , inventory = Inventory.init
                      , groundItems = []
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
                            [ Point3d.fromMeters { x = 0, y = 0, z = 0 }
                            , Point3d.fromMeters { x = -1, y = 0, z = 0 }
                            , Point3d.fromMeters { x = -2, y = -1, z = 0 }
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
                            [ Point3d.fromMeters { x = 0, y = 0, z = 0 }
                            , Point3d.fromMeters { x = 0, y = -1, z = 0 }
                            , Point3d.fromMeters { x = -1, y = -2, z = 0 }
                            , Point3d.fromMeters { x = -2, y = -3, z = 0 }
                            ]
                        , appearance = Main.Attacking goblin2
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
                    ( { modelRightNextToMonster | appearance = Main.Attacking goblin2, travelPath = [ Point3d.fromMeters { x = -3, y = -2, z = 0 } ] }
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
                        | travelPath = [ Point3d.fromMeters { x = -3, y = -3, z = 0 }, Point3d.fromMeters { x = -2, y = -3, z = 0 } ]
                        , appearance = Main.Attacking goblin2
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
                            , Point3d.fromMeters { x = 0, y = 0, z = 0 }
                            , Point3d.fromMeters { x = 0, y = -1, z = 0 }
                            , Point3d.fromMeters { x = -1, y = -2, z = 0 }
                            , Point3d.fromMeters { x = -2, y = -3, z = 0 }
                            ]
                        , appearance = Main.Attacking goblin2
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
                            , appearance = Main.Attacking attackingMonster
                            , now = 0
                        }

                    attackingMonster =
                        { goblin | id = 1, location = Point3d.fromMeters { x = -3, y = -3, z = 0 } }
                in
                Expect.equal (Main.update (Main.AnimationFrame 0) attackingModel |> Tuple.first)
                    { attackingModel | appearance = Main.Fighting attackingMonster }
        , test "Nothing changes when state is Fighting" <|
            \_ ->
                let
                    attackingModel =
                        { initialModel
                            | location = Point3d.fromMeters { x = -3, y = -2, z = 0 }
                            , appearance = Main.Fighting attackingMonster
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
                        { initialModel | monsters = [ Monster.AliveMonster travelingGoblin ] }

                    travelingGoblin =
                        { goblin | travelPath = [ Point3d.meters 0 3 0 ] }
                in
                Expect.equal (Main.update (Main.AnimationFrame 0) modelWithMonsterTravelPath |> Tuple.first)
                    { modelWithMonsterTravelPath
                        | now = 0
                        , monsters = [ Monster.AliveMonster { travelingGoblin | location = Point3d.meters -2.95 3 0 } ]
                    }
        ]
