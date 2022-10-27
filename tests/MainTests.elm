module MainTests exposing (..)

import Angle
import Expect
import Length exposing (Meters)
import Main
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection
import Set
import Test exposing (Test, describe, test)


init : Test
init =
    describe "init"
        [ test "Gives an initial model with no side effects" <|
            \_ ->
                Expect.equal (Main.init ())
                    ( { location = Point3d.meters 0 0 0
                      , state = Main.Standing
                      , health = 10
                      , maxHealth = 10
                      , travelPath = []
                      , cameraAngle = Angle.turns 0
                      , keysDown = Set.empty
                      , monsters =
                            [ { health = 3, id = 0, location = Point3d.fromMeters { x = -3, y = 3, z = 0 }, maxHealth = 3 }
                            , { health = 3, id = 1, location = Point3d.fromMeters { x = -3, y = -3, z = 0 }, maxHealth = 3 }
                            , { health = 3, id = 2, location = Point3d.fromMeters { x = 3, y = 3, z = 0 }, maxHealth = 3 }
                            , { health = 3, id = 3, location = Point3d.fromMeters { x = 3, y = -3, z = 0 }, maxHealth = 3 }
                            ]
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
        , test "Gives a travelPath and Attacking state when you click on a monster" <|
            \_ ->
                Expect.equal (Main.update (Main.MouseDown (Point2d.pixels 200 200)) initialModel)
                    ( { initialModel
                        | travelPath =
                            [ Point3d.fromMeters { x = -1, y = -1, z = 0 }
                            , Point3d.fromMeters { x = -2, y = -2, z = 0 }
                            ]
                        , state =
                            Main.Attacking
                                { health = 3, id = 1, location = Point3d.fromMeters { x = -3, y = -3, z = 0 }, maxHealth = 3 }
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
                    ( { modelRightNextToMonster
                        | state =
                            Main.Attacking
                                { health = 3, id = 1, location = Point3d.fromMeters { x = -3, y = -3, z = 0 }, maxHealth = 3 }
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
                            , state = Main.Attacking attackingMonster
                        }

                    attackingMonster =
                        { health = 3, id = 1, location = Point3d.fromMeters { x = -3, y = -3, z = 0 }, maxHealth = 3 }
                in
                Expect.equal (Main.update Main.AnimationFrame attackingModel)
                    ( { attackingModel | state = Main.Fighting attackingMonster }, Cmd.none )
        , test "Nothing changes when state is Fighting" <|
            \_ ->
                let
                    attackingModel =
                        { initialModel
                            | location = Point3d.fromMeters { x = -3, y = -2, z = 0 }
                            , state = Main.Fighting attackingMonster
                        }

                    attackingMonster =
                        { health = 3, id = 1, location = Point3d.fromMeters { x = -3, y = -3, z = 0 }, maxHealth = 3 }
                in
                Expect.equal (Main.update Main.AnimationFrame attackingModel)
                    ( attackingModel, Cmd.none )
        ]
