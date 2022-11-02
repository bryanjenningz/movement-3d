module Main exposing
    ( Appearance(..)
    , AttackStyle(..)
    , Model
    , Msg(..)
    , getCamera
    , init
    , main
    , screen
    , update
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp, onMouseDown)
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import GameMap
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Length exposing (Meters)
import Monster exposing (AliveMonsterState, Hit, Location, Monster(..), TravelPath)
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Point3d.Projection
import Quantity
import Random
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Material as Material
import Set exposing (Set)
import SketchPlane3d
import Time
import Vector3d exposing (Vector3d)
import Viewpoint3d


type alias Model =
    -- Global state
    { now : Int
    , cameraAngle : Angle
    , keysDown : Set String

    -- Player state
    , location : Location
    , appearance : Appearance
    , health : Int
    , maxHealth : Int
    , hits : List Hit
    , travelPath : TravelPath
    , attackStyle : AttackStyle
    , accuracyXp : Int
    , strengthXp : Int
    , defenseXp : Int

    -- Monster state
    , monsters : List Monster
    }


type Appearance
    = Standing
    | Attacking AliveMonsterState
    | Fighting AliveMonsterState


type AttackStyle
    = AccuracyStyle
    | StrengthStyle
    | DefenseStyle


init : () -> ( Model, Cmd Msg )
init () =
    ( -- Global state
      { now = -1
      , cameraAngle = Angle.turns 0
      , keysDown = Set.empty

      -- Player state
      , location = Point3d.meters 0 0 0
      , appearance = Standing
      , health = 10
      , maxHealth = 10
      , hits = []
      , travelPath = []
      , attackStyle = AccuracyStyle
      , accuracyXp = 0
      , strengthXp = 0
      , defenseXp = 0

      -- Monster state
      , monsters = Monster.init
      }
    , Cmd.none
    )


type Msg
    = AnimationFrame Int
    | MouseDown (Point2d Pixels Meters)
    | KeyDown String
    | KeyUp String
    | GenerateAttackRound
    | AttackRound Int Int
    | SetAttackStyle AttackStyle
    | SetNewMonsterTravelPaths (List (Maybe TravelPath))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame time ->
            ( applyAnimationFrame time model
            , Monster.generateMonsterTravelPaths model.monsters |> Random.generate SetNewMonsterTravelPaths
            )

        MouseDown mousePoint ->
            ( applyMouseDown mousePoint model, Cmd.none )

        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )

        GenerateAttackRound ->
            ( model, generateAttackRound )

        AttackRound playerDamage monsterDamage ->
            ( applyAttackRound playerDamage monsterDamage model, Cmd.none )

        SetAttackStyle attackStyle ->
            ( { model | attackStyle = attackStyle }, Cmd.none )

        SetNewMonsterTravelPaths newMonsterTravelPaths ->
            let
                newMonsters =
                    List.map2
                        (\monster maybeTravelPath ->
                            case ( monster, maybeTravelPath ) of
                                ( AliveMonster aliveMonster, Just travelPath ) ->
                                    case model.appearance of
                                        Fighting fightingMonster ->
                                            if fightingMonster.id == aliveMonster.id then
                                                monster

                                            else
                                                AliveMonster { aliveMonster | travelPath = travelPath }

                                        _ ->
                                            AliveMonster { aliveMonster | travelPath = travelPath }

                                _ ->
                                    monster
                        )
                        model.monsters
                        newMonsterTravelPaths
            in
            ( { model | monsters = newMonsters }, Cmd.none )


updateLocationTravelPath : Location -> TravelPath -> ( Location, TravelPath )
updateLocationTravelPath location travelPath =
    let
        calculateNewLocation : Location -> Location
        calculateNewLocation destination =
            Vector3d.from location destination
                |> (\path ->
                        let
                            minLength =
                                min 0.05 (Vector3d.length path |> Length.inMeters)
                        in
                        Vector3d.scaleTo (Length.meters minLength) path
                   )
                |> Vector3d.plus (Vector3d.from Point3d.origin location)
                |> Vector3d.toMeters
                |> Point3d.fromMeters
    in
    case travelPath of
        [] ->
            ( location, travelPath )

        destination :: remainingPath ->
            if location == destination then
                case remainingPath of
                    [] ->
                        ( location, [] )

                    destination2 :: _ ->
                        ( calculateNewLocation destination2, remainingPath )

            else
                ( calculateNewLocation destination, travelPath )


closestSideOf : Location -> Location -> Location
closestSideOf destination fromLocation =
    [ movePoint (Vector3d.meters 1 0 0) destination
    , movePoint (Vector3d.meters -1 0 0) destination
    , movePoint (Vector3d.meters 0 1 0) destination
    , movePoint (Vector3d.meters 0 -1 0) destination
    ]
        |> List.sortWith
            (\a b ->
                Quantity.compare (Point3d.distanceFrom fromLocation a)
                    (Point3d.distanceFrom fromLocation b)
            )
        |> List.head
        |> Maybe.withDefault (movePoint (Vector3d.meters 1 0 0) destination)


applyAnimationFrame : Int -> Model -> Model
applyAnimationFrame time model =
    let
        ( newLocation, newTravelPath ) =
            case model.appearance of
                Attacking monster ->
                    let
                        destination =
                            Maybe.withDefault monster.location (List.head monster.travelPath)

                        monsterSide =
                            closestSideOf destination model.location
                    in
                    updateLocationTravelPath model.location
                        (Monster.shortestPath model.location monsterSide)

                Fighting monster ->
                    let
                        destination =
                            Maybe.withDefault monster.location (List.head monster.travelPath)

                        monsterSide =
                            closestSideOf destination model.location
                    in
                    updateLocationTravelPath model.location
                        (Monster.shortestPath model.location monsterSide)

                _ ->
                    updateLocationTravelPath model.location model.travelPath

        newState =
            case ( newTravelPath, model.appearance ) of
                ( [], Attacking monster ) ->
                    Fighting monster

                _ ->
                    model.appearance

        newHits =
            List.filter (\hit -> hit.disappearTime > time) model.hits

        newMonsters =
            List.map
                (\m ->
                    case m of
                        AliveMonster monster ->
                            let
                                ( newMonsterLocation, newMonsterTravelPath ) =
                                    updateLocationTravelPath monster.location monster.travelPath
                            in
                            AliveMonster
                                { monster
                                    | hits = List.filter (\hit -> hit.disappearTime > time) monster.hits
                                    , location = newMonsterLocation
                                    , travelPath =
                                        case model.appearance of
                                            Fighting fightingMonster ->
                                                if monster.id == fightingMonster.id then
                                                    List.take 1 newMonsterTravelPath

                                                else
                                                    newMonsterTravelPath

                                            _ ->
                                                newMonsterTravelPath
                                }

                        DeadMonster monster ->
                            Monster.respawnMonster time monster
                )
                model.monsters

        newModel =
            { model
                | location = newLocation
                , travelPath = newTravelPath
                , appearance = newState
                , hits = newHits
                , monsters = newMonsters
                , now = time
            }

        rotationSpeed =
            0.005

        turnAngle : Float -> Angle -> Angle
        turnAngle turn angle =
            angle |> Angle.inTurns |> (+) turn |> Angle.turns
    in
    if Set.member "ArrowLeft" model.keysDown then
        { newModel | cameraAngle = turnAngle -rotationSpeed model.cameraAngle }

    else if Set.member "ArrowRight" model.keysDown then
        { newModel | cameraAngle = turnAngle rotationSpeed model.cameraAngle }

    else
        newModel


applyMouseDown : Point2d Pixels Meters -> Model -> Model
applyMouseDown mousePoint model =
    let
        mouseAxis : Axis3d Meters Meters
        mouseAxis =
            Camera3d.ray (getCamera model) screen mousePoint

        mousePointXyPlane : Maybe Location
        mousePointXyPlane =
            Axis3d.intersectionWithPlane Plane3d.xy mouseAxis
                |> Maybe.map (mapPoint (round >> toFloat))
    in
    if
        ((Point2d.toPixels mousePoint |> .x) > screenWidth)
            || ((Point2d.toPixels mousePoint |> .y) > screenHeight)
    then
        model

    else
        case mousePointXyPlane of
            Nothing ->
                model

            Just destination ->
                let
                    attackingMonster =
                        List.filter
                            (\m ->
                                case m of
                                    AliveMonster monster ->
                                        Point3d.equalWithin (Length.meters 0.5) destination monster.location

                                    DeadMonster _ ->
                                        False
                            )
                            model.monsters
                            |> List.head

                    start =
                        List.head model.travelPath |> Maybe.withDefault model.location
                in
                case attackingMonster of
                    Just (AliveMonster monster) ->
                        let
                            monsterSide =
                                closestSideOf destination model.location

                            newTravelPath =
                                (start :: Monster.shortestPath start monsterSide)
                                    |> List.filter (\point -> point /= model.location)
                        in
                        { model | travelPath = newTravelPath, appearance = Attacking monster }

                    _ ->
                        { model
                            | travelPath =
                                case Monster.shortestPath start destination of
                                    [] ->
                                        [ destination ]

                                    path ->
                                        path
                            , appearance = Standing
                        }


applyAttackRound : Int -> Int -> Model -> Model
applyAttackRound playerDamage monsterDamage model =
    case model.appearance of
        Fighting fightingMonster ->
            let
                newMonsters =
                    Monster.updateAliveMonster fightingMonster.id
                        (\monster ->
                            let
                                newHealth =
                                    monster.health - monsterDamage

                                newHits =
                                    { amount = monsterDamage
                                    , disappearTime = disappearTime
                                    }
                                        :: monster.hits
                            in
                            if newHealth <= 0 then
                                DeadMonster (Monster.killMonster model.now monster)

                            else
                                AliveMonster
                                    { monster
                                        | health = newHealth
                                        , hits = newHits
                                    }
                        )
                        model.monsters

                disappearTime =
                    model.now + 500
            in
            { model
                | health = max 1 (model.health - playerDamage)
                , hits =
                    { amount =
                        if model.health == 1 then
                            0

                        else
                            playerDamage
                    , disappearTime = disappearTime
                    }
                        :: model.hits
                , monsters = newMonsters
                , appearance =
                    case Monster.findAliveMonster fightingMonster.id newMonsters of
                        Nothing ->
                            Standing

                        Just newFightingMonster ->
                            Fighting newFightingMonster
                , accuracyXp =
                    if model.attackStyle == AccuracyStyle then
                        model.accuracyXp + monsterDamage

                    else
                        model.accuracyXp
                , strengthXp =
                    if model.attackStyle == StrengthStyle then
                        model.strengthXp + monsterDamage

                    else
                        model.strengthXp
                , defenseXp =
                    if model.attackStyle == DefenseStyle then
                        model.defenseXp + monsterDamage

                    else
                        model.defenseXp
            }

        _ ->
            model


getCamera : Model -> Camera3d Meters Meters
getCamera model =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = model.location
                , groundPlane = SketchPlane3d.xy
                , azimuth = model.cameraAngle
                , elevation = Angle.degrees 30
                , distance = Length.meters 15
                }
        , verticalFieldOfView = Angle.degrees 30
        }


screenWidth : Float
screenWidth =
    800


screenHeight : Float
screenHeight =
    600


screen : Rectangle2d Pixels Meters
screen =
    Rectangle2d.with
        { x1 = Pixels.pixels 0
        , y1 = Pixels.pixels screenHeight
        , x2 = Pixels.pixels screenWidth
        , y2 = Pixels.pixels 0
        }


mapPoint : (Float -> Float) -> Location -> Location
mapPoint f point =
    point
        |> Point3d.toMeters
        |> (\{ x, y, z } -> { x = f x, y = f y, z = f z })
        |> Point3d.fromMeters


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style "border" "1px solid white"
            , style "display" "inline-block"
            , style "position" "relative"
            , style "overflow" "hidden"
            , style "user-select" "none"
            ]
            [ Scene3d.unlit
                { entities =
                    GameMap.tiles
                        ++ (viewSquare (playerColor model.appearance) model.location
                                :: List.map viewMonster model.monsters
                           )
                , camera = getCamera model
                , clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , dimensions = ( Pixels.pixels (round screenWidth), Pixels.pixels (round screenHeight) )
                }
            , div []
                (List.map
                    (\mon ->
                        case mon of
                            AliveMonster monster ->
                                viewHealthBar (getCamera model)
                                    monster.health
                                    monster.maxHealth
                                    monster.location

                            DeadMonster _ ->
                                text ""
                    )
                    model.monsters
                )
            , viewPlayerText model
            , viewHealthBar (getCamera model)
                model.health
                model.maxHealth
                model.location
            , viewHits (getCamera model) model.hits model.location
            , div [] (List.map (viewMonsterText (getCamera model)) model.monsters)
            , div []
                (List.map
                    (\mon ->
                        case mon of
                            AliveMonster monster ->
                                viewHits (getCamera model) monster.hits monster.location

                            DeadMonster _ ->
                                text ""
                    )
                    model.monsters
                )
            ]
        , div [ style "margin-bottom" "20px" ] [ viewAttackStyle model, viewXpBar model ]
        , div [] [ text "Use left and right arrow keys to rotate the screen." ]
        , div [] [ text "Click on the screen to move to that location." ]
        , div [] [ text "Click on a monster to attack it." ]
        ]


redDamage : String
redDamage =
    "#d33030"


viewHits : Camera3d Meters Meters -> List Hit -> Location -> Html msg
viewHits camera hits point =
    let
        { x, y } =
            Point3d.Projection.toScreenSpace
                camera
                screen
                point
                |> Point2d.toPixels

        width =
            30
    in
    case List.head hits of
        Nothing ->
            text ""

        Just { amount } ->
            div
                [ style "position" "absolute"
                , style "left" (px (x - width / 2))
                , style "top" (px (y - width / 2))
                , style "width" (px width)
                , style "height" (px width)
                , style "background-color" redDamage
                , style "color" "white"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "border-radius" "100%"
                , style "font-weight" "bold"
                ]
                [ div [] [ text (String.fromInt amount) ] ]


movePoint : Vector3d Meters Meters -> Location -> Location
movePoint movement point =
    point
        |> Point3d.toMeters
        |> Vector3d.fromMeters
        |> Vector3d.plus movement
        |> Vector3d.toMeters
        |> Point3d.fromMeters


playerColor : Appearance -> Color
playerColor state =
    case state of
        Standing ->
            Color.blue

        Attacking _ ->
            Color.darkBlue

        Fighting _ ->
            Color.darkRed


viewPlayerText : Model -> Html msg
viewPlayerText model =
    let
        textPoint =
            Point3d.Projection.toScreenSpace (getCamera model) screen model.location
                |> Point2d.toPixels
                |> (\pt -> { pt | x = pt.x - width / 2, y = pt.y - 10 })

        width =
            150
    in
    div
        [ style "position" "absolute"
        , style "left" (px textPoint.x)
        , style "top" (px textPoint.y)
        , style "width" (px width)
        , style "text-align" "center"
        , style "color" "white"
        , style "font-size" "12px"
        , style "font-weight" "bold"
        ]
        [ text (getStateText model) ]


getStateText : Model -> String
getStateText model =
    case model.appearance of
        Standing ->
            case model.travelPath of
                [] ->
                    "Standing"

                _ ->
                    "Walking"

        Attacking _ ->
            "Attacking"

        Fighting _ ->
            "Fighting"


viewMonster : Monster -> Scene3d.Entity Meters
viewMonster mon =
    case mon of
        AliveMonster monster ->
            viewSquare monster.color monster.location

        DeadMonster _ ->
            Scene3d.nothing


viewMonsterText : Camera3d Meters Meters -> Monster -> Html msg
viewMonsterText camera mon =
    case mon of
        AliveMonster monster ->
            let
                textPoint =
                    Point3d.Projection.toScreenSpace camera screen monster.location
                        |> Point2d.toPixels
                        |> (\pt -> { pt | x = pt.x - width / 2, y = pt.y - 10 })

                width =
                    150
            in
            div
                [ style "position" "absolute"
                , style "left" (px textPoint.x)
                , style "top" (px textPoint.y)
                , style "width" (px width)
                , style "text-align" "center"
                , style "color" "white"
                , style "font-size" "12px"
                , style "font-weight" "bold"
                ]
                [ text monster.name ]

        DeadMonster _ ->
            text ""


px : Float -> String
px x =
    String.fromFloat x ++ "px"


viewSquare : Color -> Location -> Scene3d.Entity Meters
viewSquare color point =
    Scene3d.quad (Material.color color)
        (Point3d.translateBy (Vector3d.meters -0.5 -0.5 0) point)
        (Point3d.translateBy (Vector3d.meters 0.5 -0.5 0) point)
        (Point3d.translateBy (Vector3d.meters 0.5 0.5 0) point)
        (Point3d.translateBy (Vector3d.meters -0.5 0.5 0) point)


viewHealthBar : Camera3d Meters Meters -> Int -> Int -> Location -> Html msg
viewHealthBar camera health maxHealth point =
    let
        healthBarLocation =
            Point3d.Projection.toScreenSpace
                camera
                screen
                (movePoint (Vector3d.meters 0 0 1.2) point)
                |> Point2d.toPixels

        healthBarWidth =
            90

        healthBarHeight =
            15
    in
    div
        [ style "position" "absolute"
        , style "left" (px (healthBarLocation.x - (healthBarWidth / 2)))
        , style "top" (px healthBarLocation.y)
        , style "width" (px healthBarWidth)
        , style "height" (px healthBarHeight)
        , style "background-color" redDamage
        ]
        [ div
            [ style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "width" (String.fromFloat (toFloat health / toFloat maxHealth * 100) ++ "%")
            , style "height" (px healthBarHeight)
            , style "background-color" "lime"
            ]
            []
        ]


viewAttackStyle : Model -> Html Msg
viewAttackStyle model =
    div []
        [ button
            [ onClick (SetAttackStyle AccuracyStyle)
            , activeAttackStyle (model.attackStyle == AccuracyStyle)
            ]
            [ text "Accuracy" ]
        , button
            [ onClick (SetAttackStyle StrengthStyle)
            , activeAttackStyle (model.attackStyle == StrengthStyle)
            ]
            [ text "Strength" ]
        , button
            [ onClick (SetAttackStyle DefenseStyle)
            , activeAttackStyle (model.attackStyle == DefenseStyle)
            ]
            [ text "Defense" ]
        ]


activeAttackStyle : Bool -> Attribute msg
activeAttackStyle isActive =
    if isActive then
        style "background-color" redDamage

    else
        style "" ""


viewXpBar : Model -> Html msg
viewXpBar model =
    div []
        [ viewXp "Accuracy" model.accuracyXp
        , viewXp "Strength" model.strengthXp
        , viewXp "Defense" model.defenseXp
        ]


viewXp : String -> Int -> Html msg
viewXp skill xp =
    div [] [ text (skill ++ " XP: " ++ String.fromInt xp) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ onKeyDown (Decode.map KeyDown (Decode.field "key" Decode.string))
        , onKeyUp (Decode.map KeyUp (Decode.field "key" Decode.string))
        , onMouseDown
            (Decode.map2 (\x y -> MouseDown (Point2d.pixels x y))
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
            )
        , onAnimationFrame (Time.posixToMillis >> AnimationFrame)
        ]
            ++ (case model.appearance of
                    Fighting _ ->
                        [ Time.every 1000 (\_ -> GenerateAttackRound) ]

                    _ ->
                        []
               )


generateAttackRound : Cmd Msg
generateAttackRound =
    Random.generate (\( playerDamage, monsterDamage ) -> AttackRound playerDamage monsterDamage)
        (Random.pair (Random.int 0 1) (Random.int 0 1))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
