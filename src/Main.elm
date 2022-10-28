module Main exposing (AttackStyle(..), Model, Monster, Msg(..), State(..), getCamera, init, main, screen, update)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp, onMouseDown)
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Html exposing (Attribute, Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Length exposing (Meters)
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
    { location : Point3d Meters Meters
    , state : State
    , health : Int
    , maxHealth : Int
    , hits : List Hit
    , travelPath : List (Point3d Meters Meters)
    , cameraAngle : Angle
    , keysDown : Set String
    , monsters : List Monster
    , now : Int
    , attackStyle : AttackStyle
    , accuracyXp : Int
    , strengthXp : Int
    , defenseXp : Int
    }


type State
    = Standing
    | Attacking Monster
    | Fighting Monster


type alias Hit =
    { amount : Int
    , disappearTime : Int
    }


type alias Monster =
    { id : Int
    , location : Point3d Meters Meters
    , health : Int
    , maxHealth : Int
    , hits : List Hit
    }


type AttackStyle
    = AccuracyStyle
    | StrengthStyle
    | DefenseStyle


init : () -> ( Model, Cmd Msg )
init () =
    ( { location = Point3d.meters 0 0 0
      , state = Standing
      , health = 10
      , maxHealth = 10
      , hits = []
      , travelPath = []
      , cameraAngle = Angle.turns 0
      , keysDown = Set.empty
      , monsters =
            [ Point3d.meters -3 3 0
            , Point3d.meters -3 -3 0
            , Point3d.meters 3 3 0
            , Point3d.meters 3 -3 0
            ]
                |> List.indexedMap (\id monster -> Monster id monster 3 3 [])
      , now = -1
      , attackStyle = AccuracyStyle
      , accuracyXp = 0
      , strengthXp = 0
      , defenseXp = 0
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame time ->
            ( applyAnimationFrame time model, Cmd.none )

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


applyAnimationFrame : Int -> Model -> Model
applyAnimationFrame time model =
    let
        ( newLocation, newTravelPath ) =
            case model.travelPath of
                [] ->
                    ( model.location, model.travelPath )

                destination :: remainingPath ->
                    if model.location == destination then
                        case remainingPath of
                            [] ->
                                ( model.location, [] )

                            destination2 :: _ ->
                                ( calculateNewLocation destination2, remainingPath )

                    else
                        ( calculateNewLocation destination, model.travelPath )

        calculateNewLocation : Point3d Meters Meters -> Point3d Meters Meters
        calculateNewLocation destination =
            Vector3d.from model.location destination
                |> (\path ->
                        let
                            minLength =
                                min 0.05 (Vector3d.length path |> Length.inMeters)
                        in
                        Vector3d.scaleTo (Length.meters minLength) path
                   )
                |> Vector3d.plus (Vector3d.from Point3d.origin model.location)
                |> Vector3d.toMeters
                |> Point3d.fromMeters

        newState =
            case ( newTravelPath, model.state ) of
                ( [], Attacking monster ) ->
                    Fighting monster

                _ ->
                    model.state

        newHits =
            List.filter (\hit -> hit.disappearTime > time) model.hits

        newMonsters =
            List.map
                (\monster -> { monster | hits = List.filter (\hit -> hit.disappearTime > time) monster.hits })
                model.monsters

        newModel =
            { model
                | location = newLocation
                , travelPath = newTravelPath
                , state = newState
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

        mousePointXyPlane : Maybe (Point3d Meters Meters)
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
                            (\monster -> Point3d.equalWithin (Length.meters 0.01) destination monster.location)
                            model.monsters
                            |> List.head

                    start =
                        List.head model.travelPath |> Maybe.withDefault model.location
                in
                case attackingMonster of
                    Just monster ->
                        let
                            newTravelPath =
                                start
                                    :: shortestPath start destination
                                    -- Don't go directly on the monster when you're attacking the monster
                                    |> List.filter (\point -> point /= destination && point /= model.location)
                        in
                        { model
                            | travelPath =
                                case newTravelPath of
                                    [] ->
                                        [ movePoint (Vector3d.meters 1 0 0) destination
                                        , movePoint (Vector3d.meters -1 0 0) destination
                                        , movePoint (Vector3d.meters 0 1 0) destination
                                        , movePoint (Vector3d.meters 0 -1 0) destination
                                        ]
                                            |> List.sortWith
                                                (\a b ->
                                                    Quantity.compare (Point3d.distanceFrom model.location a)
                                                        (Point3d.distanceFrom model.location b)
                                                )
                                            |> List.head
                                            |> Maybe.withDefault (movePoint (Vector3d.meters 1 0 0) destination)
                                            |> (\newDestination ->
                                                    if Point3d.equalWithin (Length.meters 0.01) model.location newDestination then
                                                        []

                                                    else
                                                        [ newDestination ]
                                               )

                                    _ ->
                                        newTravelPath
                            , state = Attacking monster
                        }

                    Nothing ->
                        { model
                            | travelPath = shortestPath start destination
                            , state = Standing
                        }


applyAttackRound : Int -> Int -> Model -> Model
applyAttackRound playerDamage monsterDamage model =
    case model.state of
        Fighting fightingMonster ->
            let
                newMonsters =
                    List.filterMap
                        (\monster ->
                            if monster.id == fightingMonster.id then
                                let
                                    newHealth =
                                        monster.health - monsterDamage
                                in
                                if newHealth <= 0 then
                                    Nothing

                                else
                                    Just
                                        { monster
                                            | health = newHealth
                                            , hits =
                                                { amount = monsterDamage
                                                , disappearTime = disappearTime
                                                }
                                                    :: monster.hits
                                        }

                            else
                                Just monster
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
                , state =
                    case List.filter (\monster -> monster.id == fightingMonster.id) newMonsters |> List.head of
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


shortestPath : Point3d Meters coordinates -> Point3d Meters coordinates -> List (Point3d Meters coordinates)
shortestPath start destination =
    if start == destination then
        []

    else
        let
            xDiff =
                (destination |> Point3d.toMeters |> .x)
                    - (start |> Point3d.toMeters |> .x)
                    |> clamp -1 1

            yDiff =
                (destination |> Point3d.toMeters |> .y)
                    - (start |> Point3d.toMeters |> .y)
                    |> clamp -1 1

            newStart =
                Vector3d.plus
                    (Vector3d.from Point3d.origin (Point3d.meters xDiff yDiff 0))
                    (Vector3d.from Point3d.origin start)
                    |> Vector3d.toMeters
                    |> Point3d.fromMeters
        in
        newStart :: shortestPath newStart destination


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


mapPoint : (Float -> Float) -> Point3d Meters coordinates -> Point3d Meters coordinates
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
            ]
            [ Scene3d.unlit
                { entities =
                    viewSquare (playerColor model.state) model.location
                        :: List.map
                            (\monster -> viewSquare Color.darkGreen monster.location)
                            model.monsters
                , camera = getCamera model
                , clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , dimensions = ( Pixels.pixels (round screenWidth), Pixels.pixels (round screenHeight) )
                }
            , div []
                (List.map
                    (\monster ->
                        viewHealthBar (getCamera model)
                            monster.health
                            monster.maxHealth
                            monster.location
                    )
                    model.monsters
                )
            , viewHealthBar (getCamera model)
                model.health
                model.maxHealth
                model.location
            , viewHits (getCamera model) model.hits model.location
            , div []
                (List.map (\monster -> viewHits (getCamera model) monster.hits monster.location) model.monsters)
            ]
        , div [ style "margin-bottom" "20px" ] [ viewAttackStyle model, viewXpBar model ]
        , div [] [ text "Use left and right arrow keys to rotate the screen." ]
        , div [] [ text "Click on the screen to move to that location." ]
        , div [] [ text "Click on a monster to attack it." ]
        ]


redDamage : String
redDamage =
    "#d33030"


viewHits : Camera3d Meters Meters -> List Hit -> Point3d Meters Meters -> Html msg
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
                , style "left" (String.fromFloat (x - width / 2) ++ "px")
                , style "top" (String.fromFloat (y - width / 2) ++ "px")
                , style "width" (String.fromFloat width ++ "px")
                , style "height" (String.fromFloat width ++ "px")
                , style "background-color" redDamage
                , style "color" "white"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                , style "border-radius" "100%"
                , style "font-weight" "bold"
                ]
                [ div [] [ text (String.fromInt amount) ] ]


movePoint : Vector3d Meters Meters -> Point3d Meters Meters -> Point3d Meters Meters
movePoint movement point =
    point
        |> Point3d.toMeters
        |> Vector3d.fromMeters
        |> Vector3d.plus movement
        |> Vector3d.toMeters
        |> Point3d.fromMeters


playerColor : State -> Color
playerColor state =
    case state of
        Standing ->
            Color.blue

        Attacking _ ->
            Color.darkBlue

        Fighting _ ->
            Color.darkRed


viewSquare : Color -> Point3d Length.Meters coordinates -> Scene3d.Entity coordinates
viewSquare color point =
    Scene3d.quad (Material.color color)
        (Point3d.translateBy (Vector3d.meters -0.5 -0.5 0) point)
        (Point3d.translateBy (Vector3d.meters 0.5 -0.5 0) point)
        (Point3d.translateBy (Vector3d.meters 0.5 0.5 0) point)
        (Point3d.translateBy (Vector3d.meters -0.5 0.5 0) point)


viewHealthBar : Camera3d Meters Meters -> Int -> Int -> Point3d Meters Meters -> Html msg
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
        , style "left" (String.fromFloat (healthBarLocation.x - (healthBarWidth / 2)) ++ "px")
        , style "top" (String.fromFloat healthBarLocation.y ++ "px")
        , style "width" (String.fromFloat healthBarWidth ++ "px")
        , style "height" (String.fromFloat healthBarHeight ++ "px")
        , style "background-color" redDamage
        ]
        [ div
            [ style "position" "absolute"
            , style "left" "0"
            , style "top" "0"
            , style "width" (String.fromFloat (toFloat health / toFloat maxHealth * 100) ++ "%")
            , style "height" (String.fromFloat healthBarHeight ++ "px")
            , style "background-color" "green"
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
        ]
            ++ (if
                    (not <| List.isEmpty model.travelPath)
                        || Set.member "ArrowLeft" model.keysDown
                        || Set.member "ArrowRight" model.keysDown
                        || (case model.state of
                                Attacking _ ->
                                    True

                                Fighting _ ->
                                    True

                                Standing ->
                                    False
                           )
                        || (List.length model.hits > 0)
                then
                    [ onAnimationFrame (Time.posixToMillis >> AnimationFrame) ]

                else
                    []
               )
            ++ (case model.state of
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
