module Main exposing (main)

import Angle
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp, onMouseDown)
import Camera3d exposing (Camera3d)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Plane3d
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Material as Material
import Set exposing (Set)
import SketchPlane3d
import Vector3d
import Viewpoint3d


type alias Model =
    { location : Point3d Meters Meters
    , destination : Point3d Meters Meters
    , cameraAngle : Angle.Angle
    , keysDown : Set String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { location = Point3d.meters 0 0 0
      , destination = Point3d.meters 0 0 0
      , cameraAngle = Angle.turns 0
      , keysDown = Set.empty
      }
    , Cmd.none
    )


type Msg
    = AnimationFrame
    | MouseDown (Point2d Pixels Meters)
    | KeyDown String
    | KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame ->
            let
                newLocation : Point3d Meters Meters
                newLocation =
                    if model.destination == model.location then
                        model.location

                    else
                        Vector3d.from model.location model.destination
                            |> (\path ->
                                    let
                                        minLength =
                                            min 0.2 (Vector3d.length path |> Length.inMeters)
                                    in
                                    Vector3d.scaleTo (Length.meters minLength) path
                               )
                            |> Vector3d.plus (Vector3d.from Point3d.origin model.location)
                            |> Vector3d.toMeters
                            |> Point3d.fromMeters
            in
            if Set.member "ArrowLeft" model.keysDown then
                ( { model
                    | cameraAngle =
                        model.cameraAngle
                            |> Angle.inTurns
                            |> (\turns -> turns - 0.01)
                            |> Angle.turns
                    , location = newLocation
                  }
                , Cmd.none
                )

            else if Set.member "ArrowRight" model.keysDown then
                ( { model
                    | cameraAngle =
                        model.cameraAngle
                            |> Angle.inTurns
                            |> (\turns -> turns + 0.01)
                            |> Angle.turns
                    , location = newLocation
                  }
                , Cmd.none
                )

            else if newLocation == model.location then
                ( model, Cmd.none )

            else
                ( { model | location = newLocation }, Cmd.none )

        MouseDown mousePoint ->
            let
                camera : Camera3d Meters Meters
                camera =
                    getCamera model

                screen : Rectangle2d Pixels Meters
                screen =
                    Rectangle2d.with
                        { x1 = Pixels.pixels 0
                        , y1 = Pixels.pixels 600
                        , x2 = Pixels.pixels 800
                        , y2 = Pixels.pixels 0
                        }

                mouseAxis : Axis3d Meters Meters
                mouseAxis =
                    Camera3d.ray camera screen mousePoint

                maybeXyPlaneMousePoint : Maybe (Point3d Meters Meters)
                maybeXyPlaneMousePoint =
                    Axis3d.intersectionWithPlane Plane3d.xy mouseAxis
            in
            case maybeXyPlaneMousePoint of
                Nothing ->
                    ( model, Cmd.none )

                Just xyPlaneMousePoint ->
                    ( { model | destination = xyPlaneMousePoint }, Cmd.none )

        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )


getCamera : Model -> Camera3d Meters Meters
getCamera model =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = model.location
                , groundPlane = SketchPlane3d.xy
                , azimuth = model.cameraAngle
                , elevation = Angle.degrees 30
                , distance = Length.meters 30
                }
        , verticalFieldOfView = Angle.degrees 30
        }


view : Model -> Html msg
view model =
    div []
        [ div
            [ style "border" "1px solid white"
            , style "display" "inline-block"
            ]
            [ Scene3d.unlit
                { entities =
                    [ viewSquare (Point3d.meters -5 5 0)
                    , viewSquare (Point3d.meters -5 -5 0)
                    , viewSquare (Point3d.meters 5 5 0)
                    , viewSquare (Point3d.meters 5 -5 0)
                    , viewSquare model.location
                    ]
                , camera = getCamera model
                , clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
                }
            ]
        ]


viewSquare : Point3d.Point3d Length.Meters coordinates -> Scene3d.Entity coordinates
viewSquare point =
    Scene3d.quad (Material.color Color.blue)
        (Point3d.translateBy (Vector3d.meters -1 -1 0) point)
        (Point3d.translateBy (Vector3d.meters 1 -1 0) point)
        (Point3d.translateBy (Vector3d.meters 1 1 0) point)
        (Point3d.translateBy (Vector3d.meters -1 1 0) point)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrame (\_ -> AnimationFrame)
        , onKeyDown (Decode.map KeyDown (Decode.field "key" Decode.string))
        , onKeyUp (Decode.map KeyUp (Decode.field "key" Decode.string))
        , onMouseDown
            (Decode.map2 (\x y -> MouseDown (Point2d.pixels x y))
                (Decode.field "clientX" Decode.float)
                (Decode.field "clientY" Decode.float)
            )
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
