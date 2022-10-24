module Main exposing (main)

import Angle
import Axis3d
import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp, onMouseDown)
import Camera3d
import Color
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Length
import Pixels
import Plane3d
import Point2d
import Point3d
import Rectangle2d
import Scene3d
import Scene3d.Material as Material
import Set exposing (Set)
import SketchPlane3d
import Vector3d
import Viewpoint3d


type alias Model =
    { mousePoint : Point2d.Point2d Pixels.Pixels Pixels.Pixels
    , cameraAngle : Angle.Angle
    , keysDown : Set String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { mousePoint = Point2d.pixels 0 0
      , cameraAngle = Angle.turns 0
      , keysDown = Set.empty
      }
    , Cmd.none
    )


type Msg
    = AnimationFrame
    | MouseDown (Point2d.Point2d Pixels.Pixels Pixels.Pixels)
    | KeyDown String
    | KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame ->
            if Set.member "ArrowLeft" model.keysDown then
                ( { model
                    | cameraAngle =
                        model.cameraAngle
                            |> Angle.inTurns
                            |> (\turns -> turns - 0.01)
                            |> Angle.turns
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
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        MouseDown mousePoint ->
            ( { model | mousePoint = mousePoint }, Cmd.none )

        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )


view : Model -> Html msg
view model =
    let
        camera : Camera3d.Camera3d Length.Meters coordinates
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbit
                        { focalPoint = Point3d.meters 0 0 1
                        , groundPlane = SketchPlane3d.xy
                        , azimuth = model.cameraAngle
                        , elevation = Angle.degrees 45
                        , distance = Length.meters 30
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        screen : Rectangle2d.Rectangle2d Pixels.Pixels coordinates
        screen =
            Rectangle2d.with
                { x1 = Pixels.pixels 0
                , y1 = Pixels.pixels 600
                , x2 = Pixels.pixels 800
                , y2 = Pixels.pixels 0
                }

        mouseAxis : Axis3d.Axis3d Length.Meters coordinates
        mouseAxis =
            Camera3d.ray camera screen model.mousePoint

        maybeXyPlaneMousePoint : Maybe (Point3d.Point3d Length.Meters coordinates)
        maybeXyPlaneMousePoint =
            Axis3d.intersectionWithPlane Plane3d.xy mouseAxis
    in
    div []
        [ div
            [ style "border" "1px solid white"
            , style "display" "inline-block"
            ]
            [ Scene3d.unlit
                { entities =
                    (case maybeXyPlaneMousePoint of
                        Nothing ->
                            viewSquare Point3d.origin

                        Just xyPlaneMousePoint ->
                            viewSquare xyPlaneMousePoint
                    )
                        :: [ viewSquare (Point3d.meters -5 5 0)
                           , viewSquare (Point3d.meters -5 -5 0)
                           , viewSquare (Point3d.meters 5 5 0)
                           , viewSquare (Point3d.meters 5 -5 0)
                           ]
                , camera = camera
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
subscriptions model =
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
