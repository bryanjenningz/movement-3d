module Main exposing (main)

import Angle
import Axis3d
import Browser
import Browser.Events exposing (onMouseDown)
import Camera3d
import Color
import Direction3d
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
import Vector3d
import Viewpoint3d


type alias Model =
    { mouseX : Float
    , mouseY : Float
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { mouseX = 0, mouseY = 0 }, Cmd.none )


type Msg
    = MouseDown Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown mouseX mouseY ->
            ( { model | mouseX = mouseX, mouseY = mouseY }, Cmd.none )


view : Model -> Html msg
view model =
    let
        camera : Camera3d.Camera3d Length.Meters coordinates
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 0 -20 30
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        screen : Rectangle2d.Rectangle2d Pixels.Pixels coordinates
        screen =
            Rectangle2d.with
                { x1 = Pixels.pixels 0
                , y1 = Pixels.pixels 0
                , x2 = Pixels.pixels 800
                , y2 = Pixels.pixels 600
                }

        mousePoint : Point2d.Point2d Pixels.Pixels screenCoordinates
        mousePoint =
            Point2d.pixels 200 300

        mouseAxis : Axis3d.Axis3d Length.Meters coordinates
        mouseAxis =
            Camera3d.ray camera screen mousePoint

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
                    [ case maybeXyPlaneMousePoint of
                        Nothing ->
                            viewFlatSquare Point3d.origin

                        Just xyPlaneMousePoint ->
                            viewFlatSquare xyPlaneMousePoint
                    , viewFlatSquare (Point3d.meters 10 -9 0)
                    , viewFlatSquare (Point3d.meters -10 -9 0)
                    , viewFlatSquare (Point3d.meters 15 13 0)
                    , viewFlatSquare (Point3d.meters -15 13 0)
                    ]
                , camera = camera
                , clipDepth = Length.meters 1
                , background = Scene3d.transparentBackground
                , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
                }
            ]
        , div [] [ text (Debug.toString model) ]
        , div [] [ text (Debug.toString camera) ]
        ]


viewFlatSquare : Point3d.Point3d Length.Meters coordinates -> Scene3d.Entity coordinates
viewFlatSquare point =
    Scene3d.quad (Material.color Color.blue)
        (Point3d.translateBy (Vector3d.from Point3d.origin (Point3d.meters -1 -1 0)) point)
        (Point3d.translateBy (Vector3d.from Point3d.origin (Point3d.meters 1 -1 0)) point)
        (Point3d.translateBy (Vector3d.from Point3d.origin (Point3d.meters 1 1 0)) point)
        (Point3d.translateBy (Vector3d.from Point3d.origin (Point3d.meters -1 1 0)) point)


subscriptions : Model -> Sub Msg
subscriptions model =
    onMouseDown
        (Decode.map2 MouseDown
            (Decode.field "clientX" Decode.float)
            (Decode.field "clientY" Decode.float)
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
