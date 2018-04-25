module Main exposing (..)

import AnimationFrame
import Geometry exposing (Vector, Rectangle, vadd, vsubtract, vscale, vlength, vclamp)
import Html exposing (Html, text, div, p, a)
import Html.Attributes exposing (class, style, href)
import Html.Events exposing (on, onMouseUp)
import Json.Decode as Decode exposing (Decoder)
import Mouse
import Task
import Time exposing (Time)
import Window


---- CONSTANTS ----


diameter : Float
diameter =
    125


radius : Float
radius =
    diameter / 2


gravity : Vector
gravity =
    Vector 0 900


bounce : Float
bounce =
    -0.8


drag : Float
drag =
    -0.2



---- MODEL ----


type alias DragInfo =
    { lastPosition : Vector
    , ballOffset : Vector
    }


type DragStatus
    = NotDragging
    | Dragging DragInfo


type alias Model =
    { velocity : Vector
    , position : Vector
    , rotation : Float
    , windowSize : Window.Size
    , dragStatus : DragStatus
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform Resize Window.size )


initialModel : Model
initialModel =
    { velocity = Vector 500 0
    , position = Vector 0 0
    , rotation = 0
    , windowSize = Window.Size 0 0
    , dragStatus = NotDragging
    }



---- UPDATE ----


type Msg
    = Tick Time
    | Resize Window.Size
    | StartDrag Mouse.Position
    | DoDrag Mouse.Position
    | StopDrag Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( tick time model, Cmd.none )

        Resize size ->
            ( { model | windowSize = size }, Cmd.none )

        StartDrag position ->
            ( startDrag position model, Cmd.none )

        DoDrag position ->
            ( doDrag position model, Cmd.none )

        StopDrag position ->
            ( stopDrag position model, Cmd.none )


tick : Time -> Model -> Model
tick time model =
    case model.dragStatus of
        NotDragging ->
            let
                seconds =
                    Time.inSeconds time

                velocity =
                    model.velocity
                        |> vadd (vscale (drag * seconds) model.velocity)
                        |> vadd (vscale seconds gravity)

                position =
                    vadd model.position (vscale seconds velocity)

                circumference =
                    pi * diameter

                rotation =
                    toFloat (round position.x % round circumference) / circumference * pi * 2

                bounds =
                    calculateBounds model.windowSize
            in
                { model
                    | velocity = collisions bounds position velocity
                    , position = vclamp bounds position
                    , rotation = rotation
                }

        Dragging dragInfo ->
            model


calculateBounds : Window.Size -> Rectangle
calculateBounds { width, height } =
    Rectangle radius radius (toFloat width - radius) (toFloat height - radius)


collisions : Rectangle -> Vector -> Vector -> Vector
collisions bounds position velocity =
    let
        vx =
            if position.x < bounds.left || position.x > bounds.right then
                velocity.x * bounce
            else
                velocity.x

        vy =
            if position.y < bounds.top || position.y > bounds.bottom then
                velocity.y * bounce
            else
                velocity.y
    in
        Vector vx vy


startDrag : Mouse.Position -> Model -> Model
startDrag position model =
    let
        ballOffset =
            vsubtract model.position (vectorize position)
    in
        { model | dragStatus = Dragging { lastPosition = model.position, ballOffset = ballOffset } }


doDrag : Mouse.Position -> Model -> Model
doDrag position model =
    case model.dragStatus of
        NotDragging ->
            model

        Dragging dragInfo ->
            { model | position = vadd (vectorize position) dragInfo.ballOffset, dragStatus = Dragging { dragInfo | lastPosition = model.position } }


stopDrag : Mouse.Position -> Model -> Model
stopDrag position model =
    case model.dragStatus of
        NotDragging ->
            model

        Dragging dragInfo ->
            { model
                | dragStatus = NotDragging
                , position = vadd (vectorize position) dragInfo.ballOffset
                , velocity =
                    vsubtract (vectorize position) dragInfo.lastPosition |> vscale 30
            }


vectorize : Mouse.Position -> Vector
vectorize { x, y } =
    Vector (toFloat x) (toFloat y)



---- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes Resize
        , Mouse.moves DoDrag
        , Mouse.ups StopDrag
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "homepage" ]
        [ ball model.position model.rotation
        , credits
        ]


ball : Vector -> Float -> Html Msg
ball position rotation =
    let
        left =
            toString (position.x - radius) ++ "px"

        top =
            toString (position.y - radius) ++ "px"

        transform =
            "rotate(" ++ toString rotation ++ "rad)"
    in
        div
            [ class "ball"
            , style [ ( "left", left ), ( "top", top ), ( "transform", transform ) ]
            , on "mousedown" (Decode.map StartDrag Mouse.position)
            ]
            []


credits : Html Msg
credits =
    p [ class "credits" ]
        [ a [ href "mailto:robert@noisysocks.com" ]
            [ text "Robert Anderson" ]
        , text " / "
        , a [ href "https://twitter.com/noisysocks" ]
            [ text "@noisysocks" ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
