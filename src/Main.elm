module Main exposing (..)

---- EXTERNAL DEPENDENCIES ----

import AnimationFrame
import Html exposing (Html, text, div, p, a)
import Html.Attributes exposing (class, style, href)
import Html.Events as Events
import Json.Decode as Decode
import Mouse
import Task
import Time exposing (Time)
import Window


---- INTERNAL DEPENDENCIES ----

import Vector exposing (Vector, Rectangle)


---- CONSTANTS ----


initialPosition : Vector
initialPosition =
    Vector radius 300


initialVelocity : Vector
initialVelocity =
    Vector 750 -750


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


resistance : Float
resistance =
    -0.2


throwMultiplier : Float
throwMultiplier =
    30



---- MODEL ----


type alias Model =
    { position : Vector
    , velocity : Vector
    , rotation : Float
    , windowSize : Window.Size
    , dragStatus : DragStatus
    }


type DragStatus
    = NotDragging
    | Dragging DragModel


type alias DragModel =
    { lastPosition : Vector
    , ballOffset : Vector
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Task.perform Resize Window.size )


initModel : Model
initModel =
    { position = initialPosition
    , velocity = initialVelocity
    , rotation = 0
    , windowSize = Window.Size 0 0
    , dragStatus = NotDragging
    }



---- UPDATE ----


type Msg
    = Tick Time
    | Resize Window.Size
    | BeginDrag Mouse.Position
    | Drag Mouse.Position
    | EndDrag Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( tick time model, Cmd.none )

        Resize size ->
            ( { model | windowSize = size }, Cmd.none )

        BeginDrag position ->
            ( beginDrag position model, Cmd.none )

        Drag position ->
            ( drag position model, Cmd.none )

        EndDrag position ->
            ( endDrag position model, Cmd.none )


tick : Time -> Model -> Model
tick time model =
    case model.dragStatus of
        NotDragging ->
            let
                seconds =
                    Time.inSeconds time

                velocity =
                    model.velocity
                        |> Vector.add (Vector.scale (resistance * seconds) model.velocity)
                        |> Vector.add (Vector.scale seconds gravity)

                position =
                    Vector.add model.position (Vector.scale seconds velocity)

                circumference =
                    pi * diameter

                rotation =
                    toFloat (round position.x % round circumference) / circumference * turns 1

                bounds =
                    calculateBounds model.windowSize
            in
                { model
                    | position = Vector.clamp bounds position
                    , velocity = calculateCollisionVelocity bounds position velocity
                    , rotation = rotation
                }

        Dragging _ ->
            model


calculateBounds : Window.Size -> Rectangle
calculateBounds { width, height } =
    Rectangle radius radius (toFloat width - radius) (toFloat height - radius)


calculateCollisionVelocity : Rectangle -> Vector -> Vector -> Vector
calculateCollisionVelocity bounds position velocity =
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


beginDrag : Mouse.Position -> Model -> Model
beginDrag mousePosition model =
    { model
        | dragStatus =
            Dragging
                { lastPosition = model.position
                , ballOffset = Vector.subtract model.position (vectorize mousePosition)
                }
    }


drag : Mouse.Position -> Model -> Model
drag mousePosition model =
    case model.dragStatus of
        NotDragging ->
            model

        Dragging dragModel ->
            { model
                | position = Vector.add (vectorize mousePosition) dragModel.ballOffset
                , dragStatus = Dragging { dragModel | lastPosition = model.position }
            }


endDrag : Mouse.Position -> Model -> Model
endDrag mousePosition model =
    case model.dragStatus of
        NotDragging ->
            model

        Dragging dragModel ->
            let
                position =
                    Vector.add (vectorize mousePosition) dragModel.ballOffset

                velocity =
                    Vector.scale throwMultiplier (Vector.subtract position dragModel.lastPosition)
            in
                { model | position = position, velocity = velocity, dragStatus = NotDragging }


vectorize : Mouse.Position -> Vector
vectorize { x, y } =
    Vector (toFloat x) (toFloat y)



---- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes Resize
        , Mouse.moves Drag
        , Mouse.ups EndDrag
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
            , Events.on "mousedown" (Decode.map BeginDrag Mouse.position)
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
