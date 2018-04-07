module Main exposing (..)

import AnimationFrame
import Geometry exposing (Vector, Rectangle, vadd, vscale, vlength, vclamp)
import Html exposing (Html, text, div, p, a)
import Html.Attributes exposing (class, style, href)
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
    ( 0, 900 )


bounce : Float
bounce =
    -0.8


drag : Float
drag =
    -0.2



---- MODEL ----


type alias Model =
    { velocity : Vector
    , position : Vector
    , windowSize : Window.Size
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform Resize Window.size )


initialModel : Model
initialModel =
    { velocity = ( 500, 0 )
    , position = ( 0, 0 )
    , windowSize = Window.Size 0 0
    }



---- UPDATE ----


type Msg
    = Tick Time
    | Resize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( tick time model, Cmd.none )

        Resize size ->
            ( { model | windowSize = size }, Cmd.none )


tick : Time -> Model -> Model
tick time model =
    let
        seconds =
            Time.inSeconds time

        velocity =
            model.velocity
                |> vadd (vscale (drag * seconds) model.velocity)
                |> vadd (vscale seconds gravity)

        position =
            vadd model.position (vscale seconds velocity)

        bounds =
            calculateBounds model.windowSize
    in
        { model
            | velocity = collisions bounds position velocity
            , position = vclamp bounds position
        }


calculateBounds : Window.Size -> Rectangle
calculateBounds { width, height } =
    Rectangle radius radius (toFloat width - radius) (toFloat height - radius)


collisions : Rectangle -> Vector -> Vector -> Vector
collisions bounds ( x, y ) ( vx, vy ) =
    ( if x < bounds.left || x > bounds.right then
        vx * bounce
      else
        vx
    , if y < bounds.top || y > bounds.bottom then
        vy * bounce
      else
        vy
    )



---- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ AnimationFrame.diffs Tick, Window.resizes Resize ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "homepage" ]
        [ ball model.position
        , credits
        ]


ball : Vector -> Html Msg
ball position =
    let
        ( x, y ) =
            position

        circumference =
            pi * diameter

        rotation =
            toFloat (round x % round circumference) / circumference * pi * 2

        left =
            toString (x - radius) ++ "px"

        top =
            toString (y - radius) ++ "px"

        transform =
            "rotate(" ++ toString rotation ++ "rad)"
    in
        div [ class "ball", style [ ( "left", left ), ( "top", top ), ( "transform", transform ) ] ] []


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
