module Main exposing (main)

import Browser exposing (document)
import Browser.Events exposing (onKeyDown)
import Canvas
import Canvas.Settings as Canvas
import Color exposing (Color)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as Dec
import Random
import Set
import Time


type alias Point =
    ( Int, Int )


type alias Grid =
    { width : Int
    , height : Int
    , sideSize : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type GameState
    = Alive
    | Dead


gameStateKeyDecoder : Model -> Dec.Decoder Msg
gameStateKeyDecoder { gameState } =
    case gameState of
        Alive ->
            Dec.andThen
                (\s ->
                    case s of
                        "ArrowUp" ->
                            Dec.succeed (Turn Up)

                        "ArrowDown" ->
                            Dec.succeed (Turn Down)

                        "ArrowLeft" ->
                            Dec.succeed (Turn Left)

                        "ArrowRight" ->
                            Dec.succeed (Turn Right)

                        _ ->
                            Dec.fail ":("
                )
                (Dec.field "key" Dec.string)

        Dead ->
            Dec.andThen
                (\s ->
                    case s of
                        " " ->
                            Dec.succeed Restart

                        _ ->
                            Dec.fail ":("
                )
                (Dec.field "key" Dec.string)


gameStateView : Model -> List Canvas.Renderable
gameStateView { gameState, grid, playerPos, foodPos, eatenFoods } =
    case gameState of
        Alive ->
            [ viewTiles grid.sideSize playerPos colorPalette.green
            , viewTiles grid.sideSize (Maybe.withDefault [] (Maybe.map List.singleton foodPos)) colorPalette.pink
            ]

        Dead ->
            [ viewTiles grid.sideSize eatenFoods colorPalette.pink ]


type alias Model =
    { grid : Grid
    , playerPos : List Point
    , lastPlayerDir : Direction
    , nextPlayerDir : Direction
    , foodPos : Maybe Point
    , eatenFoods : List Point
    , gameState : GameState
    }


type Msg
    = Turn Direction
    | Tick
    | Food Point
    | Restart


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { grid = { width = 24, height = 18, sideSize = 40 }
            , playerPos = [ ( 4, 0 ), ( 3, 0 ), ( 2, 0 ), ( 1, 0 ), ( 0, 0 ) ]
            , lastPlayerDir = Right
            , nextPlayerDir = Right
            , foodPos = Nothing
            , eatenFoods = []
            , gameState = Alive
            }
    in
    ( model, generateFoodPos model )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( playerX, playerY ) =
            Maybe.withDefault ( 0, 0 ) (List.head model.playerPos)
    in
    case msg of
        Turn d ->
            let
                antonio dir =
                    case dir of
                        Up ->
                            Down

                        Down ->
                            Up

                        Right ->
                            Left

                        Left ->
                            Right
            in
            if d == antonio model.lastPlayerDir then
                ( model, Cmd.none )

            else
                ( { model | nextPlayerDir = d }, Cmd.none )

        Tick ->
            let
                nextPlayerPos =
                    case model.nextPlayerDir of
                        Up ->
                            ( playerX, modBy model.grid.height (playerY - 1) )

                        Down ->
                            ( playerX, modBy model.grid.height (playerY + 1) )

                        Left ->
                            ( modBy model.grid.width (playerX - 1), playerY )

                        Right ->
                            ( modBy model.grid.width (playerX + 1), playerY )

                lunchTime =
                    case model.foodPos of
                        Just foodPos ->
                            nextPlayerPos == foodPos

                        Nothing ->
                            False

                nextFoodPos =
                    if lunchTime then
                        Nothing

                    else
                        model.foodPos

                tailDropSize =
                    if lunchTime then
                        0

                    else
                        1

                nextGameState =
                    if List.member nextPlayerPos model.playerPos then
                        Dead

                    else
                        model.gameState

                nextEatenFoods =
                    case ( model.foodPos, lunchTime ) of
                        ( Just foodPos, True ) ->
                            foodPos :: model.eatenFoods

                        _ ->
                            model.eatenFoods

                cmd =
                    if lunchTime then
                        generateFoodPos model

                    else
                        Cmd.none
            in
            ( { model
                | lastPlayerDir = model.nextPlayerDir
                , playerPos = nextPlayerPos :: List.take (List.length model.playerPos - tailDropSize) model.playerPos
                , foodPos = nextFoodPos
                , eatenFoods = nextEatenFoods
                , gameState = nextGameState
              }
            , cmd
            )

        Food pos ->
            ( { model | foodPos = Just pos }, Cmd.none )

        Restart ->
            init ()


viewTiles : Int -> List Point -> Color -> Canvas.Renderable
viewTiles sideSize tilePositions color =
    let
        floatSideSize =
            toFloat sideSize

        tileRect ( x, y ) =
            Canvas.rect ( toFloat (x * sideSize), toFloat (y * sideSize) ) floatSideSize floatSideSize

        tileRects =
            List.map tileRect tilePositions
    in
    Canvas.shapes [ Canvas.fill color ] tileRects


view : Model -> Browser.Document Msg
view ({ grid } as model) =
    let
        canvasWidth =
            grid.width * grid.sideSize

        canvasHeight =
            grid.height * grid.sideSize

        background =
            Canvas.shapes
                [ Canvas.fill colorPalette.background ]
                [ Canvas.rect ( 0, 0 ) (toFloat canvasWidth) (toFloat canvasHeight) ]

        canvas =
            Canvas.toHtml ( canvasWidth, canvasHeight )
                [ class "canvas" ]
                (background :: gameStateView model)
    in
    { title = "El Snako"
    , body =
        [ div [ class "canvón" ] [ canvas ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions ({ gameState } as model) =
    case gameState of
        Alive ->
            Sub.batch
                [ onKeyDown (gameStateKeyDecoder model)
                , Time.every 100 (\_ -> Tick)
                ]

        Dead ->
            onKeyDown (gameStateKeyDecoder model)


main : Program () Model Msg
main =
    document { init = init, subscriptions = subscriptions, update = update, view = view }



---


generateFoodPos : Model -> Cmd Msg
generateFoodPos { grid, playerPos } =
    let
        gridPos =
            List.concatMap (\x -> List.map (Tuple.pair x) (List.range 0 (grid.height - 1))) (List.range 0 (grid.width - 1))

        possiblePos =
            Set.toList <| Set.diff (Set.fromList gridPos) (Set.fromList playerPos)
    in
    case possiblePos of
        [] ->
            Cmd.none

        head :: tail ->
            Random.generate Food (Random.uniform head tail)


colorPalette =
    { background = Color.rgb255 40 42 54
    , selection = Color.rgb255 68 71 90
    , foreground = Color.rgb255 248 248 242
    , comment = Color.rgb255 98 114 164
    , cyan = Color.rgb255 139 233 253
    , green = Color.rgb255 80 250 123
    , orange = Color.rgb255 255 184 108
    , pink = Color.rgb255 255 121 198
    , purple = Color.rgb255 189 147 249
    , red = Color.rgb255 255 85 85
    , yellow = Color.rgb255 241 250 140
    }
