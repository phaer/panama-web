module Panama exposing (main)
{-|
A simple web frontend for mpv.io


@docs main

|-}

import String
import WebSocket
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (int, string)
import Json.Decode as JD

websocket : String
websocket = "ws://127.0.0.1:3000/socket"

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { currentInput : String
    , status : String
    , playing : Bool
    , volume : Int
    , position : Int
    , playlist : List PlaylistItem
    }

type alias PlaylistItem =
    { title : Maybe String
    , mediaUrl : Maybe String
    , sourceUrl : String
    , current : Bool
    , index : Int
    }
type alias Playlist = List PlaylistItem

type Msg
    = TogglePlay
    | VolumeChanged Int
    | PositionChanged Int
    | PlaylistChanged (List PlaylistItem)
    | MessageReceived String
    | InputChanged String
    | PlaylistAdd String
    | PlaylistRemove Int
    | PlaylistSelect Int

-- # Init
init : (Model, Cmd Msg)
init =
    (Model "" "" False 0 0 [], sendCommand "update")

sendJsonList : List Json.Encode.Value -> Cmd a
sendJsonList l = WebSocket.send websocket <| Json.Encode.encode 0 <| Json.Encode.list l

sendCommand : String -> Cmd a
sendCommand cmd =
    sendJsonList [string cmd]

sendProperty : String -> Json.Encode.Value -> Cmd a
sendProperty n v =
    sendJsonList [string n, v]

playlistDecoder : JD.Decoder Playlist
playlistDecoder =
    JD.list <| JD.map5 PlaylistItem
        (JD.maybe (JD.field "title" JD.string))
        (JD.maybe (JD.field "media_url" JD.string))
        (JD.field "source_url" JD.string)
        (JD.oneOf [JD.field "current" JD.bool, JD.succeed False])
        (JD.field "index" JD.int)

decodeModel : String -> Model
decodeModel s =
    case JD.decodeString modelDecoder s of
        Ok value -> value
        Err msg -> Model "" (Debug.log "error: " msg) False 0 0 []

modelDecoder : JD.Decoder Model
modelDecoder = JD.map4 (Model "" "")
               (JD.field "playing" JD.bool)
               (JD.field "volume" JD.int)
               (JD.field "position" JD.int)
               (JD.field "playlist" playlistDecoder)

setCurrentItem: Model -> Int -> Model
setCurrentItem m i = {m | playlist = List.indexedMap (\n p -> {p | current = n == i}) m.playlist}


-- # Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "msg" msg of
    TogglePlay ->
      ({model | playing = (not model.playing)}, sendCommand "toggle-play")

    VolumeChanged v ->
      ({model | volume = v}, int v |> sendProperty "volume")

    PositionChanged p ->
      ({model | position = p}, int p |> sendProperty "position")

    PlaylistChanged pl ->
      ({model | playlist = pl}, Cmd.none)

    MessageReceived m ->
      (decodeModel m, Cmd.none)

    InputChanged i ->
      ({model | currentInput = i}, Cmd.none)

    PlaylistAdd i ->
      ({model | currentInput = ""}, string i |> sendProperty "playlist-add")

    PlaylistRemove i ->
      (model, int i |> sendProperty "playlist-remove")

    PlaylistSelect i ->
      (setCurrentItem model i, int i |> sendProperty "playlist-select")

-- # Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen websocket MessageReceived


-- # View
view : Model -> Html Msg
view model =
  div [ ]
    [ viewLog model,
      viewControls model
    , viewPlaylist model
    ]

viewLog : Model -> Html Msg
viewLog model = div [class "log"] [text model.status]

viewControls : Model -> Html Msg
viewControls model =
  div [class "controls"]
    [ viewInput model
    , viewVolume model
    , viewPosition model
    ]


parseIntWithDefault : Int -> String -> Int
parseIntWithDefault d = String.toInt >> Result.toMaybe >> Maybe.withDefault d


slider :  String -> Int -> (String -> a) -> Html a
slider n v handler =
    div [class <| n ++ " mdl-grid"]
        [ label [ for n
                , class "mdl-cell mdl-cell--1-col"
                ] [text n]
        , div [ class "mdl-cell mdl-cell--3-col"]
            [ input [ onInput handler
                    , name n
                    , type_ "range"
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "100"
                    , value <| toString v
                    , class "mdl-slider mdl-js-slider"
                    ] []
            ]
        , span [ class "mdl-cell mdl-cell-1-col" ] [ text <| toString v ]
      ]

viewInput : Model -> Html Msg
viewInput model =
    div [class "input"]
        [ label [ for "input-text"
                ] [text "add"]
        , input [ onInput InputChanged
                , name "input-text"
                , type_ "text"
                , placeholder "media url"
                , value model.currentInput
                , class "mdl-textfield__input mdl-cell mdl-cell--10-col"
                ] []
        , button
              [ onClick <| PlaylistAdd model.currentInput
              , class "mdl-button mdl-js-button mdl-button--raised mdl-button--colorize mdl-cell mdl-cell--2-col"]
              [text "Add"]
        , button
              [ onClick <| TogglePlay
              , class "mdl-button mdl-js-button mdl-button--raised mdl-button--colorize mdl-cell mdl-cell--2-col"]
              [text <| if model.playing then "Pause" else "Play"]

        ]

viewVolume : Model -> Html Msg
viewVolume model =
    slider "volume" model.volume (parseIntWithDefault 0 >> VolumeChanged)

viewPosition : Model -> Html Msg
viewPosition model =
    slider "position" model.position (parseIntWithDefault 0 >> PositionChanged)

viewPlaylist : Model -> Html Msg
viewPlaylist model =
  div [class "playlist"]
    [ ul []
      (List.map viewPlaylistItem model.playlist)
    ]

viewPlaylistItem : PlaylistItem -> Html Msg
viewPlaylistItem item = li [ class ("playlist-item" ++ if item.current then " current" else "")]
                        [ span [ onClick <| PlaylistSelect item.index] [text item.sourceUrl]
                        , button [ onClick <| PlaylistRemove item.index] [text "delete"]
                        ]

viewMessage : String -> Html Msg
viewMessage msg =
  div [] [ text msg ]
