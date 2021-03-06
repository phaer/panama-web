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
import Json.Encode as JE
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
    , toggleSettings : Bool
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
    , playing : Bool
    , index : Int
    , loading : Bool
    }
type alias Playlist = List PlaylistItem

type Msg
    = TogglePlay
    | ToggleSettings
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
    (Model "" False "" False 0 0 [], sendCommand "update")

sendJsonList : List JE.Value -> Cmd a
sendJsonList l = WebSocket.send websocket <| JE.encode 0 <| JE.list l

sendCommand : String -> Cmd a
sendCommand cmd =
    sendJsonList [JE.string cmd]

sendProperty : String -> JE.Value -> Cmd a
sendProperty n v =
    sendJsonList [JE.string n, v]

playlistDecoder : JD.Decoder Playlist
playlistDecoder =
    JD.list <| JD.map7 PlaylistItem
        (JD.maybe (JD.field "title" JD.string))
        (JD.maybe (JD.field "media_url" JD.string))
        (JD.field "source_url" JD.string)
        (JD.oneOf [JD.field "current" JD.bool, JD.succeed False])
        (JD.oneOf [JD.field "playing" JD.bool, JD.succeed False])
        (JD.field "index" JD.int)
        (JD.oneOf [JD.field "loading" JD.bool, JD.succeed False])

encodePlaylistItem : PlaylistItem -> JE.Value
encodePlaylistItem item =
    JE.object [ ("title", Maybe.withDefault JE.null (Maybe.map JE.string item.title))
              , ("media_url", Maybe.withDefault JE.null (Maybe.map JE.string item.mediaUrl))
              , ("source_url", JE.string item.sourceUrl)
              , ("current", JE.bool item.current)
              , ("playing", JE.bool item.playing)
              , ("index", JE.int item.index)
              , ("loading", JE.bool item.loading)
        ]

encodePlaylist : Playlist -> JE.Value
encodePlaylist playlist = JE.list <| List.map encodePlaylistItem playlist

decodeModel : Model -> String -> Model
decodeModel model s =
    case JD.decodeString (modelDecoder model) s of
        Ok value -> value
        Err msg -> { model | status = (Debug.log "error: " msg)}

modelDecoder : Model -> JD.Decoder Model
modelDecoder model = JD.map4 (Model model.currentInput model.toggleSettings "")
               (JD.field "playing" JD.bool)
               (JD.field "volume" JD.int)
               (JD.field "position" JD.int)
               (JD.field "playlist" playlistDecoder)

encodeModel : Model -> JE.Value
encodeModel model =
    JE.object [ ("currentInput", JE.string model.currentInput)
              , ("status", JE.string model.status)
              , ("playing", JE.bool model.playing)
              , ("volume", JE.int model.volume)
              , ("position", JE.int model.position)
              , ("playlist", encodePlaylist model.playlist)
              ]



setCurrentItem : Model -> Int -> Model
setCurrentItem m i = {m | playlist = List.indexedMap (\n p -> {p | current = n == i}) m.playlist}

isValidInput : String -> Bool
isValidInput s =
    not (String.isEmpty s)
    && String.contains "://" s

-- # Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TogglePlay ->
      ({model | playing = (not model.playing)}, sendCommand "toggle-play")

    ToggleSettings ->
      ({model | toggleSettings = (not model.toggleSettings)}, Cmd.none)

    VolumeChanged v ->
      ({model | volume = v}, JE.int v |> sendProperty "volume")

    PositionChanged p ->
      ({model | position = p}, JE.int p |> sendProperty "position")

    PlaylistChanged pl ->
      ({model | playlist = pl}, Cmd.none)

    MessageReceived m ->
      (decodeModel model m, Cmd.none)

    InputChanged i ->
      ({model | currentInput = i}, Cmd.none)

    PlaylistAdd i ->
      if
          isValidInput i
      then
          ({model | currentInput = ""}, JE.string i |> sendProperty "playlist-add")
      else
          (model, Cmd.none)

    PlaylistRemove i ->
      (model, JE.int i |> sendProperty "playlist-remove")

    PlaylistSelect i ->
      (setCurrentItem model i, JE.int i |> sendProperty "playlist-select")

-- # Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen websocket MessageReceived


-- # View
view : Model -> Html Msg
view model =
  div [ class "container" ]
      [ viewDebug model
      , viewLog model
      , viewInput model
      , viewControls model
      , viewSettings model
      , viewPlaylist model
      , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "./css/panama.css" ] []
      ]

viewDebug : Model -> Html Msg
viewDebug model =
    div [class "debug", style [("float", "right")]]
        [ pre []
              [text <| JE.encode 2 <| encodeModel model]
        ]

viewLog : Model -> Html Msg
viewLog model = div [class "log"] [text model.status]


parseIntWithDefault : Int -> String -> Int
parseIntWithDefault d = String.toInt >> Result.toMaybe >> Maybe.withDefault d


slider :  String -> Int -> (String -> a) -> Html a
slider n v handler =
    div [class <| n ++ " slider"]
        [ label [ for n
                , class "slider-label"
                ] [text n]
        , div [ class "slider-div"]
            [ input [ onInput handler
                    , name n
                    , type_ "range"
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "100"
                    , value <| toString v
                    , class "slider-input"
                    ] []
            ]
        , span [ class "value" ] [ text <| toString v ]
      ]

viewInput : Model -> Html Msg
viewInput model =
    Html.form [class "input"
              , onSubmit <| PlaylistAdd model.currentInput
              ]
        [ label [ for "input-text"
                ] [text "🔍︎"]
        , input [ onInput InputChanged
                , name "input-text"
                , type_ "text"
                , placeholder "Enter URL here…"
                , value model.currentInput
                , class "input-text"
                , autofocus True
                ] []
        , button [] [text "Go!"]
        ]

viewControls : Model -> Html Msg
viewControls model =
  div [class "controls"]
    [
      button
        [ onClick <| TogglePlay
        , class "play-toggle" ]
        [text <| if model.playing then "| |" else "▶"]
    , slider "position" model.position (parseIntWithDefault 0 >> PositionChanged)
    , slider "🔊︎" model.volume (parseIntWithDefault 0 >> VolumeChanged)
    , button
        [ onClick <| ToggleSettings ]
        [ text "⚙" ]
    ]


viewPlaylist : Model -> Html Msg
viewPlaylist model =
  let
    content = case List.isEmpty model.playlist of
      True ->
        p [] [ Html.text "No items added yet :(" ]
      False ->
        ul [] (List.map viewPlaylistItem model.playlist)
  in
    div [class "playlist"]
      [content]

viewPlaylistItemClasses : PlaylistItem -> String
viewPlaylistItemClasses item =
    ("playlist-item"
         ++ if item.current then " current" else ""
         ++ if item.loading then " loading" else "")

viewPlaylistItemText : PlaylistItem -> Html Msg
viewPlaylistItemText item =
    text <| case item.title of
                Just title -> title
                Nothing -> item.sourceUrl

viewPlaylistItem : PlaylistItem -> Html Msg
viewPlaylistItem item = li [ class <| viewPlaylistItemClasses item]
                        [ span [ onClick <| PlaylistSelect item.index] [viewPlaylistItemText item]
                        -- , span [] [text <| if item.loading then " LOADING! " else ""]
                        , a [ href item.sourceUrl, target "_blank"] [text "link"]
                        , button [ onClick <| PlaylistRemove item.index] [text "×"]
                        ]

settingToggle : String -> Bool -> a -> Html a
settingToggle t v handler =
    li [ class "setting"
       , onClick <| handler ]
       [ span [ class "setting-title" ]
              [ text t]
       , span [] [ text <| toString v ]]

viewSettings : Model -> Html Msg
viewSettings model =
    case model.toggleSettings of
        True ->
            div [ class "settings"]
                [ ul [ class "settings-list" ] [] ]
        False ->
            div [] []

viewMessage : String -> Html Msg
viewMessage msg =
  div [] [ text msg ]
