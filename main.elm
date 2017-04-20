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
import Http

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
    , searchResults : List SearchItem
    , playing : Bool
    , volume : Int
    , position : Int
    , playlist : List PlaylistItem
    }

type alias SearchItem =
    { title : String
    , sourceUrl : String
    , thumbnail : Maybe String
    , content : Maybe String
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
    | VolumeChanged Int
    | PositionChanged Int
    | PlaylistChanged (List PlaylistItem)
    | MessageReceived String
    | InputChanged String
    | PlaylistAdd String
    | PlaylistRemove Int
    | PlaylistSelect Int
    | SearchChanged (Result Http.Error (List SearchItem))


-- # Init
init : (Model, Cmd Msg)
init =
    (Model "" "" [] False 0 0 [], sendCommand "update")

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

searchDecoder : JD.Decoder (List SearchItem)
searchDecoder =
    JD.field "results" <| JD.list <| JD.map4 SearchItem
        (JD.field "title" JD.string)
        (JD.field "url" JD.string)
        (JD.maybe (JD.field "thumbnail" JD.string))
        (JD.maybe (JD.field "content" JD.string))

getSearchResults : String -> Cmd Msg
getSearchResults query =
  if
    query == "" || String.startsWith "http" query
  then
    Cmd.none
  else
    let
      url = "https://searx.gotrust.de/?categories=videos&engines=vimeo,youtube&format=json&q=" ++ query
    in
      Http.send SearchChanged (Http.get url searchDecoder)

decodeModel : String -> Model
decodeModel s =
    case JD.decodeString modelDecoder s of
        Ok value -> value
        Err msg -> Model "" (Debug.log "error: " msg) [] False 0 0 []

modelDecoder : JD.Decoder Model
modelDecoder = JD.map4 (Model "" "" [])
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


setCurrentItem: Model -> Int -> Model
setCurrentItem m i = {m | playlist = List.indexedMap (\n p -> {p | current = n == i}) m.playlist}


-- # Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    TogglePlay ->
      ({model | playing = (not model.playing)}, sendCommand "toggle-play")

    VolumeChanged v ->
      ({model | volume = v}, JE.int v |> sendProperty "volume")

    PositionChanged p ->
      ({model | position = p}, JE.int p |> sendProperty "position")

    PlaylistChanged pl ->
      ({model | playlist = pl}, Cmd.none)

    MessageReceived m ->
      (decodeModel m, Cmd.none)

    InputChanged i ->
      ({model | currentInput = i}, getSearchResults i)

    PlaylistAdd i ->
      ({model | currentInput = ""}, JE.string i |> sendProperty "playlist-add")

    PlaylistRemove i ->
      (model, JE.int i |> sendProperty "playlist-remove")

    PlaylistSelect i ->
      (setCurrentItem model i, JE.int i |> sendProperty "playlist-select")

    SearchChanged (Ok s) ->
      ({model | searchResults = s}, Cmd.none)

    SearchChanged (Err _) ->
      (model, Cmd.none)


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
      , viewSearch model
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
                ] [text "🔍"]
        , input [ onInput InputChanged
                , name "input-text"
                , type_ "text"
                , placeholder "Search or enter URL here…"
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
        , class <| if model.playing then "pause" else "play" ]
        [text <| if model.playing then "| |" else "▶"]
    , slider "position" model.position (parseIntWithDefault 0 >> PositionChanged)
    , slider "🔊" model.volume (parseIntWithDefault 0 >> VolumeChanged)
    ]

viewSearch : Model -> Html Msg
viewSearch model =
  div [class "search-results"]
    [ ul []
      (List.map viewSearchItem model.searchResults)
    ]

viewSearchItem : SearchItem -> Html Msg
viewSearchItem item =
  let
    thumbnail = case item.thumbnail of
      Nothing ->
        ""

      Just val ->
        val
    content = case item.content of
      Nothing ->
        ""

      Just val ->
        val

  in
    li []
      [ img [ onClick <| PlaylistAdd item.sourceUrl
            , src thumbnail
            ] []
      , div [] [ p [ onClick <| PlaylistAdd item.sourceUrl
                   , class "search-item-title"] [text item.title]
               , p [ onClick <| PlaylistAdd item.sourceUrl] [text content]
               ]
      , a [ href item.sourceUrl, target "_blank"] [text "link"]
      ]

viewPlaylist : Model -> Html Msg
viewPlaylist model =
  let
    content = case List.isEmpty model.playlist of
      True ->
        p [] [ Html.text "No items added yet :(" ]
      False ->
        ul []
          (List.map viewPlaylistItem model.playlist)
  in
    div [class "playlist"]
      [ content ]

viewPlaylistItemClasses : PlaylistItem -> String
viewPlaylistItemClasses item =
    ("playlist-item"
         ++ if item.current then " current" else ""
         ++ if item.loading then " loading" else "")

viewPlaylistItem : PlaylistItem -> Html Msg
viewPlaylistItem item = li [ class <| viewPlaylistItemClasses item]
                        [ span [ onClick <| PlaylistSelect item.index] [text item.sourceUrl]
                        -- , span [] [text <| if item.loading then " LOADING! " else ""]
                        , a [ href item.sourceUrl, target "_blank"] [text "link"]
                        , button [ onClick <| PlaylistRemove item.index] [text "×"]
                        ]

viewMessage : String -> Html Msg
viewMessage msg =
  div [] [ text msg ]
