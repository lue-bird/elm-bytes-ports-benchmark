port module IntListPort exposing (Msg, subscriptions, update)

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Json.Decode
import Json.Encode
import Model exposing (Model)
import Ports exposing (receivedBytes)
import Util


port intListFromJS : (Json.Encode.Value -> msg) -> Sub msg


port intListTriggerSend : (() -> msg) -> Sub msg


port intListToJS : Json.Encode.Value -> Cmd msg


type Msg
    = GotBytes Bytes
    | SendBytes


bytesEncoder : Bytes -> Json.Encode.Value
bytesEncoder bytes =
    Bytes.Decode.decode (Bytes.Decode.loop ( Bytes.width bytes, [] ) listStep) bytes
        |> Maybe.withDefault []
        |> Json.Encode.list Json.Encode.int


bytesDecoder : Json.Decode.Decoder Bytes
bytesDecoder =
    Json.Decode.list Json.Decode.int
        |> Json.Decode.map
            (\unsignedInt8List ->
                unsignedInt8List
                    |> List.map Bytes.Encode.unsignedInt8
                    |> Bytes.Encode.sequence
                    |> Bytes.Encode.encode
            )


listStep : ( Int, List Int ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List Int ) (List Int))
listStep ( n, xs ) =
    if n <= 0 then
        Bytes.Decode.succeed (Bytes.Decode.Done (xs |> List.reverse))

    else
        Bytes.Decode.map (\x -> Bytes.Decode.Loop ( n - 1, x :: xs ))
            Bytes.Decode.unsignedInt8


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotBytes bytes ->
            ( { model | bytes = bytes }, receivedBytes (Bytes.width bytes) )

        SendBytes ->
            ( model, intListToJS (bytesEncoder model.bytes) )


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ intListFromJS
            (\json ->
                json
                    |> Json.Decode.decodeValue bytesDecoder
                    |> Result.withDefault Util.noBytes
                    |> GotBytes
            )
        , intListTriggerSend (\_ -> SendBytes)
        ]
