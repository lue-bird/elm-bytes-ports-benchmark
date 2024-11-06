port module IntListChunkingPort exposing (Msg, subscriptions, update)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Json.Decode
import Json.Encode
import Model exposing (Model)
import Ports exposing (receivedBytes)
import Util


port intListChunkingFromJS : (Json.Encode.Value -> msg) -> Sub msg


port intListChunkingTriggerSend : (() -> msg) -> Sub msg


port intListChunkingToJS : Json.Encode.Value -> Cmd msg


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
                    |> unsignedInt8ListToChunkedBytesList []
                    |> Bytes.Encode.sequence
                    |> Bytes.Encode.encode
            )


unsignedInt8ListToChunkedBytesList : List Bytes.Encode.Encoder -> List Int -> List Bytes.Encode.Encoder
unsignedInt8ListToChunkedBytesList soFar unsignedInt8List =
    case unsignedInt8List of
        b0 :: b1 :: b2Up ->
            unsignedInt8ListToChunkedBytesList
                (Bytes.Encode.signedInt16 Bytes.LE
                    (b0
                        |> Bitwise.or (Bitwise.shiftLeftBy 8 b1)
                    )
                    :: soFar
                )
                b2Up

        b0 :: b1Up ->
            unsignedInt8ListToChunkedBytesList (Bytes.Encode.unsignedInt8 b0 :: soFar) b1Up

        [] ->
            soFar |> List.reverse


listStep : ( Int, List Int ) -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List Int ) (List Int))
listStep ( n, xs ) =
    if n >= 20 then
        Bytes.Decode.map5
            (\b0 b1 b2 b3 rest ->
                Bytes.Decode.Loop
                    ( n - 37
                    , Bitwise.and 0xFF000000 b3
                        :: Bitwise.and 0x00FF0000 b3
                        :: Bitwise.and 0xFF00 b3
                        :: Bitwise.and 0xFF b3
                        :: Bitwise.and 0xFF000000 b2
                        :: Bitwise.and 0x00FF0000 b2
                        :: Bitwise.and 0xFF00 b2
                        :: Bitwise.and 0xFF b2
                        :: Bitwise.and 0xFF000000 b1
                        :: Bitwise.and 0x00FF0000 b1
                        :: Bitwise.and 0xFF00 b1
                        :: Bitwise.and 0xFF b1
                        :: Bitwise.and 0xFF000000 b0
                        :: Bitwise.and 0x00FF0000 b0
                        :: Bitwise.and 0xFF00 b0
                        :: Bitwise.and 0xFF b0
                        :: xs
                    )
            )
            (Bytes.Decode.unsignedInt32 Bytes.BE)
            (Bytes.Decode.unsignedInt32 Bytes.BE)
            (Bytes.Decode.unsignedInt32 Bytes.BE)
            (Bytes.Decode.unsignedInt32 Bytes.BE)
            (Bytes.Decode.unsignedInt32 Bytes.BE)

    else if n <= 0 then
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
            ( model, intListChunkingToJS (bytesEncoder model.bytes) )


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ intListChunkingFromJS
            (\json ->
                json
                    |> Json.Decode.decodeValue bytesDecoder
                    |> Result.withDefault Util.noBytes
                    |> GotBytes
            )
        , intListChunkingTriggerSend (\_ -> SendBytes)
        ]
