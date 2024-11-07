port module AsciiPort exposing (Msg, fromBytes, subscriptions, toBytes, update)

import Array exposing (Array)
import Bitwise
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Json.Decode
import Json.Encode
import Model exposing (Model)
import Ports exposing (receivedBytes)
import Util


port asciiFromJS : (Json.Encode.Value -> msg) -> Sub msg


port asciiToJS : Json.Encode.Value -> Cmd msg


port asciiTriggerSend : (() -> msg) -> Sub msg


type Msg
    = GotBytes Bytes
    | SendBytes


bytesEncoder : Bytes -> Json.Encode.Value
bytesEncoder bytes =
    fromBytes bytes
        |> Maybe.withDefault ""
        |> Json.Encode.string


bytesDecoder : Json.Decode.Decoder Bytes
bytesDecoder =
    Json.Decode.string
        |> Json.Decode.map toBytes


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        GotBytes bytes ->
            ( { model | bytes = bytes }, receivedBytes (Bytes.width bytes) )

        SendBytes ->
            ( model, asciiToJS (bytesEncoder model.bytes) )


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ asciiFromJS
            (Json.Decode.decodeValue bytesDecoder
                >> Result.withDefault Util.noBytes
                >> GotBytes
            )
        , asciiTriggerSend (\_ -> SendBytes)
        ]


toBytes : String -> Bytes
toBytes string =
    Encode.encode (encoder string)


encoder : String -> Encode.Encoder
encoder string =
    encodeChunks string []
        |> List.reverse
        |> Encode.sequence


encodeChunks : String -> List Encode.Encoder -> List Encode.Encoder
encodeChunks input soFar =
    case String.toList (String.slice 0 4 input) of
        [ a, b, c, d ] ->
            encodeChunks (String.dropLeft 4 input)
                (Encode.unsignedInt32 Bytes.BE
                    (Bitwise.or
                        (Bitwise.or
                            (Bitwise.shiftLeftBy 24 (asciiCharToInt a))
                            (Bitwise.shiftLeftBy 16 (asciiCharToInt b))
                        )
                        (Bitwise.or
                            (Bitwise.shiftLeftBy 8 (asciiCharToInt c))
                            (asciiCharToInt d)
                        )
                    )
                    :: soFar
                )

        a :: bUp ->
            encodeChunks (String.dropLeft 1 input)
                (Encode.unsignedInt8
                    (asciiCharToInt a)
                    :: soFar
                )

        [] ->
            soFar


asciiCharToInt : Char -> Int
asciiCharToInt asciiChar =
    Char.toCode asciiChar



--


fromBytes : Bytes -> Maybe String
fromBytes bytes =
    Decode.decode (decoder (Bytes.width bytes)) bytes


decoder : Int -> Decode.Decoder String
decoder width =
    Decode.loop { remaining = width, string = "" } loopHelp


loopHelp : { remaining : Int, string : String } -> Decode.Decoder (Decode.Step { remaining : Int, string : String } String)
loopHelp { remaining, string } =
    if remaining >= 4 then
        Decode.map
            (\chunk ->
                Decode.Loop
                    { remaining = remaining - 4
                    , string =
                        string ++ unsignedInt32ToAsciiString chunk
                    }
            )
            decodeUnsignedInt32BE

    else if remaining == 0 then
        Decode.succeed (Decode.Done string)

    else
        Decode.map
            (\a ->
                Decode.Loop
                    { remaining = remaining - 1
                    , string = string ++ unsignedInt8ToAsciiString a
                    }
            )
            Decode.unsignedInt8


decodeUnsignedInt32BE : Decode.Decoder Int
decodeUnsignedInt32BE =
    Decode.unsignedInt32 Bytes.BE


unsignedInt32ToAsciiString : Int -> String
unsignedInt32ToAsciiString bits =
    unsignedInt8ToAsciiString (Bitwise.and (Bitwise.shiftRightZfBy 24 bits) 0xFF)
        ++ unsignedInt8ToAsciiString (Bitwise.and (Bitwise.shiftRightZfBy 16 bits) 0xFF)
        ++ unsignedInt8ToAsciiString (Bitwise.and (Bitwise.shiftRightZfBy 8 bits) 0xFF)
        ++ unsignedInt8ToAsciiString (Bitwise.and bits 0xFF)


unsignedInt8ToAsciiString : Int -> String
unsignedInt8ToAsciiString bits =
    String.fromChar (Char.fromCode bits)
