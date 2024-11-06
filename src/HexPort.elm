port module HexPort exposing (Msg, fromBytes, subscriptions, toBytes, unsignedInt32ToHex, unsignedInt8ToHexString, update)

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


port hexFromJS : (Json.Encode.Value -> msg) -> Sub msg


port hexToJS : Json.Encode.Value -> Cmd msg


port hexTriggerSend : (() -> msg) -> Sub msg


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
            ( model, hexToJS (bytesEncoder model.bytes) )


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ hexFromJS
            (Json.Decode.decodeValue bytesDecoder
                >> Result.withDefault Util.noBytes
                >> GotBytes
            )
        , hexTriggerSend (\_ -> SendBytes)
        ]


toBytes : String -> Bytes
toBytes string =
    Encode.encode (encoder string)


encoder : String -> Encode.Encoder
encoder string =
    encodeChunks string []
        |> List.reverse
        |> Encode.sequence


{-| Big picture:

  - read 4 base64 characters
  - convert them to 3 bytes (24 bits)
  - encode these bytes

-}
encodeChunks : String -> List Encode.Encoder -> List Encode.Encoder
encodeChunks input soFar =
    case String.toList (String.left 8 input) of
        [ a, b, c, d, e, f, g, h ] ->
            encodeChunks (String.dropLeft 8 input)
                (Encode.unsignedInt32 Bytes.BE
                    (Bitwise.or
                        (Bitwise.or
                            (Bitwise.or
                                (Bitwise.shiftLeftBy 28 (hexCharToInt a))
                                (Bitwise.shiftLeftBy 24 (hexCharToInt b))
                            )
                            (Bitwise.or
                                (Bitwise.shiftLeftBy 20 (hexCharToInt c))
                                (Bitwise.shiftLeftBy 16 (hexCharToInt d))
                            )
                        )
                        (Bitwise.or
                            (Bitwise.or
                                (Bitwise.shiftLeftBy 12 (hexCharToInt e))
                                (Bitwise.shiftLeftBy 8 (hexCharToInt f))
                            )
                            (Bitwise.or
                                (Bitwise.shiftLeftBy 4 (hexCharToInt g))
                                (hexCharToInt h)
                            )
                        )
                    )
                    :: soFar
                )

        a :: b :: cUp ->
            encodeChunks (String.dropLeft 2 input)
                (Encode.unsignedInt8
                    (Bitwise.or
                        (Bitwise.shiftLeftBy 4 (hexCharToInt a))
                        (hexCharToInt b)
                    )
                    :: soFar
                )

        [] ->
            soFar

        _ ->
            soFar


hexCharToInt : Char -> Int
hexCharToInt hexChar =
    case hexChar of
        '0' ->
            0

        '1' ->
            1

        '2' ->
            2

        '3' ->
            3

        '4' ->
            4

        '5' ->
            5

        '6' ->
            6

        '7' ->
            7

        '8' ->
            8

        '9' ->
            9

        'A' ->
            10

        'a' ->
            10

        'B' ->
            11

        'b' ->
            11

        'C' ->
            12

        'c' ->
            12

        'D' ->
            13

        'd' ->
            13

        'E' ->
            14

        'e' ->
            14

        --'F'
        _ ->
            15



--


fromBytes : Bytes -> Maybe String
fromBytes bytes =
    Decode.decode (decoder (Bytes.width bytes)) bytes


decoder : Int -> Decode.Decoder String
decoder width =
    Decode.loop { remaining = width, string = "" } loopHelp


loopHelp : { remaining : Int, string : String } -> Decode.Decoder (Decode.Step { remaining : Int, string : String } String)
loopHelp { remaining, string } =
    if remaining >= 16 then
        Decode.map5
            (\a b c d e ->
                Decode.Loop
                    { remaining = remaining - 16
                    , string =
                        string
                            ++ unsignedInt32ToHex a
                            ++ unsignedInt32ToHex b
                            ++ unsignedInt32ToHex c
                            ++ unsignedInt32ToHex d
                            ++ unsignedInt32ToHex e
                    }
            )
            decodeUnsignedInt32BE
            decodeUnsignedInt32BE
            decodeUnsignedInt32BE
            decodeUnsignedInt32BE
            decodeUnsignedInt32BE

    else if remaining == 0 then
        Decode.succeed (Decode.Done string)

    else
        Decode.map
            (\a ->
                Decode.Loop
                    { remaining = remaining - 1
                    , string = string ++ unsignedInt8ToHexString a
                    }
            )
            Decode.unsignedInt8


unsignedInt4ToHexString : Int -> String
unsignedInt4ToHexString n =
    case n of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "A"

        11 ->
            "B"

        12 ->
            "C"

        13 ->
            "D"

        14 ->
            "E"

        -- 15
        _ ->
            "F"


decodeUnsignedInt32BE : Decode.Decoder Int
decodeUnsignedInt32BE =
    Decode.unsignedInt32 Bytes.BE


unsignedInt32ToHex : Int -> String
unsignedInt32ToHex bits =
    unsignedInt4ToHexString (Bitwise.and (Bitwise.shiftRightZfBy 28 bits) 0x0F)
        ++ unsignedInt4ToHexString (Bitwise.and (Bitwise.shiftRightZfBy 24 bits) 0x0F)
        ++ unsignedInt4ToHexString (Bitwise.and (Bitwise.shiftRightZfBy 20 bits) 0x0F)
        ++ unsignedInt4ToHexString (Bitwise.and (Bitwise.shiftRightZfBy 16 bits) 0x0F)
        ++ unsignedInt4ToHexString (Bitwise.and (Bitwise.shiftRightZfBy 12 bits) 0x0F)
        ++ unsignedInt4ToHexString (Bitwise.and (Bitwise.shiftRightZfBy 8 bits) 0x0F)
        ++ unsignedInt4ToHexString (Bitwise.and (Bitwise.shiftRightZfBy 4 bits) 0x0F)
        ++ unsignedInt4ToHexString (Bitwise.and bits 0x0F)


unsignedInt8ToHexString : Int -> String
unsignedInt8ToHexString bits =
    unsignedInt4ToHexString (Bitwise.and (Bitwise.shiftRightZfBy 4 bits) 0x0F)
        ++ unsignedInt4ToHexString (Bitwise.and bits 0x0F)
