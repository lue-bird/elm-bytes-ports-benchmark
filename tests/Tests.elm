module Tests exposing (..)

import AsciiPort
import Bytes
import Bytes.Encode
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import HexPort
import Test exposing (..)


suite : Test
suite =
    Test.describe "bytes intermediate representation"
        [ Test.describe "ascii"
            [ Test.fuzz fuzzAsciiString
                "roundtrip over bytes"
                (\asciiString ->
                    asciiString
                        |> AsciiPort.toBytes
                        |> Debug.log "bytes encoded"
                        |> AsciiPort.fromBytes
                        |> Expect.equal (Just asciiString)
                )
            , Test.fuzz fuzzBytes
                "roundtrip over ascii string"
                (\bytes ->
                    bytes
                        |> AsciiPort.fromBytes
                        |> Maybe.map AsciiPort.toBytes
                        |> Expect.equal (Just bytes)
                )
            ]
        , Test.describe "hex"
            [ Test.fuzz fuzzHexString
                "roundtrip over bytes"
                (\hexString ->
                    hexString
                        |> HexPort.toBytes
                        |> HexPort.fromBytes
                        |> Expect.equal (Just hexString)
                )
            , Test.fuzz fuzzBytes
                "roundtrip over hex string"
                (\bytes ->
                    bytes
                        |> HexPort.fromBytes
                        |> Maybe.map HexPort.toBytes
                        |> Expect.equal (Just bytes)
                )
            ]
        ]


fuzzBytes : Fuzzer Bytes.Bytes
fuzzBytes =
    Fuzz.listOfLengthBetween 0
        30
        (Fuzz.intRange 0 255)
        |> Fuzz.map
            (\unsignedInt8s ->
                unsignedInt8s
                    |> List.map Bytes.Encode.unsignedInt8
                    |> Bytes.Encode.sequence
                    |> Bytes.Encode.encode
            )


fuzzHexString : Fuzzer String
fuzzHexString =
    Fuzz.listOfLengthBetween 0
        30
        (Fuzz.oneOfValues [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' ])
        |> Fuzz.filter (\list -> (List.length list |> Basics.remainderBy 2) == 0)
        |> Fuzz.map String.fromList


fuzzAsciiString : Fuzzer String
fuzzAsciiString =
    Fuzz.listOfLengthBetween 0
        30
        Fuzz.asciiChar
        |> Fuzz.map String.fromList
