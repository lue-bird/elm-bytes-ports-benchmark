port module Main exposing (..)

import AsciiPort
import Base64Port
import FilePort
import HexPort
import HttpTask
import Identity
import IntArrayPort
import IntListChunkingPort
import IntListPort
import Json.Encode
import Model exposing (Model)


port fromJS : (Json.Encode.Value -> msg) -> Sub msg


port toJS : Json.Encode.Value -> Cmd msg


type Msg
    = IdentityMsg Identity.Msg
    | FilePortMsg FilePort.Msg
    | IntArrayPortMsg IntArrayPort.Msg
    | IntListPortMsg IntListPort.Msg
    | IntListChunkingPortMsg IntListChunkingPort.Msg
    | Base64PortMsg Base64Port.Msg
    | HexPortMsg HexPort.Msg
    | AsciiPortMsg AsciiPort.Msg
    | HttpTaskMsg HttpTask.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        supdate cmsg ( m, cmd ) =
            ( m, Cmd.map cmsg cmd )
    in
    case msg of
        IdentityMsg smsg ->
            Identity.update smsg model |> supdate IdentityMsg

        FilePortMsg smsg ->
            FilePort.update smsg model |> supdate FilePortMsg

        IntArrayPortMsg smsg ->
            IntArrayPort.update smsg model |> supdate IntArrayPortMsg

        IntListPortMsg smsg ->
            IntListPort.update smsg model |> supdate IntListPortMsg

        IntListChunkingPortMsg smsg ->
            IntListChunkingPort.update smsg model |> supdate IntListChunkingPortMsg

        HexPortMsg smsg ->
            HexPort.update smsg model |> supdate HexPortMsg

        AsciiPortMsg smsg ->
            AsciiPort.update smsg model |> supdate AsciiPortMsg

        Base64PortMsg smsg ->
            Base64Port.update smsg model |> supdate Base64PortMsg

        HttpTaskMsg smsg ->
            HttpTask.update smsg model |> supdate HttpTaskMsg


subscriptions : model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Identity.subscriptions model |> Sub.map IdentityMsg
        , FilePort.subscriptions model |> Sub.map FilePortMsg
        , IntListChunkingPort.subscriptions model |> Sub.map IntListChunkingPortMsg
        , IntListPort.subscriptions model |> Sub.map IntListPortMsg
        , IntArrayPort.subscriptions model |> Sub.map IntArrayPortMsg
        , HexPort.subscriptions model |> Sub.map HexPortMsg
        , AsciiPort.subscriptions model |> Sub.map AsciiPortMsg
        , Base64Port.subscriptions model |> Sub.map Base64PortMsg
        , HttpTask.subscriptions model |> Sub.map HttpTaskMsg
        ]


main : Program {} Model Msg
main =
    Platform.worker
        { init = \_ -> ( Model.init, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }
