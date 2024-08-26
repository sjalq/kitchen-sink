module Backend.ModelImport exposing (..)

import Http
import Lamdera.Wire3 exposing (bytesDecode)
import Task exposing (Task)
import Types exposing (BackendModel)


fetchImportedModel : String -> String -> Task Http.Error BackendModel
fetchImportedModel url modelKey =
    Http.task
        { method = "POST"
        , headers =
            [ Http.header "Content-Type" "application/octet-stream"
            , Http.header "X-Lamdera-Model-Key" modelKey
            ]
        , url = url
        , body = Http.emptyBody
        , resolver =
            Http.bytesResolver <|
                \response ->
                    case response of
                        Http.GoodStatus_ _ body ->
                            case bytesDecode Types.w3_decode_BackendModel body of
                                Just model ->
                                    Ok model

                                Nothing ->
                                    Err (Http.BadBody "Bytes decode failed")

                        Http.BadStatus_ meta _ ->
                            Err (Http.BadStatus meta.statusCode)

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.BadUrl_ url_ ->
                            Err (Http.BadUrl url_)
        , timeout = Nothing
        }
