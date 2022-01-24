port module Ports exposing (..)

import Json.Encode exposing (Value)



-- MAIN PORTS


port setStorage : Value -> Cmd msg


port updateState : (Value -> msg) -> Sub msg


port logError : String -> Cmd msg


port blurs : (Value -> msg) -> Sub msg



-- DOC PORTS


port sendDocDiff : Value -> Cmd msg


port docDiffReceiver : (Value -> msg) -> Sub msg
