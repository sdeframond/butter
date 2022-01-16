port module Ports exposing (..)

import Json.Encode exposing (Value)



-- PORTS


port setStorage : Value -> Cmd msg


port updateState : (Value -> msg) -> Sub msg


port logError : String -> Cmd msg


port blurs : (Value -> msg) -> Sub msg
