module Cell exposing (Cell, fromSource, parsed, updateReferences, source)

import AST exposing (AST, parseCell)
import Types exposing (Error(..), Name)
import Result as R


type Cell
    = Cell (Result AST.Error AST)


source : Cell -> String
source (Cell res) =
    case res of
        Ok ast ->
            AST.toString ast

        Err (AST.Error src _) ->
            src


fromSource : String -> Cell
fromSource src =
    Cell <| parseCell src


parsed : Cell -> Result Error AST
parsed (Cell res) =
    res |> R.mapError (always ParsingError)


updateReferences : (Name -> Name) -> Cell -> Cell
updateReferences f ((Cell res) as c) =
    case res of
        Err _ ->
            c

        Ok ast ->
            Cell (Ok <| AST.updateReferences f ast)
