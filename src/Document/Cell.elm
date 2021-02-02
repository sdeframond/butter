module Document.Cell exposing (Cell, fromSource, parsed, renameSheets, source)

import Document.AST as AST exposing (AST, parseCell)
import Document.Types exposing (Error(..), Name)
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


renameSheets : (Name -> Name) -> Cell -> Cell
renameSheets f ((Cell res) as c) =
    case res of
        Err _ ->
            c

        Ok ast ->
            Cell (Ok <| AST.renameSheets f ast)
