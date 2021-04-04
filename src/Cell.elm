module Cell exposing (Cell, eval, fromSource, source, updateReferences)

import AST
import Result as R
import Types exposing (Error(..), Name)


type Cell
    = Cell String


source : Cell -> String
source (Cell str) =
    str


fromSource : String -> Cell
fromSource src =
    Cell src


eval : AST.Context -> Cell -> Types.ValueOrError
eval context (Cell src) =
    AST.evalString context src


updateReferences : (Name -> Name) -> Cell -> Cell
updateReferences f ((Cell str) as c) =
    AST.updateReferences f str
        |> R.map Cell
        |> R.withDefault c
