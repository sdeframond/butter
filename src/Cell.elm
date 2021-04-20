module Cell exposing (Cell, eval, fromSource, isValid, sourceView)

import Ast exposing (Ast)
import Types exposing (Error(..))


type Cell
    = Cell UserInput ParsedAst


type alias ParsedAst =
    Result Types.Error (Ast (Result OriginalSheetName Types.SheetId))


type alias UserInput =
    String


type alias OriginalSheetName =
    String


isValid : Cell -> Bool
isValid (Cell _ astResult) =
    case astResult of
        Ok _ ->
            True

        Err _ ->
            False


sourceView : (Types.SheetId -> Maybe Types.Name) -> Cell -> Maybe String
sourceView getSheetName (Cell originalSource astResult) =
    let
        referenceToString : Result OriginalSheetName Types.SheetId -> Maybe String
        referenceToString reference =
            case reference of
                Ok ref ->
                    getSheetName ref

                Err name ->
                    Just name

        astToString : Ast (Result OriginalSheetName Types.SheetId) -> Maybe String
        astToString ast =
            ast
                |> Ast.mapSheetReferences referenceToString
                |> Ast.toString
    in
    astResult
        |> Result.map astToString
        |> Result.withDefault (Just originalSource)


fromSource : (Types.Name -> Maybe Types.SheetId) -> UserInput -> Cell
fromSource getSheetId src =
    src
        |> Ast.parseCell
        |> Result.mapError (always Types.ParsingError)
        |> Result.map (Ast.mapSheetReferences (\sheetName -> getSheetId sheetName |> Result.fromMaybe sheetName))
        |> Cell src


eval : Ast.Context Types.SheetId -> Cell -> Types.ValueOrError
eval context (Cell _ astResult) =
    let
        myContext : Ast.Context (Result OriginalSheetName Types.SheetId)
        myContext =
            { resolveLocalReference = context.resolveLocalReference
            , resolveGlobalReference = resolveGlobalReference
            }

        resolveGlobalReference : ( Result OriginalSheetName Types.SheetId, Types.Name ) -> Types.ValueOrError
        resolveGlobalReference ( sheetRefIdOrSource, cellRef ) =
            sheetRefIdOrSource
                |> Result.mapError Types.UndefinedSheetError
                |> Result.andThen (\id -> context.resolveGlobalReference ( id, cellRef ))
    in
    astResult |> Result.andThen (Ast.eval myContext)
