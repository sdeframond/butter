module Cell exposing (Cell, eval, fromSource, isValid, sourceView)

import AST exposing (AST)
import Types exposing (Error(..))


type Cell
    = Cell
        -- the original string as typed by the user, in case it cannot be parsed.
        Source
        (Result
            Types.Error
            (AST
                (Result
                    -- the original sheet name, in case there is no sheet with this name.
                    SheetNameSource
                    Types.SheetId
                )
            )
        )


type alias Source =
    String


type alias SheetNameSource =
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
        referenceToString : Result SheetNameSource Types.SheetId -> Maybe String
        referenceToString reference =
            case reference of
                Ok ref ->
                    getSheetName ref

                Err name ->
                    Just name

        astToString : AST (Result SheetNameSource Types.SheetId) -> Maybe String
        astToString ast =
            ast
                |> AST.mapSheetReferences referenceToString
                |> AST.toString
    in
    astResult
        |> Result.map astToString
        |> Result.withDefault (Just originalSource)


fromSource : (Types.Name -> Maybe Types.SheetId) -> String -> Cell
fromSource getSheetId src =
    src
        |> AST.parseCell
        |> Result.mapError (always Types.ParsingError)
        |> Result.map (AST.mapSheetReferences (\sheetName -> getSheetId sheetName |> Result.fromMaybe sheetName))
        |> Cell src


eval : AST.Context Types.SheetId -> Cell -> Types.ValueOrError
eval context (Cell _ astResult) =
    let
        myContext : AST.Context (Result SheetNameSource Types.SheetId)
        myContext =
            { resolveLocalReference = context.resolveLocalReference
            , resolveGlobalReference = resolveGlobalReference
            }

        resolveGlobalReference : ( Result SheetNameSource Types.SheetId, Types.Name ) -> Types.ValueOrError
        resolveGlobalReference ( sheetRefIdOrSource, cellRef ) =
            sheetRefIdOrSource
                |> Result.mapError Types.UndefinedSheetError
                |> Result.andThen (\id -> context.resolveGlobalReference ( id, cellRef ))
    in
    astResult |> Result.andThen (AST.eval myContext)
