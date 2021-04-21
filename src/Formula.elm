module Formula exposing (Formula, eval, fromSource, isValid, sourceView)

import Ast exposing (Ast)
import Types exposing (Error(..))


type Formula
    = Formula UserInput ParsedAst


type alias ParsedAst =
    Result Types.Error (Ast (Result OriginalSheetName Types.SheetId))


type alias UserInput =
    String


type alias OriginalSheetName =
    String


isValid : Formula -> Bool
isValid (Formula _ astResult) =
    case astResult of
        Ok _ ->
            True

        Err _ ->
            False


sourceView : (Types.SheetId -> Maybe Types.Name) -> Formula -> Maybe String
sourceView getSheetName (Formula originalSource astResult) =
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


fromSource : (Types.Name -> Maybe Types.SheetId) -> UserInput -> Formula
fromSource getSheetId src =
    src
        |> Ast.parseCell
        |> Result.mapError (always Types.ParsingError)
        |> Result.map (Ast.mapSheetReferences (\sheetName -> getSheetId sheetName |> Result.fromMaybe sheetName))
        |> Formula src


eval : Ast.Context Types.SheetId -> Formula -> Types.ValueOrError
eval context (Formula _ astResult) =
    astResult
        |> Result.andThen (Ast.filterMapReferences (\r -> Result.mapError Types.UndefinedSheetError r))
        |> Result.andThen (Ast.eval context)
