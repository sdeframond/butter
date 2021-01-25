module Document exposing
    ( Document
    , Error(..)
    , Name
    , Position(..)
    , Value(..)
    , ValueOrError
    , cellSource
    , fromList
    , get
    , insert
    , insertSheet
    , removeSheet
    , renameSheet
    , selectSheet
    , sheets
    , singleSheet
    )

import Char exposing (isUpper)
import Debug exposing (log)
import Dict as D exposing (Dict)
import Document.AST as AST exposing (AST(..), FormulaAST(..), parseCell, parseName)
import List as L
import Maybe as M
import OrderedDict as OD exposing (OrderedDict)
import Result as R
import Set exposing (Set)
import Tuple as T


type Error
    = ParsingError AST.Error
    | UndefinedNameError LocatedName
    | UndefinedIdError Id
    | TypeError String
    | CyclicReferenceError (List LocatedName)
    | UndefinedSheetError Name
    | RemovingLastSheetError Name
    | InvalidSheetNameError
    | DuplicateSheetNameError


type alias Id =
    Int


type alias SheetId =
    Int


type alias Name =
    String


type alias LocatedName =
    ( Name, Name )


type Value
    = IntValue Int
    | StringValue String


type alias ValueOrError =
    Result Error Value


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


parsedCell : Cell -> Result Error AST
parsedCell (Cell res) =
    res |> R.mapError ParsingError


type Serial
    = Serial SheetId


next : Serial -> ( Id, Serial )
next (Serial id) =
    ( id, Serial <| id + 1 )


type Document
    = Document
        { cells : Dict ( SheetId, Name ) Cell
        , sheetIds : OrderedDict Name SheetId
        , serial : Serial
        , currentSheet : Name
        }


singleSheet : Name -> Document
singleSheet name =
    let
        ( sheetId, serial ) =
            next (Serial 1)
    in
    Document
        { cells = D.empty
        , sheetIds = OD.fromList [ ( name, sheetId ) ]
        , serial = serial
        , currentSheet = name
        }


type Position a
    = Before a
    | Current a
    | After a


sheets : Document -> List (Position Name)
sheets (Document { sheetIds, currentSheet }) =
    let
        f name list =
            if name == currentSheet then
                Current name :: list

            else
                case list of
                    [] ->
                        [ After name ]

                    (Before _) :: _ ->
                        Before name :: list

                    (Current _) :: _ ->
                        Before name :: list

                    (After _) :: _ ->
                        After name :: list
    in
    OD.keys sheetIds |> L.foldr f []


selectSheet : Name -> Document -> Result Error Document
selectSheet name (Document d) =
    if OD.member name d.sheetIds then
        Ok <| Document { d | currentSheet = name }

    else
        Err (UndefinedSheetError name)


insertSheet : Name -> Document -> Document
insertSheet name doc =
    let
        ( _, newDoc ) =
            findOrCreateSheet name doc
    in
    newDoc


findOrCreateSheet : Name -> Document -> ( SheetId, Document )
findOrCreateSheet name (Document d) =
    let
        ( id, serial, sheetIds ) =
            case OD.get name d.sheetIds of
                Just id_ ->
                    ( id_, d.serial, d.sheetIds )

                Nothing ->
                    let
                        ( id_, serial_ ) =
                            next d.serial
                    in
                    ( id_, serial_, OD.insert name id_ d.sheetIds )
    in
    ( id
    , Document
        { d
            | serial = serial
            , sheetIds = sheetIds
        }
    )


removeSheet : Name -> Document -> Result Error Document
removeSheet name (Document d) =
    OD.get name d.sheetIds
        |> R.fromMaybe (UndefinedSheetError name)
        |> R.andThen
            (if L.length (OD.keys d.sheetIds) == 1 then
                always (Err (RemovingLastSheetError name))

             else
                Ok
            )
        |> R.map
            (\id ->
                let
                    sheetIds =
                        OD.remove name d.sheetIds
                in
                Document
                    { d
                        | sheetIds = sheetIds
                        , cells = d.cells |> D.filter (\( id_, _ ) _ -> id /= id_)
                        , currentSheet =
                            if d.currentSheet == name then
                                -- FIXME: we should not need `withDefault` here.
                                OD.keys sheetIds |> L.head |> M.withDefault ""

                            else
                                d.currentSheet
                    }
            )


renameSheet : Name -> Name -> Document -> Result Error Document
renameSheet name newName_ (Document d) =
    if OD.member newName_ d.sheetIds then
        Err DuplicateSheetNameError

    else
        case parseName newName_ of
            Ok newName ->
                let
                    rename_ ( n, id ) =
                        if n == name then
                            ( newName, id )

                        else
                            ( n, id )
                in
                Ok <|
                    Document
                        { d
                            | sheetIds =
                                d.sheetIds |> OD.toList |> L.map rename_ |> OD.fromList
                            , currentSheet =
                                if name == d.currentSheet then
                                    newName

                                else
                                    d.currentSheet
                        }

            Err _ ->
                Err InvalidSheetNameError


insert : Name -> String -> Document -> Document
insert cellName value (Document d_) =
    let
        ( id, Document d ) =
            findOrCreateSheet d_.currentSheet (Document d_)
    in
    case value of
        "" ->
            remove d.currentSheet cellName (Document d_)

        _ ->
            Document
                { d | cells = D.insert ( id, cellName ) (fromSource value) d.cells }


remove : Name -> Name -> Document -> Document
remove sheetName cellName doc =
    let
        (Document d) =
            doc
    in
    case OD.get sheetName d.sheetIds of
        Nothing ->
            doc

        Just id ->
            Document { d | cells = D.remove ( id, cellName ) d.cells }


fromList : Name -> List ( String, String ) -> Document
fromList sheet pairs =
    List.foldl (\( a, b ) -> insert a b) (singleSheet sheet) pairs


getCell : Name -> Name -> Document -> Result Error Cell
getCell sheetName cellName (Document { cells, sheetIds }) =
    OD.get sheetName sheetIds
        |> R.fromMaybe (UndefinedSheetError sheetName)
        |> R.andThen
            (\id ->
                D.get ( id, cellName ) cells
                    |> R.fromMaybe
                        (UndefinedNameError ( sheetName, cellName ))
            )


cellSource : Name -> Document -> Result Error String
cellSource cellName (Document d) =
    getCell d.currentSheet cellName (Document d) |> R.map source


get : Name -> Document -> ValueOrError
get name (Document d) =
    evalCell ( d.currentSheet, name ) D.empty (Document d) |> T.first


type alias Memo =
    Dict LocatedName ValueOrError


evalCell : LocatedName -> Memo -> Document -> ( ValueOrError, Memo )
evalCell name memo doc =
    evalHelp [] name memo doc


evalHelp : List LocatedName -> LocatedName -> Memo -> Document -> ( ValueOrError, Memo )
evalHelp ancestors name memo_ doc =
    let
        intBinaryOperator f memo errMsg x y =
            let
                ( xRes, xMemo ) =
                    evalFormulaAst memo x

                ( yRes, yMemo ) =
                    evalFormulaAst xMemo y

                applyOp xVal yVal =
                    case ( xVal, yVal ) of
                        ( IntValue i, IntValue j ) ->
                            Ok <| IntValue (f i j)

                        _ ->
                            Err <| TypeError errMsg

                res =
                    R.andThen (\xx -> R.andThen (\yy -> applyOp xx yy) yRes) xRes
            in
            ( res, yMemo )

        evalAst : Memo -> AST -> ( ValueOrError, Memo )
        evalAst memo ast =
            case ast of
                Formula x ->
                    evalFormulaAst memo x

                RootLiteral s ->
                    ( Ok <| StringValue s, memo )

        evalFormulaAst : Memo -> FormulaAST -> ( ValueOrError, Memo )
        evalFormulaAst memo ast =
            case ast of
                IntLiteral i ->
                    ( Ok <| IntValue i, memo )

                StringLiteral s ->
                    ( Ok <| StringValue s, memo )

                Plus x y ->
                    intBinaryOperator (+) memo "(+) works only on IntValue" x y

                Minus x y ->
                    intBinaryOperator (-) memo "(-) works only on IntValue" x y

                RelativeReference cellName ->
                    evalHelp (name :: ancestors) ( T.first name, cellName ) memo doc

                AbsoluteReference sheetName cellName ->
                    evalHelp (name :: ancestors) ( sheetName, cellName ) memo doc

        (Document { cells }) =
            doc

        memoize ( v, m ) =
            ( v, D.insert name v m )
    in
    if List.member name ancestors then
        ( Err <| CyclicReferenceError ancestors, memo_ )

    else
        case D.get name memo_ of
            Just v ->
                ( v, memo_ )

            Nothing ->
                getCell (T.first name) (T.second name) doc
                    |> R.andThen parsedCell
                    |> R.map (evalAst memo_)
                    |> (\ast_ ->
                            case ast_ of
                                Err e ->
                                    ( Err e, memo_ )

                                Ok v ->
                                    v
                       )
                    |> memoize
