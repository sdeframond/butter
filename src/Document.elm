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
    | DuplicateSheetNameError Name


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
    = Document DocData


type alias DocData =
    { cells : Dict ( SheetId, Name ) Cell
    , serial : Serial
    , sheetsBefore : OrderedDict Name SheetId
    , currentSheet : ( Name, SheetId )
    , sheetsAfter : OrderedDict Name SheetId
    }


singleSheet : Name -> Document
singleSheet name =
    let
        ( sheetId, serial ) =
            next (Serial 1)
    in
    Document
        { cells = D.empty
        , serial = serial
        , currentSheet = ( name, sheetId )
        , sheetsBefore = OD.empty
        , sheetsAfter = OD.empty
        }


findSheetIdByName : Name -> DocData -> Result Error SheetId
findSheetIdByName name { sheetsBefore, sheetsAfter, currentSheet } =
    let
        ( currentName, currentId ) =
            currentSheet
    in
    if name == currentName then
        Ok currentId

    else
        case OD.get name sheetsBefore of
            Just id ->
                Ok id

            Nothing ->
                case OD.get name sheetsAfter of
                    Just id ->
                        Ok id

                    Nothing ->
                        Err (UndefinedSheetError name)


type Position a
    = Before a
    | Current a
    | After a


sheets : Document -> List (Position Name)
sheets (Document { sheetsBefore, currentSheet, sheetsAfter }) =
    L.concat
        [ sheetsBefore |> OD.keys |> L.map Before
        , [ Current (T.first currentSheet) ]
        , sheetsAfter |> OD.keys |> L.map After
        ]


selectSheet : Name -> Document -> Result Error Document
selectSheet selectedName (Document data) =
    let
        tag pivot position list =
            case list of
                [] ->
                    []

                head :: tail ->
                    if head == pivot then
                        -- skip element and change position
                        tag pivot After tail

                    else
                        -- append to the list with the current position
                        position head :: tag pivot position tail

        sheetList =
            L.concat
                [ OD.toList data.sheetsBefore
                , [ data.currentSheet ]
                , OD.toList data.sheetsAfter
                ]

        toData sheet d =
            case sheet of
                Before ( name, id ) ->
                    { d | sheetsBefore = OD.insert name id d.sheetsBefore }

                After ( name, id ) ->
                    { d | sheetsAfter = OD.insert name id d.sheetsAfter }

                Current _ ->
                    d
    in
    findSheetIdByName selectedName data
        |> R.map
            (\id ->
                sheetList
                    |> tag ( selectedName, id ) Before
                    |> L.foldl toData
                        { data
                            | currentSheet = ( selectedName, id )
                            , sheetsBefore = OD.empty
                            , sheetsAfter = OD.empty
                        }
                    |> Document
            )


insertSheet : Name -> Document -> Result Error Document
insertSheet name (Document data) =
    let
        ( serial, sheetsAfter ) =
            let
                ( id_, serial_ ) =
                    next data.serial
            in
            ( serial_, OD.insert name id_ data.sheetsAfter )
    in
    case findSheetIdByName name data of
        Ok _ ->
            Err (DuplicateSheetNameError name)

        Err _ ->
            Ok <|
                Document
                    { data
                        | serial = serial
                        , sheetsAfter = sheetsAfter
                    }


removeSheet : Name -> Document -> Result Error Document
removeSheet name (Document d) =
    if name == T.first d.currentSheet then
        case ( OD.toList d.sheetsBefore, OD.toList d.sheetsAfter ) of
            ( _, head :: tail ) ->
                Ok <| Document { d | currentSheet = head, sheetsAfter = OD.fromList tail }

            ( head :: tail, _ ) ->
                Ok <| Document { d | currentSheet = head, sheetsBefore = OD.fromList tail }

            _ ->
                Err (RemovingLastSheetError name)

    else
        findSheetIdByName name d
            |> R.map
                (\id ->
                    Document
                        { d
                            | sheetsBefore = OD.remove name d.sheetsBefore
                            , sheetsAfter = OD.remove name d.sheetsAfter
                            , cells = d.cells |> D.filter (\( id_, _ ) _ -> id /= id_)
                        }
                )


renameSheet : Name -> Name -> Document -> Result Error Document
renameSheet name newName_ (Document d) =
    findSheetIdByName name d
        |> R.andThen
            (\id ->
                case findSheetIdByName newName_ d of
                    Ok _ ->
                        Err (DuplicateSheetNameError newName_)

                    Err _ ->
                        Ok id
            )
        |> R.andThen
            (\id ->
                case parseName newName_ of
                    Ok newName ->
                        let
                            rename ( n, id_ ) =
                                if n == name then
                                    ( newName, id_ )

                                else
                                    ( n, id_ )
                        in
                        Ok <|
                            Document
                                { d
                                    | sheetsBefore =
                                        d.sheetsBefore |> OD.toList |> L.map rename |> OD.fromList
                                    , sheetsAfter =
                                        d.sheetsAfter |> OD.toList |> L.map rename |> OD.fromList
                                    , currentSheet = rename d.currentSheet
                                }

                    Err _ ->
                        Err InvalidSheetNameError
            )


insert : Name -> String -> Document -> Document
insert cellName value (Document d) =
    let
        ( _, id ) =
            d.currentSheet
    in
    case value of
        "" ->
            Document { d | cells = D.remove ( id, cellName ) d.cells }

        _ ->
            Document
                { d | cells = D.insert ( id, cellName ) (fromSource value) d.cells }


fromList : Name -> List ( String, String ) -> Document
fromList sheet pairs =
    List.foldl (\( a, b ) -> insert a b) (singleSheet sheet) pairs


getCell : Name -> Name -> DocData -> Result Error Cell
getCell sheetName cellName data =
    findSheetIdByName sheetName data
        |> R.andThen
            (\id ->
                D.get ( id, cellName ) data.cells
                    |> R.fromMaybe (UndefinedNameError ( sheetName, cellName ))
            )


cellSource : Name -> Document -> Result Error String
cellSource cellName (Document d) =
    getCell (T.first d.currentSheet) cellName d |> R.map source


get : Name -> Document -> ValueOrError
get name (Document d) =
    evalCell ( T.first d.currentSheet, name ) D.empty d |> T.first


type alias Memo =
    Dict LocatedName ValueOrError


evalCell : LocatedName -> Memo -> DocData -> ( ValueOrError, Memo )
evalCell name memo data =
    evalHelp [] name memo data


evalHelp : List LocatedName -> LocatedName -> Memo -> DocData -> ( ValueOrError, Memo )
evalHelp ancestors name memo_ data =
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
                    evalHelp (name :: ancestors) ( T.first name, cellName ) memo data

                AbsoluteReference sheetName cellName ->
                    evalHelp (name :: ancestors) ( sheetName, cellName ) memo data

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
                getCell (T.first name) (T.second name) data
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
