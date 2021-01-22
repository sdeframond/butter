module Document exposing
    ( Document
    , Error(..)
    , Name
    , SheetError(..)
    , Value(..)
    , ValueOrError
    , empty
    , fromList
    , get
    , insert
    , insertSheet
    , removeSheet
    , renameSheet
    , sheets
    , singleSheet
    , source
    )

import Char exposing (isUpper)
import Debug exposing (log)
import Dict as D exposing (Dict)
import Document.Parser exposing (AST(..), parseCell, parseName)
import List as L
import Maybe as M
import OrderedDict as OD exposing (OrderedDict)
import Result as R
import Set exposing (Set)
import Tuple as T


type Error
    = ParsingError String
    | UndefinedNameError LocatedName
    | UndefinedIdError Id
    | TypeError String
    | CyclicReferenceError (List LocatedName)


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


type Document
    = Document
        { cells : Dict ( SheetId, Name ) String
        , sheetIds : OrderedDict Name SheetId
        , serial : Serial
        }


type Serial
    = Serial SheetId


next : Serial -> ( Id, Serial )
next (Serial id) =
    ( id, Serial <| id + 1 )


empty : Document
empty =
    let
        ( sheetId, serial ) =
            next (Serial 1)
    in
    Document
        { cells = D.empty
        , sheetIds = OD.empty
        , serial = serial
        }


singleSheet : Name -> Document
singleSheet name =
    empty |> insertSheet name


sheets : Document -> List Name
sheets (Document { sheetIds }) =
    OD.keys sheetIds


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


removeSheet : Name -> Document -> Document
removeSheet name (Document d) =
    case OD.get name d.sheetIds of
        Nothing ->
            Document d

        Just id ->
            Document
                { d
                    | sheetIds = OD.remove name d.sheetIds
                    , cells = d.cells |> D.filter (\( id_, _ ) _ -> id /= id_)
                }


type SheetError
    = InvalidSheetNameError
    | DuplicateSheetNameError


renameSheet : Name -> Name -> Document -> Result SheetError Document
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
                        }

            Err _ ->
                Err InvalidSheetNameError


insert : Name -> Name -> String -> Document -> Document
insert sheetName cellName value doc =
    let
        ( id, Document d ) =
            findOrCreateSheet sheetName doc
    in
    case value of
        "" ->
            remove sheetName cellName doc

        _ ->
            Document
                { d | cells = D.insert ( id, cellName ) value d.cells }


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
    List.foldl (\( a, b ) -> insert sheet a b) (singleSheet sheet) pairs


source : Name -> Name -> Document -> Maybe String
source sheetName cellName (Document { cells, sheetIds }) =
    OD.get sheetName sheetIds
        |> M.andThen (\id -> D.get ( id, cellName ) cells)


get : Name -> Name -> Document -> ValueOrError
get sheet name doc =
    evalCell ( sheet, name ) D.empty doc |> T.first


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
                    evalAst memo x

                ( yRes, yMemo ) =
                    evalAst xMemo y

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
                source (T.first name) (T.second name) doc
                    |> R.fromMaybe (UndefinedNameError name)
                    |> R.andThen (parseCell >> R.mapError ParsingError)
                    |> R.map (evalAst memo_)
                    |> (\ast_ ->
                            case ast_ of
                                Err e ->
                                    ( Err e, memo_ )

                                Ok v ->
                                    v
                       )
                    |> memoize
