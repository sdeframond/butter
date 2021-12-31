module DocumentWithUndoTest exposing (..)

import Core.DocumentWithUndo as Document
import Expect
import Json.Decode
import Sheet
import Test exposing (..)


expectPastToChangeBy : Int -> (Document.Model -> Document.Model) -> Document.Model -> Expect.Expectation
expectPastToChangeBy delta func doc =
    func doc
        |> (Document.past >> List.length)
        |> Expect.equal (List.length (Document.past doc) + delta)


suite : Test
suite =
    let
        doc =
            Document.init
    in
    describe "DocumentWithUndo"
        [ test ".commitEditedSheetNames pushes a new state when there is a change" <|
            \_ ->
                doc
                    |> expectPastToChangeBy 1
                        (Document.updateCurrentEditedSheetName "changed" >> Document.commitEditedSheetNames)
        , test ".commitEditedSheetNames does not push a new state when there is no change" <|
            \_ ->
                doc
                    |> expectPastToChangeBy 0
                        (Document.updateCurrentEditedSheetName "not valid" >> Document.commitEditedSheetNames)
        , test ".editCurrentSheetName does not push a new state" <|
            \_ ->
                doc |> expectPastToChangeBy 0 Document.editCurrentSheetName
        , test ".insertSheet pushes a new state" <|
            \_ ->
                doc |> expectPastToChangeBy 1 (Document.insertSheet Sheet.allParams.grid)
        , test ".applyContentFrom does not push a new state" <|
            \_ ->
                doc |> expectPastToChangeBy 0 (Document.applyContentFrom Document.init)
        , test ".removeSheet does not push a new state when there is only one sheet left" <|
            \_ ->
                doc |> expectPastToChangeBy 0 (\d -> Document.removeSheet (Document.getCurrentSheetId d) d)
        , test ".removeSheet pushes a new state when there is more than one sheet" <|
            \_ ->
                doc
                    |> Document.insertSheet Sheet.allParams.grid
                    |> expectPastToChangeBy 1 (\d -> Document.removeSheet (Document.getCurrentSheetId d) d)
        , test ".updateCurrentEditedSheetName does not push a new state" <|
            \_ ->
                doc |> expectPastToChangeBy 0 (Document.updateCurrentEditedSheetName "changed")
        , todo ".updateCurrentSheet"
        , test ".decoder can decode .encoder's output" <|
            \_ ->
                doc
                    |> Document.encode
                    |> Json.Decode.decodeValue Document.decoder
                    |> Expect.equal (Ok doc)
        , test ".fromBytes can decode .toBytes's output" <|
            \_ ->
                doc
                    |> Document.toBytes
                    |> Document.fromBytes
                    |> Expect.equal (Just doc)
        ]
