module DocumentWithUndoTest exposing (..)

import Core.DocumentWithUndo as Document
import Expect
import Json.Decode
import Sheet
import Test exposing (..)


expectPastToChangeBy : Int -> (Document.Model -> Document.Model) -> Expect.Expectation
expectPastToChangeBy delta func =
    let
        doc =
            Document.init
    in
    func doc
        |> (.past >> List.length)
        |> Expect.equal (List.length doc.past + delta)


suite : Test
suite =
    describe "DocumentWithUndo"
        [ test ".commitEditedSheetNames pushes a new state when there is a change" <|
            \_ ->
                expectPastToChangeBy 1
                    (Document.updateCurrentEditedSheetName "changed" >> Document.commitEditedSheetNames)
        , test ".commitEditedSheetNames does not push a new state when there is no change" <|
            \_ ->
                expectPastToChangeBy 0
                    (Document.updateCurrentEditedSheetName "not valid" >> Document.commitEditedSheetNames)
        , test ".editCurrentSheetName does not push a new state" <|
            \_ ->
                expectPastToChangeBy 0 Document.editCurrentSheetName
        , test ".insertSheet pushes a new state" <|
            \_ ->
                expectPastToChangeBy 1 (Document.insertSheet Sheet.allParams.grid)
        , test ".merge does not push a new state" <|
            \_ ->
                expectPastToChangeBy 0 (Document.merge Document.init)
        , test ".removeSheet pushes a new state" <|
            \_ ->
                expectPastToChangeBy 1 (\d -> Document.removeSheet (Document.getCurrentSheetId d) d)
        , test ".updateCurrentEditedSheetName does not push a new state" <|
            \_ ->
                expectPastToChangeBy 0 (Document.updateCurrentEditedSheetName "changed")
        , todo ".updateCurrentSheet"
        , test ".decoder can decode .encoder's output" <|
            \_ ->
                let
                    doc =
                        Document.init
                in
                doc
                    |> Document.encode
                    |> Json.Decode.decodeValue Document.decoder
                    |> Expect.equal (Ok doc)
        , test ".fromBytes can decode .toBytes's output" <|
            \_ ->
                let
                    doc =
                        Document.init
                in
                doc
                    |> Document.toBytes
                    |> Document.fromBytes
                    |> Expect.equal (Just doc)
        ]
