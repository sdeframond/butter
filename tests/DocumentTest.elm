module DocumentTest exposing (suite)

import Core.Document as Document
import Expect
import Name
import Test exposing (..)


suite : Test
suite =
    describe "Core.Document"
        [ test ".cancelEdits clears current edit status" <|
            \_ ->
                Document.init
                    |> Document.editCurrentSheetName
                    |> Document.cancelEdits
                    |> Document.getCurrentSheetEditStatus
                    |> Expect.equal Nothing
        , test ".cancelEdits does not change current sheet name" <|
            \_ ->
                Document.init
                    |> Document.updateCurrentEditedSheetName "changed"
                    |> Document.cancelEdits
                    |> Document.getCurrentSheetName
                    |> Name.toString
                    |> Expect.equal "Sheet"
        , test ".commitEditedSheetNames clears current edit status" <|
            \_ ->
                Document.init
                    |> Document.editCurrentSheetName
                    |> Document.commitEditedSheetNames
                    |> Document.getCurrentSheetEditStatus
                    |> Expect.equal Nothing
        , test ".commitEditedSheetNames changes current sheet name when the name is valid" <|
            \_ ->
                Document.init
                    |> Document.updateCurrentEditedSheetName "changed"
                    |> Document.commitEditedSheetNames
                    |> Document.getCurrentSheetName
                    |> Name.toString
                    |> Expect.equal "changed"
        , test ".commitEditedSheetNames does not change current sheet name when the name is invalid" <|
            \_ ->
                Document.init
                    |> Document.updateCurrentEditedSheetName "not valid"
                    |> Document.commitEditedSheetNames
                    |> Document.getCurrentSheetName
                    |> Name.toString
                    |> Expect.equal "Sheet"
        , test ".editCurrentSheetName sets current sheet edit status to its name" <|
            \_ ->
                Document.init
                    |> Document.editCurrentSheetName
                    |> Document.getCurrentSheetEditStatus
                    |> Expect.equal (Just "Sheet")
        , test ".updateCurrentEditedSheetName sets current sheet's edit status" <|
            \_ ->
                Document.init
                    |> Document.updateCurrentEditedSheetName "toto"
                    |> Document.getCurrentSheetEditStatus
                    |> Expect.equal (Just "toto")
        , todo ".eval"
        , todo ".insertSheet"
        , todo ".merge takes the local document's view"
        , todo ".merge takes the remote document's content"
        , todo ".removeSheet"
        , todo ".updateCurrentSheet"
        , todo ".zipMapSheets"
        ]
