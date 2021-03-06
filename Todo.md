
Notes
=====

Table cell:
    - either a literal value
    - or the result of a column formula.
    - Table cells are not grid cells because all table cells in the same column must be evaluated from the same formula (if they are not literals).


Grid ranges :
    - Syntax : `Sheet1.A1:C20`
    - Options :
        - Deduce intermediary cells from the range bound's name
            What about names such as `FOO1:BAR2` ?

Table ranges :
    - Syntax : `MyTable.MyColumn`

Todo
====

- Enabler : replace `SetModel` Msg with `SelectSheet` Msg in order to avoid race conditions. See https://blog.realkinetic.com/the-trickiest-elm-bug-ive-ever-seen-988aff6cbbd7. 
- Enabler : make Formula.encode/decode context independent (remove the need for a `getSheetId` parameter).
  - Benefit : less coupling.
    - But : it would allow broken references to be introduced when decoding (esp. from a Diff).
  - Options :
    - Requires encoding the AST instead of the source.
    - Or replacing the formula's UserInput with the formatted source then using this directly. But it breaks when renaming a sheet.
- Enabler : Remove dependency to elm-sortable-table (implement my own)
    - Benefits : to store current state in local storage
    - Benefit : to facilitate having a consistent look across sheet types
- Feature : Common look for Grid, Table and PivotTable
- Enabler : use elm-serializer instead of encoder/decoder
- Feature : Functions (which ones ?)
- Feature : Edit table's field definitions
- Feature : add more aggregate functions to Pivot Tables
- Feature : Cancel cell edition with Echap
- Feature : Cancel sheet edition with Echap
- Feature : CSV import
- Feature : Copy/paste (bonus : support copy-pasting from Excel)
- Feature : More operators (* , ^, /, unary +, ++ on strings) and parenthesis 
- Bug : accept non-ascii letter in names.
- Feature : Support large sheets (#row and col > 1_000_000)
- Feature : Let the user close all documents
- Feature : better Name.sanitize (or different rules for document than for sheets ? Accepts dots, spaces...)
- Feature : Re-order sheets
- Feature : Support floating precision numbers
- Feature : automatically update a pivot table when its source change (dat and fields)

- Enabler : Test coverage
    - Tried on Apr. 5, does not work out of the box.
- Debt : fork elm-pivot-table to remove dependency to elm-ui.
- Enabler : Move all modules under "Butter" namespace to prevent conflits with dependencies (eg. Table, PivotTable...)

Doing
=====

- Feature : live multiplayer
- Feature : Undo/redo
  - TODO :
    - Implement undo/redo within sheets
  - Architecture :
    - Use @elm-community/undo-redo
    - Then, when we want to implement collaboration:
      - Send diffs between previous and new states
      - To be able to make diffs, replace lists with dictionnary and fractional indices https://www.figma.com/blog/realtime-editing-of-ordered-sequences/#fractional-indexing
        - watch out for the special case of ZipList: wrap together a Dict and the current key.

  - Previously explored dead end: using actions
    - Done: encode/decode actions so that the history stays when reloading
    - Done: rename sheet.

Done
====

- 2021-12-22 : Undo/redo for sheet names
- Enabler : Remove .edit in NamedAndOrderedStore
- 2021-08-30 Feature : Basic concurrent editing of same same document in two tabs.
- 2021-08-29 - Feature : PWA
- 2021-08-29 - Feature : Multi-tab/window support
- 2021-08-29 - Document name == filename
- 2021-08-26 - Name.sanitize instead of unsafeFromString
- 2021-08-21 - Multiple documents
- 2021-07-07 - Feature : Save to file (and load file)
- 2021-07-05 - Make SheetStore a generic store. Maybe by moving some more logic into ZipList ? (Like renameSheet and removeSheet)
- 2021-05-11 - Feature : Local storage
- 2021-05-07 - Enabler : encapsulate Name in order to make sure it is parsed.
- 2021-05-05 - Refactor : Move sheet selection and edition next to where the data is defined (maybe into Document ?)
- 2021-05-04 - Feature : explicitely type grid cells (similar to table fields)
- 2021-04-21 - Refactor : Move grid sheet's data into the GridSheet itself.
- 2021-04-17 - Feature : Pivot tables
- 2021-04-17 - Enabler : Use IDs instead of name in references
- 2021-03-28 - Feature : Formulas in tables
- 2021-04-05 - Fix : update references in MyTable when renaming a sheet
- 2021-04-02 - Refactor : Move SheetList into an abstract SelectionList module (or ZipList ?)
- 2021-03-31 - Enabler : Move Document.* back into the root folder.
- 2021-03-23 - Refactor : Move evaluation code into AST
- 2021-03-22 - Feature : Support for tables
- 2021-02-02 - Feature : Rename references to a sheet when renaming a sheet
- 2021-02-01 - Fix : Do not recalculate after each key stroke
- 2021-01 - Cell edition
- 2021-01 - Cell evalutation evaluation
- 2021-01 - Sheets
- 2021-01 - Cross-sheet references