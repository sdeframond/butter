Todo
====

- Feature : Save to file (and load file)
- Feature : Local storage
- Feature : Common look for Grid, Table and PivotTable
- Feature : Functions (which ones ?)
- Feature : Support large sheets (#row and col > 1_000_000)
- Feature : Edit table field definition
- Feature : Cancel cell edition with Echap
- Feature : Cancel sheet edition with Echap
- Feature : CSV import
- Feature : Copy/paste (bonus : support copy-pasting from Excel)
- Feature : More operators (* , ^, /, unary +, ++ on strings) and parenthesis 
- Feature : Re-order sheets
- Feature : Support floating precision numbers

- Refactor : Move sheet selection and edition into Document (maybe ?)
- Enabler : Use IDs instead of name in references (and a M2M relationship to translate between them)
    - Benefit : one would not have to add code to support renaming references when adding a new type of sheet/field/container.
- Enabler : Test coverage
    - Tried on Apr. 5, does not work out of the box.
- Debt : fork elm-pivot-table to remove dependency to elm-ui.
- Enabler : grid cells must be configured as formula for their content to be parsed as such
    - Benefit : remove ambiguity when content starts with "="
- Enabler : Move all modules under "Butter" namespace to prevent conflits with dependencies (eg. Table, PivotTable...)

Doing
=====

- Feature : Pivot tables
    - TODO : 
        - Create it from the source table (instead of specifying the source within the PT)
        - Garbage collect sheets by reference counting so that a PT keeps having a valid source when a table is deleted
    - QUESTION : what do to when adding/removing/renaming fields in the source table ?
        - renaming : automatically use the new name in the pivot table
        - adding : just add it to the Unused field group
        - removing : remove it from the lists even if it was used ?
- 2021-03-28 - Feature : Formulas in tables

Notes
-----

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


Done
====

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