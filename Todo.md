
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

- Bug : accept non-ascii letter in names.
- Enabler : Remove dependency to elm-sortable-table (implement my own)
    - Benefits : to store current state in local storage
    - Benefit : to facilitate having a consistent look across sheet types
- Feature : Common look for Grid, Table and PivotTable
- Feature : Functions (which ones ?)
- Feature : Edit table field definition
- Feature : add more aggregate functions to Pivot Tables
- Feature : Cancel cell edition with Echap
- Feature : Cancel sheet edition with Echap
- Feature : CSV import
- Feature : Copy/paste (bonus : support copy-pasting from Excel)
- Feature : More operators (* , ^, /, unary +, ++ on strings) and parenthesis 
- Feature : automatically update a pivot table when its source change (dat and fields)
- Feature : Support large sheets (#row and col > 1_000_000)
- Feature : Re-order sheets
- Feature : Support floating precision numbers

- Enabler : Test coverage
    - Tried on Apr. 5, does not work out of the box.
- Debt : fork elm-pivot-table to remove dependency to elm-ui.
- Enabler : Move all modules under "Butter" namespace to prevent conflits with dependencies (eg. Table, PivotTable...)

Doing
=====

- Feature : Save to file (and load file)

Done
====

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