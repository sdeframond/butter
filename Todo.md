TODO
====

- Feature : Formulas in tables
- Feature : Pivot tables
- Feature : Save to file (and load file)
- Feature : Local storage
- Feature : Functions (which ones ?)
- Feature : Support large sheets (#row and col > 1_000_000)
- Feature : Cancel cell edition with Echap
- Feature : Cancel sheet edition with Echap
- Feature : CSV import
- Feature : Copy/paste (bonus : suport copy-parting from Excel)
- Feature : More operators (* , ^, /, unary +, ++ on strings) and parenthesis 
- Feature : Re-order sheets
- Feature : Support floating precision numbers

- Refactor : Move SheetList into an abstract SelectionList module (or ZipList ?)
- Refactor : Move sheet selection and edition into Document (maybe ?)

DOING
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


DONE
====

- 2021-03-23 - Refactor : Move evaluation code into AST
- 2021-03-22 - Feature : Support for tables
- 2021-02-02 - Feature : Rename references to a sheet when renaming a sheet
- 2021-02-01 - Fix : Do not recalculate after each key stroke
- 2021-01 - Cell edition
- 2021-01 - Cell evalutation evaluation
- 2021-01 - Sheets
- 2021-01 - Cross-sheet references