# The `datatypes` OpenDocument spreadsheet format

How the `datatypes` package for GNU Octave stores a `table` or a `timetable`
in an OpenDocument spreadsheet, so that the file round-trips without losing
variable types, formats, units, descriptions, row labels or an attached event
table.

**This is a description of what the package writes, not a standard.** It is
versioned with the package and may change between releases. It is published so
that a reader in another language can be written against it, and so that a
file can be understood by someone holding only the file.

Written for `datatypes` 1.4.0.

## Why a spreadsheet

CSV carries values and nothing else: a column of dates comes back as text, and
a variable's unit, description and display format are gone. Binary columnar
formats such as Parquet carry types faithfully but cannot be opened by a person
without tooling. OpenDocument is an OASIS standard, it is what LibreOffice
writes natively, and a sheet of it can be read by hand.

The cost is that a spreadsheet has no place to put a table's metadata. This
format puts it on a hidden sheet, so a reader that knows nothing of the
convention still sees the data, and one that does gets the types back.

Both packaged (`.ods`) and flat (`.fods`) OpenDocument files are written and
read.

## File layout

A file holds one or more **data sheets** and exactly one **metadata sheet**
named `__datatypes_meta__`.

A data sheet is ordinary. Its first row holds the column headers and each
following row holds one row of the table. A `datetime` is written in ISO 8601
form, a `duration` as a clock, and everything else as its natural
representation. Nothing on a data sheet is specific to this format, which is
the point: opened in a spreadsheet application it looks like a table.

For a `timetable` the first column is the row times, headed by the row
dimension name. For a `table` with row names the first column is those names,
headed likewise.

The metadata sheet is excluded from every sheet enumeration the package
performs, so a caller asking for "the first sheet" gets the first sheet of
data.

## The metadata sheet

A grid of text cells, read top to bottom. It has two parts: an optional
**preamble** of keyword lines, then one **section** per data sheet.

### Sections

A section opens with a marker row whose first cell is `## Sheet: <name>`,
naming the data sheet it describes. A file holding a single table may omit the
markers, in which case the whole grid is one unnamed section.

The marker is followed by a comment row carrying four counts:

```
# varTypes 1 rows; varNames 1 rows; varDescriptions 1 rows; varUnits 1 rows.
```

and then that many rows of each block, in that order, one column per variable:

* **types**, always present;
* **descriptions**, omitted when no variable has one;
* **units**, omitted when no variable has one.

Variable **names** live on the data sheet, in its header row. The `varNames`
count records how many rows of that sheet they occupy, which is more than one
only for nested variables.

### The type block

One cell per variable, holding the Octave class name. A class that carries a
display format writes it after a vertical bar:

```
datetime|dd-MMM-uuuu HH:mm:ss
duration|h
double
cell
```

The first cell of the type row describes the label column rather than a
variable:

* `RowNames` for a table's row names;
* `RowTimes|<class>|<format>` for a timetable's row times;
* empty when the sheet has neither.

A nested variable writes one type row per level of nesting, reading down the
column, which is why `varTypes` may count more than one row:

```
# varTypes 2 rows; varNames 2 rows; varDescriptions 0 rows; varUnits 0 rows.
double | table  | table
       | double | double
```

### Worked example

A timetable of three rows and two variables, one of them carrying a unit and a
description, with an event table attached:

```
## Events crossref: | Sheet1 | Sheet1_Events | EventLabels |  |
## Sheet: Sheet1
# varTypes 1 rows; varNames 1 rows; varDescriptions 1 rows; varUnits 1 rows.
RowTimes|datetime|dd-MMM-uuuu HH:mm:ss | double  | cell
                                       | a value |
                                       | kg      |
## Sheet: Sheet1_Events
# varTypes 1 rows; varNames 1 rows; varDescriptions 0 rows; varUnits 0 rows.
RowTimes|datetime|dd-MMM-uuuu HH:mm:ss | cell
```

## Event tables

An event table is a timetable of events that attaches to another timetable.
It is written as **an ordinary data sheet with a section of its own**, and a
line in the preamble records the attachment.

```
## Events crossref: | Sales 2024 | Sales 2024 Events | Labels | Lengths | Ends
```

The cells are, in order: the keyword, the name of the sheet that carries the
timetable, the name of the sheet that carries its event table, and the three
variable designations of the event table. A designation that is not set is
written as an empty cell. The names occupy separate cells rather than one
string because a sheet name may contain spaces.

The designations are on this line because no other part of the format records
them: the metadata sheet holds types, names, descriptions and units, and an
event table's three designations are none of those. Without them the event
table returns with its designations cleared, which is a silent loss.

One line per timetable that carries events. A timetable without events writes
no line, so a file with no events has no preamble at all.

An attached event table with zero rows is a real state, distinct both from no
event table and from a detached timetable, and it survives the file: its sheet
and its preamble line are written like any other.

### Rules a reader must follow

* **A referenced sheet is consumed, not returned on its own.** It attaches to
  the timetable that names it and produces no separate result. Returning it
  twice would put the same event table in two places, and a caller editing one
  copy would write back a file whose halves disagree.
* **One event sheet may be referenced by more than one timetable.** The writer
  emits one sheet per timetable, but a hand-written file may share one, and it
  is read as written. Each referring timetable gets its own copy.
* **The event table is validated against every timetable it attaches to**, not
  once per sheet. A sheet valid for one referrer may be invalid for another.
  The row times of the two must be of the same class.
* **A file carrying a preamble must be sectioned.** An unsectioned grid is
  taken whole, so a preamble line would be read as metadata. Events imply at
  least two sheets, so this is automatic in practice.
* **An unrecognised `## <keyword>:` preamble line must be skipped**, so that a
  later addition does not break a reader written against this document.

A reader should refuse a reference naming a sheet the file does not hold, a
preamble line whose referring sheet has no section, a referenced sheet whose
row times are of a different class than the referring timetable's, and a
referenced sheet that carries a reference of its own. An event table cannot
carry an event table.

### Choosing a sheet

Asked for no particular sheet, a table reader returns the first data sheet and
a timetable reader returns the first data sheet that is not named as an event
table by a preamble line, so that a file holding one timetable and its events
reads with no argument.

A sheet asked for by number indexes every data sheet, event sheets included,
so that one number means one sheet whichever reader is used and matches what a
spreadsheet application shows. Only the default does the excluding. An event
sheet asked for by name comes back as an event table, which is what it is.

## Why the preamble is at the top

Anything appended to the metadata grid is absorbed into the last section and
corrupts it, silently in some shapes and with an error in others, because a
section runs from its marker to the next marker or to the end of the grid.
A row above the first marker belongs to no section and is invisible to a reader
that predates the feature. That is the whole reason for the position.

Four alternatives were tried and rejected: extra rows at the end of the
referring sheet's own section, which is the same corruption; a fifth field in
the counts row, which changes the header of every file the package writes and
breaks old readers on files that carry no events; a second hidden sheet, which
readers that predate it count as data; and a naming convention such as `Foo`
and `Foo_Events`, which breaks the moment a user renames a sheet, and being
renameable in a spreadsheet application is half the point of the format.

## Stability

The format is versioned with the package. The preamble mechanism is designed
so that a new keyword can be added without breaking a reader written against
this document, provided that reader skips keywords it does not recognise.

Questions and corrections: https://github.com/pr0m1th3as/datatypes/issues
