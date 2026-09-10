# datatypes

Extra data types for GNU Octave

**Content:**

1. [About](#1-about)
2. [Documentation](#2-documentation)
3. [Reading and writing files](#3-reading-and-writing-files)
4. [Installation](#4-installation)
5. [License](#5-license)
6. [Acknowledgements](#6-acknowledgements)

## 1. About

The **datatypes** package is a collection of [classdef Classes](https://docs.octave.org/latest/classdef-Classes.html) for providing extra data types not available in core Octave.  The package was inspired by the `tablicious` package, but it was coded almost entirely from scratch with focus on MATLAB compatibility both in terms of functionality and visual presentation in Octave's terminal. To this end, **datatypes** also overloads the `disp` and `display` methods of the `cell` class, which are responsible for printing the contents of cell arrays to the terminal, so that `cell` arrays are displayed in a MATLAB-like formatted fashion.

The **datatypes** package provides `table` and `timetable` classes as well as classes for `calendarDuration`, `categorical`, `datetime`, `duration`, and `string` arrays, along with several class-related standalone functions and a number of supplementary classes. A `timetable` is a table whose rows are labelled by time rather than by name, with the resampling and alignment machinery (`retime`, `synchronize`, `lag`) that goes with it; it is accompanied by `eventtable` and `eventfilter`, which attach events to a timetable and select rows by what was happening rather than by when. The package suite runs 10,697 built-in self tests, with the reference values measured against MATLAB and baked into the tests.

The `datetime` class is based on a compiled `oct` function as the underlying workforce for the classdef, which relies on the [`date.h`](https://github.com/HowardHinnant/date) standalone header library. This decision was made for maximum speed and efficiency when handling large `datetime` arrays, which cannot be achieved by parsing the tzdata library via native Octave code.

## 2. Documentation
All classdef Classes and their respective methods are documented with [texinfo](https://www.gnu.org/software/texinfo/) format, which can be accessed from the Octave command with the `help` function.  Use dot notation to access the help of a particular method. For example:
```
help table
help table.table
help table.sortrows
```

You can also find the entire documentation of the **datatypes** package along with its classdef index at [https://pr0m1th3as.github.io/datatypes/](https://pr0m1th3as.github.io/datatypes/). Alternatively, you can build the online documentation locally using the [`pkg-octave-doc`](https://github.com/gnu-octave/pkg-octave-doc) package. Assuming both packages are installed and loaded, browse to any directory of your choice with *write* permission and run:
```
package_texi2html ("datatypes")
```

## 3. Reading and writing files

A `table` or a `timetable` can be written to a delimited text file, to an OpenDocument spreadsheet, or to an Excel workbook, and read back from any of them.

The OpenDocument route is the one that loses nothing. A CSV file carries values and nothing else, so a column of dates comes back as text and the units, descriptions and display formats you set are gone. The package writes an OpenDocument spreadsheet whose data sheets are ordinary, readable in LibreOffice like any other, and puts the type information on a hidden sheet, so the file round-trips exactly:

```
t  = datetime (2024, 1, 1) + hours ((0:2)');
TT = timetable (t, [1; 2; 3], 'VariableNames', {'reading'});
TT.Properties.VariableUnits = {'kg'};

timetable2ods (TT, 'measurements.ods');
TT2 = ods2timetable ('measurements.ods');

isequal (TT, TT2)      % true, units and row times included
```

It is also the only format of the package that carries an attached `eventtable`, on a sheet of its own.

The layout is documented in [`doc/ODS_FORMAT.md`](doc/ODS_FORMAT.md), so that a reader in another language can be written against it and so that a file can be understood by someone holding only the file. There is also a longer write-up of the problem this solves, [Why your CSV loses your data types, and what to do about it](https://pr0m1th3as.github.io/datatypes/blog/csv-and-data-types.html).

## 4. Installation

To install the latest release, you need Octave (>=11.1.0) installed on your system. Install it by typing:

  `pkg install datatypes`

Install the latest dev version from the Octave command prompt by typing

 `pkg install "https://github.com/pr0m1th3as/datatypes/archive/refs/heads/main.zip"`

Load the package by typing

  `pkg load datatypes`

**NOTICE !** Loading the **datatypes** package affects the way cell arrays are displayed in the terminal.

**WARNING !!** Unloading the **datatypes** package during an Octave session will clear the class structure table and all variables from the workspace. Make sure to save your workspace (if needed) before unloading **datatypes**.

## 5. License
All software in the **datatypes** package is published under the GNU General Public License version 3 (**GPLv3**). The package logo (found in `/doc/`) as well as all other documentation related to the **datatypes** package hosted in this repository (including this README file and the autogenerated online documentation found in `/docs/`) is licensed under a Creative Commons Attribution-ShareAlike 4.0 International license (**CC BY-SA 4.0**). The logos in the `/logos/` folder referring  to the EU Commission and the NLnet Foundation are also licensed under **CC BY-SA 4.0**

## 6. Acknowledgements
The deveopment of the **dataypes** package is currently supported through an [NGI0 Commons Fund](https://nlnet.nl/commonsfund/) granted by the [NLnet Foundation](https://nlnet.nl/) for the [New data types for GNU Octave](https://nlnet.nl/project/GNU-Octave-datatypes/) project.
<p align="center" width="100%">
<a href="https://nlnet.nl/project/GNU-Octave-datatypes/"><img src="/logos/new-data-types-for-gnu-octave.png"/></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://nlnet.nl/"><img src="/logos/NLnet-banner.png"/></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://nlnet.nl/commonsfund/"><img src="/logos/NGI0-CommonsFund.png"/></a>
</p>

The **datatypes** package was initially developed to support the necessary functionality for the [**csg-toolkit**](https://github.com/pr0m1th3as/csg-toolkit) package as part of my [RECONSTRUCT](https://www.physicalanthropology.gr/reconstruct.php) project, which has received funding from the European Union’s Horizon research and innovation funding programme under the Marie Sklodowska-Curie Grant agreement No 101104702.
<p align="center" width="100%">
<a href="https://www.physicalanthropology.gr/reconstruct.php"><img src="/logos/reconstruct.png"/></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://marie-sklodowska-curie-actions.ec.europa.eu/"><img src="/logos/MSCA.png"/></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://research-and-innovation.ec.europa.eu/funding/funding-opportunities/funding-programmes-and-open-calls/horizon-europe_en"><img src="/logos/EU-Horizon.png"/></a>
</p>
