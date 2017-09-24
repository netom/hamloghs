# HamLogHS

Amateur radio station log management software written in Haskell.

## What is this?

HamLogHS is a set of command-line tools to manage amateur radio station 
logs.

The feature set of the executables are stable enough to be useful, but 
BE VERY CAREFUL, as the application does not have any tests yet, and 
nor I, nor anyone else can ever guarantee it won't cause horrible and 
unrecoverable data loss.

***USE IT ONLY ON YOUR OWN RESPONSIBILITY***

## Project outline

### Data storage & transfer

HamLogHS tools use the standard ADIF (ADI) format to store and transfer 
data. These logs can be imported / exported using various logger 
applications and websites like LotW.arrl.org, QRZ.com, eQSL.cc, and 
ClubLog.org.

The programs should be suitable for use by hand, or by graphical or 
command-line front-ends.

See TODO.md for immediate plans.

### Why?

No program is perfect. Never will be. HamLogHS is no exception. Since 
the ADIF file format is so pervasive, it was choosen as the disk format 
and the "pipe format" - the way programs store, organize and 
communicate data.

## Development strategy: *Do one thing, do it well*

The aim of the project is to develop a set of tools each a with 
narrow featureset such as filtering and record manipulation.

The tools "speak" the same format: ADIF (ADI) ADIF is an ugly format, 
but it's currently widely supported, hence the choice.

The tools might be piped together to perform complicated tasks.

## Tools

The hl-export and hl-import commands operate on a *database*, an ADIF 
file located at ~/.hl/data/hl.adi by default. This location can be 
changed by using the --home option, or $HL_HOME environment variable.

### hl-export

Export the ADIF database to a single file.

### hl-filter

Output ADIF records matching the given filter expressions.

### hl-from-csv

Converts a CSV file to ADIF. Tag names must be given as column headers.

### hl-import

Reads ADIF files and merges them into the database.

### hl-merge

Merges multiple ADIF files into a single one. Tries to avoid 
duplication. Similar records are merged so the most data is kept. (I.e. 
it intends to do merging *the right way*). This merging logic is used
in hl-import too.

### hl-record

Creates an ADIF record from command-line arguments and environmental 
variables.

This may be used to implement simple command-line loggers. An example
can be seen in bash/.hlrc.

### hl-to-adx

Converts ADIF files to ADX.

### hl-to-csv

Converts ADIF files to CSV.

### hl-to-fods

Converts ADIF files to OpenDocument spreadsheets. This format is used 
by OpenOffice and LibreOffice and a few other office suites.

### hl-to-list

Writes a shors summary of each record in an ADIF file. Great for 
quickly inspecting your station log, one record on each line.

### hl-to-msoxml

Converts ADIF files to Microsoft Office XML format.
