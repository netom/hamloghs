# hamloghs - Ham radio log in Haskell

## Project outline

WARNING - this is NOT a useful project yet. It's heavily work in progress. Any suggestion or feature request is very welcome

HamLogHS is a set of command-line tools to manage station logs.

These tools all use the standard ADIF (ADI/ADX) format to store and transfer data.
These logs can be imported / exported using various logger applications
and websites like LotW, eQSL, and ClubLog.

HamLogHS tools also understand other file formats shuch as Cabrillo, CSV
and XML.

## Development strategy: *Do one thing, do it well*

* The aim of the project is to develop a set of tools each a with narrow featureset.
* The main theme is data file reading / writing in different formats
  * Filtering, record manipulation and aggregation also important
* The tools "speak" the same format: ADIF (ADI)
  * ADIF is an ugly format, but it's currently widely supported, hence the choice.
* The tools might be piped together to perform complicated tasks, for example:
  * Export the qso-s in the last 48 hours that affect the grids not contacted last year, and are futher away than 1000 kilometers.
  * List unconfirmed contacts counting towards my DXCC award
* The tools accept command-line arguments, and can parse environment variables. Config file parsing is not included.

## Planned tools

* Creating records
  * HL_MY_GRIDSQUARE=JN97mm hl-record --mode SSB -t now --rst-rcvd 59 --rst-sent 56 --call OM2ZJQ >> log.adi
* Merging / splitting logs stored in a directory tree, partitioned on date
  * cat log.adi | hl-import -d my_log_dir
  * hl-export -d my_log_dir --from-date=yesterday --to-date=now
* Read/write various formats
  * hl-read-adx, hd-write-adx
  * Excel XML
  * ODF spreadsheet
  * Protobuf
  * BSON
  * JSON
  * YAML
  * CSV / TSV
  * Haskell persistent compatible database
    * MySQL
    * Postgresql
    * SQLite
    * Redis
    * MongoDB
* Filtering
  * cat log.adi | hl-filter
  * Match on:
    * Equality
    * Range
    * Regular expression
* Checking / validating logs
  * hl-check < log.adi
    * Runs various checks including possible duplicates, certain file format errors
  * hl-rescue
    * Should be able to retrieve ADIF records from crashed hard drive images in reasonable time
* Various low-level tools
  * take
  * drop
  * takewhile
  * dropwhile
  * ... ?
