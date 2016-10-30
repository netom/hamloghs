# HamLogHS

A Ham radio logger written in Haskell.

## What is this?

HamLogHS is a set of command-line tools to manage amateur radio station logs.

The feature set of the few basic executables are stable enough to start experimenting with it, but BE VERY CAREFUL, as the 
application does not have any test yet, and nor I, nor anyone can every guarantee that it won't cause a horrible and 
unrecoverable data loss.

***USE IT ONLY ON YOUR OWN RESPONSIBILITY***

## Project outline

### TODO

There is really a *lot* of thigs to be done. For an immediate list of how you can help, see TODO.md.

### Data storage & transfer

HamLogHS tools all use the standard ADIF (ADI) format to store and transfer data. These logs can be imported / exported using 
various logger applications and websites like LotW.arrl.org, QRZ.com, eQSL.cc, and ClubLog.org.

The programs should be suitable for use by hand, or by graphical or command-line front-ends.

The most important goal is to provide such binaries.

### Library

HamLogHS will - at some distant point in time - will provide libraries to read and write ADIF data.

This is the secondary goal.

### Why?

No program is perfect. Never will be. HamLogHS is no exception. Since the ADIF file format is so pervasive, it was choosen as
the disk format and the "pipe format" - the way programs store, organize and communicate data.

I admit I *hate* ADIF format with a passion. (To see why, see my blog article "Musings on the ADIF file format": )

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

See TODO.md for immediate plans.

## Planned tools

* Creating records
  * HL_T_MY_GRIDSQUARE=JN97mm hl-record --mode SSB --rst-rcvd 59 --rst-sent 56 --call OM2ZJQ >> log.adi
* Merging / splitting logs stored in a directory tree, partitioned on date
  * cat log.adi | hl-import -d my_log_dir
  * hl-export -d my_log_dir --from-date=yesterday --to-date=now
  * hl-merge to_import.adi my_other_log.adi > merged_log.adi
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
