Changes in Version 1.5.0 (2017-11-29)
--------------------------------------------------------

* `ParseUserEnteredTable` and `ParseAsDataFrame` now default using a 
value of `NULL` (i.e. unknown if date strings are in U.S. or
international format) for the`us.format` argument (DS-1685)


Changes in Version 1.2.0 (2017-10-20)
--------------------------------------------------------

* The functions `RemoveByName`, `RemoveRowsAndOrColumns`,
and `RetainedRowsAndOrColumns` have been moved to 
flipTables v2.1.0 (DS-1471)


Changes in Version 1.1.3 (2017-10-18)
--------------------------------------------------------

NEW FEATURES

* Added function `asNumericList`, which replaces ListToDataFrame and generalizes it
    to permit it to return lists (if the input is a list).
* Added `AsNumeric` method for lists
* Replaced usage of `flipTime::ParseDateTime` with `flipTime::AsDateTime`


Changes in Version 1.1.0 (2017-10-12)
--------------------------------------------------------

NEW FEATURES

* Begin using semantic versioning
* New function `RemoveByName` which allows removing entries from a
  vector, list, or data.frame by names while perserving attributes and allowing names
  to be specified as a string of comma or semi-colon separated names (DS-1581)

BUG FIXES

* `RemoveRowsAndOrColumns` now copies all attributes of the input and
  ensures they are returned in the output (DS-1565)
