Changes in Version 1.6.4 (2018-01-18)
--------------------------------------------------------

BUG FIXES

* `ParseAsDataFrame` assigns column names X1, X2, ... in the case
when `want.col.names` is `FALSE` (DS-1779)

Changes in Version 1.6.3 (2017-12-29)
--------------------------------------------------------

BUG FIXES

* Fix for `ParseUserEnteredTable` when the input data is a single
  column and data frame output is requested
* Fix for `ParseAsDataFrame` in the cases when there is a single row
  of data or two rows of data and column names are requested (this
  also corrects `ParseUserEnteredTable` in these situations when a
  data frame is requested).  
* `ParseUserEnteredTable` now warns correctly if for single row or
  column inputs when `warn` is `TRUE` and `want.data.frame` is `FALSE`
  

Changes in Version 1.6.0 (2017-12-13)
--------------------------------------------------------

NEW FEATURES

* `ParseUserEnteredTable` supports more ways to supply missing
  values, including "Missing", "-", ".", "N/A", "NA", "NaN", "invalid".

BUG FIXES

* `SplitVectorToList`will now always return an object of class list.
* When a `factor` is supplied for the `values` argument of
  `SplitVectorToList`, it will no longer be converted to `integer`in
  the outputted `list`.

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
