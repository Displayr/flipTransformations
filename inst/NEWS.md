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
