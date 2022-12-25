## Version 1.0.2 of the portvine package (just Bugfixes)

Besides the bugfixes mentioned below no changes to the package were made.

* Bugfix: Set the `shape` parameter for the inverse PIT transformation of the copula scale residuals and do not use the default.

* Bugfix: Allow for 0 orders in the utility function `default_garch_spec()`

* Bugfix: The residual calculation should allow for orders for the mean and variance models that are different than the standard (1,1) order

## R CMD check results

The package was checked on all major operation systems.

0 errors | 0 warnings | 1 note

* checking installed package size ... NOTE
  installed size is 5.8Mb
  sub-directories of 1Mb or more:
    libs   5.0Mb
    
  This note is the same as for the current CRAN version 1.0.1 and
  is due to usage of the rather large C++ library vinecopulib
  provided by the CRAN package rvinecopulib in order to facilitate the 
  conditional sampling algorithms.
