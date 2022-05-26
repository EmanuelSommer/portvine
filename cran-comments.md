## This is a resubmission

It is implementing feedback from Uwe Ligges (Thanks for that!).

* The rather large tarball size was reduced by moving a figure heavy vignette
  to an article now only displayed on the package website.

* The overall check time was reduced as suggested mainly by making the examples
  in the vignette faster and by skipping a test of an internal function on CRAN,
  which are aimed for the local testing when implementing new features.

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.

* checking installed package size ... NOTE
  installed size is 5.7Mb
  sub-directories of 1Mb or more:
    libs   4.9Mb
    
  This is I guess due to usage of the rather large C++ library vinecopulib
  provided by the CRAN package rvinecopulib in order to facilitate the novel 
  conditional sampling algorithms.
