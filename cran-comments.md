## This is a resubmission

It is implementing feedback from Uwe Ligges. The rather
large tarball size was reduced by moving a figure heavy vignette to an article
now only displayed on the package website.

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
