## This is a resubmission

It is implementing feedback from Victoria Wimmer (Thanks for that!).

* Title was reduced to less than 65 characters

* The example which would run over 5 sec was wrapped in \donttest{} instead of 
  \dontrun{}
  
* I had a look at all my functions and as far as I am concerned I only found 
  calls of cat() in non print or summary functions with the following scheme:
  if(trace) cat(). So the user can suppress the information messages by setting
  the argument 'trace' to FALSE. Otherwise I only
  once print to the console with message() to inform the user of potentially
  unintended behavior.

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
