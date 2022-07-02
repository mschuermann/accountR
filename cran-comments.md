## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

There was 1 NOTE:
* checking R code for possible problems ... NOTE
  aged_analysis: no visible binding for global variable ‘days_overdue’
  aging_report: no visible binding for global variable ‘Customer’
  aging_report: no visible binding for global variable ‘.’
  Undefined global functions or variables:
    . Customer days_overdue
    
This results from data masking of the tidyverse package's functions. It does not affect the code because they are not global variables.
