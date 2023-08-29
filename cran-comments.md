## Resubmission
This is a resubmission. In this version I have:

* Edited the DESCRIPTION to use single quotes when referring to software and API names.

* Edited the DESCRIPTION to use 'shiny' instead of `Shiny`.

* Edited the DESCRIPTION so that the first sentence of the Description explains a major situation in which a researcher might use bootstrap methods.

* Removed examples from the documentation of unexported functions.

* Wrapped the example for exactamente_app() in if(interactive()) instead of commenting the example out.

In addition to the changes above which were motivated by the critique of the original submission, I also made two other changes for clarity and alignment with the literature.

* Simplified the 'exactamente' package by removing a bootstrap method which performs poorly (the unweighted exact case bootstrap).

* Specified Kisielinska (2013) <doi:10.1007/s00180-012-0350-0> as the main reference.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... [12s] NOTE
  Maintainer: 'Mackson Ncube <macksonncube.stats@gmail.com>'
  
  New submission
