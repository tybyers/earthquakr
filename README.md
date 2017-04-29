[![Travis-CI Build Status](https://travis-ci.org/tybyers/earthquakr.svg?branch=master)](https://travis-ci.org/tybyers/earthquakr)

## earthquakr R package

This is a small R package for cleaning, timelining, and mapping [NOAA Significant Earthquake data](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1).

### About

This R package was originally built to satisfy the requirements of the Capstone project for the [Coursera](http://www.coursera.org) [Mastering Software Development in R](https://www.coursera.org/specializations/r) 5-course specialization.

### Installation

To install this package to run on your system, please first install and load the `devtools` package. Then you may install and load this package thus:

```r
devtools::install_github('tybyers/earthquakr')
library(earthquakr)
```

### Vignette

You may read the interactive vignette at my personal blog site, at: http://datawrangl.com/assets/earthquakr-introduction.html.

Alternatively, read the introduction vignette using the command `vignette('introduction', package = 'earthquakr')` after installation.  However, in order to do this, you must build the vignettes when installing, using the command `devtools::install_github('tybyers/earthquakr', build_vignettes = TRUE)`


### A Note on Plagiarism

For my fellow Coursera students, please remember that using or "borrowing" this code in order to complete your own project is a violation of the Coursera Honor Code. In past courses in this specialization, I reported several individuals who clearly copied my work, which I noticed when I was grading their assignments.  I will not hesitate to do the same for this Capstone project.  
