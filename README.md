# README

<!-- badges: start -->
<!-- badges: end -->

This repository contains modeling scripts for the 
'Long-term implicit memory for auditory patterns'
project by Roberta Bianco, Peter Harrison, Marcus Pearce, and Maria Chait.

## Usage

The code runs in the programming language R, which you must 
install if you don't have already.

Next, install the devtools package:

``` r
install.packages("devtools")
```

Then install the `ppm` package from GitHub: 

``` r
devtools::install_github("pmcharrison/ppm@v0.2.0")
```

Then install some other packages from CRAN:

```{r}
install.packages(c(
  "tidyverse", "ggsci", "testthat", "zeallot",
  "cowplot", "grid", "plyr", "nloptr", "egg", 
  "checkmate", "R.utils", "memoise", "cpm"
))
```

Now you should be able to run the analysis scripts.
They run in this order, saving their results to `output/`:

- `0-compile-data/0-compile-data.R`
- `1-model/1-model.R`
- `2-plot/2-plot.R`

A directory called `cache` will also be created, 
which stores cached versions of analysis outputs.
Delete this if you want the computations to start from scratch.

If you receive any error messages about missing R packages,
try to (re)install the offending packages using `install.packages`.
