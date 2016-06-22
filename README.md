# platetools
[![Build Status](https://travis-ci.org/Swarchal/platetools.svg?branch=master)](https://travis-ci.org/Swarchal/platetools)
[![cran-version](http://www.r-pkg.org/badges/version/platetools)](http://cran.rstudio.com/web/packages/platetools)
[![Codecov branch](https://img.shields.io/codecov/c/github/Swarchal/platetools/master.svg)](https://codecov.io/gh/Swarchal/platetools)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/platetools)](http://www.r-pkg.org/pkg/platetools)

#### An R package for working with multi-well plates

#### Installation

CRAN:
```r
install.packages("platetools")
```
GitHub (dev version):
```r
devtools::install_github("swarchal/platetools")
```

### Use

See the [vignette](vignette.ipynb) for examples.

###### Grammar

###### Prefix:
- `raw`: raw values
- `hit`: hit detection based on standard deviations
- `z`: z-scored values
- `b`: b-scored values
- `bhit`: b-score values before hit detection

###### Suffix:

- `map`: single plate map
- `grid`: multple plate maps

e.g:  
`z_map`: z-scored plate map  
`raw_grid:` raw values, multiple plate maps  
`bhit_grid:` b-scored values, coloured by hits, multiple plates


##### Maintainer
Scott Warchal - <s.warchal@sms.ed.ac.uk>
