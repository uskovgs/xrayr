<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

```R
devtools::install_github("uskovgs/xrayr")
```


```R
library(xrayr)
nh(213.1612728, -65.38967664, r_arcmin=5, showInfo = T)
#>       _r    recno      HPX  RAJ2000  DEJ2000     GLON     GLAT      NHI 
#> "arcmin"      " "      " "    "deg"    "deg"    "deg"    "deg"   "cm-2" 
#> # A tibble: 5 × 8
#>    `_r`   recno     HPX RAJ2000 DEJ2000  GLON  GLAT     NHI
#>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl> <dbl>   <dbl>
#> 1  1.93 6710742 6710741    213.   -65.4  311. -3.81 5.14e21
#> 2  2.30 6714838 6714837    213.   -65.4  311. -3.84 5.02e21
#> 3  2.61 6718934 6718933    213.   -65.4  311. -3.88 5.03e21
#> 4  3.01 6714839 6714838    213.   -65.4  311. -3.84 5.12e21
#> 5  4.72 6706646 6706645    213.   -65.3  311. -3.77 5.13e21
#> ----
#> This value is derived from the 2D HI4PI map, 
#>  a full-sky HI survey by the HI4PI collaboration 2016,
#>  Astronomy & Astrophysics, 594, A116.
#> ----
#> *RA target: 213.1612728
#> *DEC target: -65.38967664
#> RA range [213.0238, 213.2793]
#> DEC range [-65.4288, -65.3357]
#> Average mean nH (cm**-2): 5.09e+21 
#> Weighted average nH (cm**-2): 5.109e+21
#> [1] 5.108978e+21
```

``` r
library(xrayr)

# in degreed
x1 <- ra_dec(ra = 213.1, dec = -75.1)
# in hms dms
x2 <- radec('14h1m1s -75d15m23.68s')

# separation in arcsec
separation(x1, x2)
#> [1] 2680.447


# get RA value
ra(x2)
#> [1] 210.2542
# get DEC value
dec(x2)
#> [1] -75.25658
```




## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
  By participating in this project you agree to abide by its terms.
