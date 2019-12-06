
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jvmrr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/igjit/jvmrr.svg?branch=master)](https://travis-ci.org/igjit/jvmrr)
[![Codecov test
coverage](https://codecov.io/gh/igjit/jvmrr/branch/master/graph/badge.svg)](https://codecov.io/gh/igjit/jvmrr?branch=master)
<!-- badges: end -->

jvmrr is a toy Java VM written in R.

## Installation

You can install the development version of jvmrr from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("igjit/jvmrr")
```

## How to play

``` r
library(jvmrr)

jvmrr_example()
#> [1] "Arith.class"    "FizzBuzz.class" "Hello.class"
class_file <- jvmrr_example("FizzBuzz.class")
java_class <- read_class(class_file)
java_class %>% head(3)
#> $magic
#> [1] ca fe ba be
#> 
#> $minor_version
#> [1] 0
#> 
#> $major_version
#> [1] 55
java_class %>% execute()
#> 1
#> 2
#> Fizz
#> 4
#> Buzz
#> Fizz
#> 7
#> 8
#> Fizz
#> Buzz
#> 11
#> Fizz
#> 13
#> 14
#> FizzBuzz
#> 16
#> 17
#> Fizz
#> 19
#> Buzz
```
