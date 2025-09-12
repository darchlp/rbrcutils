
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbrcutils

<!-- badges: start -->

<!-- badges: end -->

The goal of rbrcutils is to provide access to functions to produce
tables and figures used commonly in developing reports for the Rosemary
Bryant AO Research Centre.

## Installation

You can install the development version of rbrcutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("darchlp/rbrcutils")
```

## Example

Here are some examples of the plots which can be produced:

### Consecutive categorical plot

Plot a series of categorical variables.

``` r
library(rbrcutils)
df <- data.frame(
  "record_id" = 1:50,
  "apples" = sample(
    forcats::as_factor(c("Good", "Neutral", "Bad")),
    size = 50,
    replace = T
  ),
  "bananas" = sample(
    forcats::as_factor(c("Good", "Neutral", "Bad")),
    size = 50,
    replace = T
  ),
  "pears" = sample(
    forcats::as_factor(c("Good", "Neutral", "Bad")),
    size = 50,
    replace = T
  )
)
consecutive_cat_plot(
  df,
  c("Apples" = "apples", "Bananas" = "bananas", "Pears" = "pears"),
  colors = c(
    "Bad" = "#b44218",
    "Neutral" = "#b4b4b4",
    "Good" = "#179dab"
  )
)
#> Plot not saved
```

<img src="man/figures/README-consecutive_cat_plot-1.png" width="100%" />

### Consecutive box plot

Plot a series of numerical variables.

``` r
library(rbrcutils)
df <- data.frame(
  "record_id" = 1:50,
  "number1" = sample(1:5, size = 50, replace = T),
  "number2" = sample(1:5, size = 50, replace = T),
  "number3" = sample(1:5, size = 50, replace = T)
)
consecutive_box_plot(
  df,
  var = c(
    "Number 1" = "number1",
    "Number 2" = "number2",
    "Number 3" = "number3"
  ),
  labels = c(
    "1" = "Strongly diagree",
    "2" = "Disagree",
    "3" = "Neutral",
    "4" = "Agree",
    "5" = "Strongly agree"
  ),
  ordered_mean = T
)
#> Plot not saved
```

<img src="man/figures/README-consecutive_box_plot-1.png" width="100%" />
