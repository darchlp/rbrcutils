
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbrcutils

<!-- badges: start -->

<!-- badges: end -->

The goal of rbrcutils is to provide access to functions used commonly in
developing reports for the Rosemary Bryant AO Research Centre.

## Installation

You can install the development version of rbrcutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("darchlp/rbrcutils")
```

## Example

Currently, this package provides functionality for producing plots to
visualise categorical variables (in the form of column plots) and
numerical variables (in the form of box and whisker plots). These plots
can either be a series of variables over the whole dataset
`consecutive_**_plot` or a single variable by a grouping variable
`**_group_plot`.

### Categorical variables

Plot a series of categorical variables over the whole data.

``` r
library(rbrcutils)
df <- data.frame(
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

Or a categorical variable by a grouping variable.

``` r
library(rbrcutils)
df <- data.frame(
  "apples" = sample(
    forcats::as_factor(c("Good", "Neutral", "Bad")),
    size = 50,
    replace = TRUE
  ),
  "group" = sample(
    forcats::as_factor(c("Group 1", "Group 2", "Group 3")),
    size = 50,
    replace = TRUE
  )
)
cat_group_plot(
  df,
  xvar = "apples",
  yvar = "group",
  colors = c(
    "Bad" = "#b44218",
    "Neutral" = "#b4b4b4",
    "Good" = "#179dab"
  )
)
#> Plot not saved
```

<img src="man/figures/README-cat_group_plot-1.png" width="100%" />

### Numeric variable plot

Plot a series of numerical variables over the whole data.

``` r
library(rbrcutils)
df <- data.frame(
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

Or a numeric variable by a grouping variable.

``` r
library(rbrcutils)
df <- data.frame(
  "number" = sample(1:5, size = 50, replace = TRUE),
  "group" = sample(
    forcats::as_factor(c("Group 1", "Group 2", "Group 3")),
    size = 50,
    replace = TRUE
  )
)

box_group_plot(
  df,
  xvar = "number",
  xvar_name = "Numeric",
  yvar = "group",
  yvar_name = "Grouping",
  labels = c(
    "1" = "Strongly disagree",
    "2" = "Disagree",
    "3" = "Neutral",
    "4" = "Agree",
    "5" = "Strongly agree"
  )
)
#> Plot not saved
#> Warning: Use of `.df[[xvar]]` is discouraged.
#> ℹ Use `.data[[xvar]]` instead.
#> Use of `.df[[xvar]]` is discouraged.
#> ℹ Use `.data[[xvar]]` instead.
```

<img src="man/figures/README-box_group_plot-1.png" width="100%" />
