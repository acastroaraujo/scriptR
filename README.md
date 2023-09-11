
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scriptR

<!-- badges: start -->
<!-- badges: end -->

The goal of scriptR is to parse R scripts into a format that is suitable
for analysis.

## Installation

You can install the development version of `scriptR` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("acastroaraujo/scriptR")
```

## Example

The `scriptR` package currently contains three functions:

- `parse_script`: creates an R6 object with some datasets available via
  the `$` operator.

  - `$code` has a modified version of what you’d get with
    `getParseData()`

  - `$functions` has a modified version of `$code`, such that only
    function calls are retained.

  - `$pkg_search` contains the `search()` path for the libraries loaded
    in the scripts via `library()`.

- `get_unused_objects`: produces the unused objects contained in a
  script (`$code`).

- `get_conflicts`: produces the conflicts among packages.

This last function will work with *any* vector of installed package
names.

For example:

``` r
library(scriptR)

get_conflicts(c("network", "igraph", "dplyr"))
#> # A tibble: 19 × 4
#>    f                      network igraph dplyr
#>    <chr>                    <int>  <int> <int>
#>  1 delete.vertices              1      1     0
#>  2 set.edge.attribute           1      1     0
#>  3 %s%                          1      1     0
#>  4 is.bipartite                 1      1     0
#>  5 get.vertex.attribute         1      1     0
#>  6 delete.edges                 1      1     0
#>  7 set.vertex.attribute         1      1     0
#>  8 add.edges                    1      1     0
#>  9 is.directed                  1      1     0
#> 10 %c%                          1      1     0
#> 11 get.edges                    1      1     0
#> 12 get.edge.attribute           1      1     0
#> 13 list.edge.attributes         1      1     0
#> 14 add.vertices                 1      1     0
#> 15 list.vertex.attributes       1      1     0
#> 16 as_data_frame                0      1     1
#> 17 groups                       0      1     1
#> 18 %>%                          0      1     1
#> 19 union                        0      1     1
```

More examples:

``` r
url <- "https://raw.githubusercontent.com/acastroaraujo/ccc/master/data-raw/04-docterms.R"

out <- parse_script(url)
out
#> Scripts:
#> [1] "https://raw.githubusercontent.com/acastroaraujo/ccc/master/data-raw/04-docterms.R"
dplyr::glimpse(out$code)
#> Rows: 124
#> Columns: 11
#> $ script   <chr> "04-docterms.R", "04-docterms.R", "04-docterms.R", "04-docter…
#> $ text     <chr> "library", "(", "tidyverse", ")", "source", "(", "\"data-raw/…
#> $ token    <chr> "SYMBOL_FUNCTION_CALL", "'('", "SYMBOL", "')'", "SYMBOL_FUNCT…
#> $ line1    <int> 2, 2, 2, 2, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 7, 7, 7…
#> $ col1     <int> 1, 8, 9, 18, 1, 7, 8, 36, 1, 4, 7, 15, 16, 43, 1, 6, 9, 15, 1…
#> $ line2    <int> 2, 2, 2, 2, 3, 3, 3, 3, 5, 5, 5, 5, 5, 5, 7, 7, 7, 7, 7, 7, 7…
#> $ col2     <int> 7, 8, 17, 18, 6, 7, 35, 36, 2, 5, 14, 15, 42, 43, 4, 7, 14, 1…
#> $ id       <int> 3, 4, 6, 7, 16, 17, 19, 20, 31, 32, 34, 35, 37, 38, 50, 51, 5…
#> $ parent   <int> 5, 12, 8, 12, 18, 25, 21, 25, 33, 45, 36, 43, 39, 43, 52, 67,…
#> $ terminal <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
#> $ dir      <chr> "https://raw.githubusercontent.com/acastroaraujo/ccc/master/d…
dplyr::glimpse(out$functions)
#> Rows: 17
#> Columns: 11
#> $ f        <chr> "library", "source", "read_rds", "unique", "length", "count",…
#> $ pkg      <chr> "base", "base", "readr", "base", "base", "dplyr", "dplyr", "d…
#> $ script   <chr> "04-docterms.R", "04-docterms.R", "04-docterms.R", "04-docter…
#> $ line1    <int> 2, 3, 5, 7, 8, 11, 12, 15, 16, 16, 17, 20, 22, 27, 28, 29, 32
#> $ col1     <int> 1, 1, 7, 9, 11, 3, 3, 3, 3, 13, 3, 3, 12, 3, 14, 13, 10
#> $ line2    <int> 2, 3, 5, 7, 8, 11, 12, 15, 16, 16, 17, 20, 22, 27, 28, 29, 32
#> $ col2     <int> 7, 6, 14, 14, 16, 7, 8, 8, 11, 18, 6, 8, 17, 8, 19, 18, 17
#> $ id       <int> 3, 16, 34, 53, 73, 96, 109, 139, 156, 159, 179, 203, 227, 255…
#> $ parent   <int> 5, 18, 36, 55, 75, 98, 111, 141, 158, 161, 181, 205, 229, 257…
#> $ terminal <lgl> TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T…
#> $ dir      <chr> "https://raw.githubusercontent.com/acastroaraujo/ccc/master/d…
out$pkg_search
#>  [1] "lubridate" "forcats"   "stringr"   "dplyr"     "purrr"     "readr"    
#>  [7] "tidyr"     "tibble"    "ggplot2"   "tidyverse" "stats"     "graphics" 
#> [13] "grDevices" "utils"     "datasets"  "methods"   "base"
```

It’s then very easy to work with these datasets.

Last example:

``` r
library(dplyr)
pkg_df <- out$functions |>
  count(f, pkg, sort = TRUE)

pkg_df <- pkg_df |>
  group_by(pkg) |>
  summarize(
    n_functions = length(pkg),
    n = sum(n)
  ) |>
  arrange(n_functions)

## NA in this example means that the pkg was specified via the :: operator
## probably a good a idea to change this
pkg_df 
#> # A tibble: 4 × 3
#>   pkg   n_functions     n
#>   <chr>       <int> <int>
#> 1 readr           1     1
#> 2 <NA>            1     1
#> 3 base            5     7
#> 4 dplyr           6     8
```

This is also when we would use the other `scriptR` functions:

``` r
get_unused_objs(out$code)
#> # A tibble: 1 × 11
#>   script        text   token line1  col1 line2  col2    id parent terminal dir  
#>   <chr>         <chr>  <chr> <int> <int> <int> <int> <int>  <int> <lgl>    <chr>
#> 1 04-docterms.R n_wor… SYMB…    22     1    22     7   224    226 TRUE     http…

get_conflicts(out$pkg_search) |> 
  filter(f %in% unique(out$functions$f)) |> 
  select_if(\(x) any(x == 1 | class(x) == "character"))
#> # A tibble: 2 × 5
#>   f      dplyr tidyr tibble stats
#>   <chr>  <int> <int>  <int> <int>
#> 1 filter     1     0      0     1
#> 2 tibble     1     1      1     0
```
