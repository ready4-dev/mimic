# Update minute variable names

update_minute_var_nms() is an Update function that edits an object,
while preserving core object attributes. Specifically, this function
implements an algorithm to update minute variable names. The function
returns Data (a tibble).

## Usage

``` r
update_minute_var_nms(data_tb, type_1L_chr = c("undo", "do"))
```

## Arguments

- data_tb:

  Data (a tibble)

- type_1L_chr:

  Type (a character vector of length one), Default: c("undo", "do")

## Value

Data (a tibble)
