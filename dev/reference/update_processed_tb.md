# Update processed tibble

update_processed_tb() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update processed tibble. The function returns
Data (a tibble).

## Usage

``` r
update_processed_tb(
  data_tb,
  first_eight_1L_lgl = NA,
  program_1L_chr = NA_character_
)
```

## Arguments

- data_tb:

  Data (a tibble)

- first_eight_1L_lgl:

  First eight (a logical vector of length one), Default: NA

- program_1L_chr:

  Program (a character vector of length one), Default: 'NA'

## Value

Data (a tibble)
