# Update episodes lookup table

update_episodes_lup() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update episodes lookup table. The function
returns Episodes lookup table (a tibble).

## Usage

``` r
update_episodes_lup(
  episodes_lup_tb,
  dates_chr,
  part_one_1L_chr,
  program_true_1L_chr
)
```

## Arguments

- episodes_lup_tb:

  Episodes lookup table (a tibble)

- dates_chr:

  Dates (a character vector)

- part_one_1L_chr:

  Part one (a character vector of length one)

- program_true_1L_chr:

  Program true (a character vector of length one)

## Value

Episodes lookup table (a tibble)
