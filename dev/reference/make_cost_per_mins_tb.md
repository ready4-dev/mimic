# Make cost per minimums tibble

make_cost_per_mins_tb() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make cost per
minimums tibble. The function returns Data (a tibble).

## Usage

``` r
make_cost_per_mins_tb(
  mbs_tb = NULL,
  cost_types_chr = character(0),
  path_1L_chr = character(0),
  mbs_fl_nm_1L_chr = character(0),
  mbs_sheet_1L_chr = "Table EXP.14",
  mbs_skip_1L_int = 4,
  unit_cost_fl_nm_1L_chr = character(0),
  unit_cost_tb_ls = NULL
)
```

## Arguments

- mbs_tb:

  Medicare Benefits Schedule (a tibble), Default: NULL

- cost_types_chr:

  Cost types (a character vector), Default: character(0)

- path_1L_chr:

  Path (a character vector of length one), Default: character(0)

- mbs_fl_nm_1L_chr:

  Medicare Benefits Schedule file name (a character vector of length
  one), Default: character(0)

- mbs_sheet_1L_chr:

  Medicare Benefits Schedule sheet (a character vector of length one),
  Default: 'Table EXP.14'

- mbs_skip_1L_int:

  Medicare Benefits Schedule skip (an integer vector of length one),
  Default: 4

- unit_cost_fl_nm_1L_chr:

  Unit cost file name (a character vector of length one), Default:
  character(0)

- unit_cost_tb_ls:

  Unit cost (a list of tibbles), Default: NULL

## Value

Data (a tibble)
