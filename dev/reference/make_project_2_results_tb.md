# Make project 2 results tibble

make_project_2_results_tb() is a Make function that creates a new R
object. Specifically, this function implements an algorithm to make
project 2 results tibble. The function returns Results (a tibble).

## Usage

``` r
make_project_2_results_tb(
  sim_results_ls,
  comparator_1L_chr,
  intervention_1L_chr,
  params_tb,
  platform_1L_chr,
  digits_1L_int = 2,
  disciplines_chr = make_disciplines(),
  drop_chr = character(0),
  filter_1L_lgl = TRUE,
  format_1L_lgl = TRUE,
  type_1L_chr = c("main", "cost", "outcomes", "use")
)
```

## Arguments

- sim_results_ls:

  Sim results (a list)

- comparator_1L_chr:

  Comparator (a character vector of length one)

- intervention_1L_chr:

  Intervention (a character vector of length one)

- params_tb:

  Parameters (a tibble)

- platform_1L_chr:

  Platform (a character vector of length one)

- digits_1L_int:

  Digits (an integer vector of length one), Default: 2

- disciplines_chr:

  Disciplines (a character vector), Default: make_disciplines()

- drop_chr:

  Drop (a character vector), Default: character(0)

- filter_1L_lgl:

  Filter (a logical vector of length one), Default: TRUE

- format_1L_lgl:

  Format (a logical vector of length one), Default: TRUE

- type_1L_chr:

  Type (a character vector of length one), Default: c("main", "cost",
  "outcomes", "use")

## Value

Results (a tibble)
