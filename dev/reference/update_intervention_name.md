# Update intervention name

update_intervention_name() is an Update function that edits an object,
while preserving core object attributes. Specifically, this function
implements an algorithm to update intervention name. The function
returns Data (a tibble).

## Usage

``` r
update_intervention_name(
  data_tb,
  new_1L_chr = "Comparator",
  old_1L_chr = "FlexPsych",
  var_nm_1L_chr = "Intervention"
)
```

## Arguments

- data_tb:

  Data (a tibble)

- new_1L_chr:

  New (a character vector of length one), Default: 'Comparator'

- old_1L_chr:

  Old (a character vector of length one), Default: 'FlexPsych'

- var_nm_1L_chr:

  Variable name (a character vector of length one), Default:
  'Intervention'

## Value

Data (a tibble)
