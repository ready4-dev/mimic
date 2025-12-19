# Add simulated data

add_simulated_data() is an Add function that updates an object by adding
new values to new or empty fields. Specifically, this function
implements an algorithm to add simulated data. The function is called
for its side effects and does not return a value.

## Usage

``` r
add_simulated_data(
  model_mdl,
  var_1L_chr,
  Y_Ready4useDyad,
  iterations_int = 1:100L,
  join_with_chr = character(0),
  rewind_chr = character(0),
  tfmn_1L_chr = "NTF",
  type_1L_chr = c("first", "second", "third", "fourth"),
  what_1L_chr = c("old", "new")
)
```

## Arguments

- model_mdl:

  Model (a model)

- var_1L_chr:

  Variable (a character vector of length one)

- Y_Ready4useDyad:

  PARAM_DESCRIPTION

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- join_with_chr:

  Join with (a character vector), Default: character(0)

- rewind_chr:

  Rewind (a character vector), Default: character(0)

- tfmn_1L_chr:

  Transformation (a character vector of length one), Default: 'NTF'

- type_1L_chr:

  Type (a character vector of length one), Default: c("first", "second",
  "third", "fourth")

- what_1L_chr:

  What (a character vector of length one), Default: c("old", "new")

## Value

Y (A dataset and data dictionary pair.)
