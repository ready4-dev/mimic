# Add episode wait time

add_episode_wait_time() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add episode wait time. The function is called
for its side effects and does not return a value.

## Usage

``` r
add_episode_wait_time(
  X_Ready4useDyad,
  episode_start_mdl = NULL,
  iterations_int = 1:100L,
  type_1L_chr = c("first", "repeat"),
  treatment_1L_chr = character(0)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- episode_start_mdl:

  Episode start (a model), Default: NULL

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- type_1L_chr:

  Type (a character vector of length one), Default: c("first", "repeat")

- treatment_1L_chr:

  Treatment (a character vector of length one), Default: character(0)

## Value

X (A dataset and data dictionary pair.)
