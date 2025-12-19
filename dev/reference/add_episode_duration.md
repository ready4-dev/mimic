# Add episode duration

add_episode_duration() is an Add function that updates an object by
adding new values to new or empty fields. Specifically, this function
implements an algorithm to add episode duration. The function is called
for its side effects and does not return a value.

## Usage

``` r
add_episode_duration(
  X_Ready4useDyad,
  episode_end_mdl = NULL,
  iterations_int = 1:100L,
  treatment_1L_chr = character(0)
)
```

## Arguments

- X_Ready4useDyad:

  PARAM_DESCRIPTION

- episode_end_mdl:

  Episode end (a model), Default: NULL

- iterations_int:

  Iterations (an integer vector), Default: 1:100L

- treatment_1L_chr:

  Treatment (a character vector of length one), Default: character(0)

## Value

X (A dataset and data dictionary pair.)
