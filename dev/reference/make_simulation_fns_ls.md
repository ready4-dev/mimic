# Make simulation functions list

make_simulation_fns_ls() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make simulation
functions list. The function returns Simulation functions (a list).

## Usage

``` r
make_simulation_fns_ls(
  type_1L_chr = c("all", "main", "processing", "sensitivity", "transformation"),
  comparator_fn = identity,
  extra_draws_fn = NULL,
  intervention_fn = identity,
  sensitivities_ls = make_sensitivities_ls(),
  synthesis_fn = make_project_results_synthesis,
  transformation_ls = make_class_tfmns(),
  ...
)
```

## Arguments

- type_1L_chr:

  Type (a character vector of length one), Default: c("all", "main",
  "processing", "sensitivity", "transformation")

- comparator_fn:

  Comparator (a function), Default: identity

- extra_draws_fn:

  Extra draws (a function), Default: NULL

- intervention_fn:

  Intervention (a function), Default: identity

- sensitivities_ls:

  Sensitivities (a list), Default: make_sensitivities_ls()

- synthesis_fn:

  Synthesis (a function), Default: make_project_results_synthesis

- transformation_ls:

  Transformation (a list), Default: make_class_tfmns()

- ...:

  Additional arguments

## Value

Simulation functions (a list)
