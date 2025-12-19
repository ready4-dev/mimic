# Make regressions list

make_regressions_ls() is a Make function that creates a new R object.
Specifically, this function implements an algorithm to make regressions
list. The function returns Regressions (a list).

## Usage

``` r
make_regressions_ls(
  prototype_ls_ls = list(AQoL6D_ls = list(), CHU9D_ls = list(), k10_ls = list(),
    Minutes_ls = list(), Treatments_ls = list(Waitlist_ls = list(), Treatment_ls =
    list(), Discharged_ls = list())),
  prototype_mdls_ls = list(AQoL6D_mdl = NULL, CHU9D_mdl = NULL, k10_mdl = NULL,
    Minutes_mdl = NULL, Treatments_ls = list(Waitlist_mdl = NULL, Treatment_mdl = NULL,
    Discharged_mdl = NULL))
)
```

## Arguments

- prototype_ls_ls:

  Prototype (a list of lists), Default: list(AQoL6D_ls = list(),
  CHU9D_ls = list(), k10_ls = list(), Minutes_ls = list(), Treatments_ls
  = list(Waitlist_ls = list(), Treatment_ls = list(), Discharged_ls =
  list()))

- prototype_mdls_ls:

  Prototype models (a list), Default: list(AQoL6D_mdl = NULL, CHU9D_mdl
  = NULL, k10_mdl = NULL, Minutes_mdl = NULL, Treatments_ls =
  list(Waitlist_mdl = NULL, Treatment_mdl = NULL, Discharged_mdl =
  NULL))

## Value

Regressions (a list)
