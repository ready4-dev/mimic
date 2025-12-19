# Update population list

update_population_ls() is an Update function that edits an object, while
preserving core object attributes. Specifically, this function
implements an algorithm to update population list. The function returns
Population (a list).

## Usage

``` r
update_population_ls(
  population_ls = NULL,
  X_Ready4useDyad = ready4use::Ready4useDyad(),
  type_1L_chr = c("split", "join", "form"),
  use_1L_chr = c("Y", "Z")
)
```

## Arguments

- population_ls:

  Population (a list), Default: NULL

- X_Ready4useDyad:

  PARAM_DESCRIPTION, Default: ready4use::Ready4useDyad()

- type_1L_chr:

  Type (a character vector of length one), Default: c("split", "join",
  "form")

- use_1L_chr:

  Use (a character vector of length one), Default: c("Y", "Z")

## Value

Population (a list)
