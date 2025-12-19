# Write project comma separated variables files

write_project_csvs() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write project comma separated variables files. The function
is called for its side effects and does not return a value.

## Usage

``` r
write_project_csvs(
  model_data_ls,
  path_to_private_1L_chr,
  processed_dir_1L_chr,
  divider_1L_chr = "\\"
)
```

## Arguments

- model_data_ls:

  Model data (a list)

- path_to_private_1L_chr:

  Path to private (a character vector of length one)

- processed_dir_1L_chr:

  Processed directory (a character vector of length one)

- divider_1L_chr:

  Divider (a character vector of length one), Default: '\\

## Value

No return value, called for side effects.
