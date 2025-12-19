# Write projectDS

write_project_RDS() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write projectds. The function is called for its side
effects and does not return a value.

## Usage

``` r
write_project_RDS(
  data_ls,
  path_to_private_1L_chr,
  processed_dir_1L_chr,
  divider_1L_chr = "\\",
  r_dir_1L_chr = "R"
)
```

## Arguments

- data_ls:

  Data (a list)

- path_to_private_1L_chr:

  Path to private (a character vector of length one)

- processed_dir_1L_chr:

  Processed directory (a character vector of length one)

- divider_1L_chr:

  Divider (a character vector of length one), Default: '\\

- r_dir_1L_chr:

  R directory (a character vector of length one), Default: 'R'

## Value

No return value, called for side effects.
