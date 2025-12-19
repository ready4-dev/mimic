# Write project workspace

write_project_ws() is a Write function that writes a file to a specified
local directory. Specifically, this function implements an algorithm to
write project workspace. The function is called for its side effects and
does not return a value.

## Usage

``` r
write_project_ws(
  path_to_private_1L_chr,
  processed_dir_1L_chr,
  divider_1L_chr = "\\"
)
```

## Arguments

- path_to_private_1L_chr:

  Path to private (a character vector of length one)

- processed_dir_1L_chr:

  Processed directory (a character vector of length one)

- divider_1L_chr:

  Divider (a character vector of length one), Default: '\\

## Value

No return value, called for side effects.
