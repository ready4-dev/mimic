# Write raw Minimum Dataset data

write_raw_mds_data() is a Write function that writes a file to a
specified local directory. Specifically, this function implements an
algorithm to write raw minimum dataset data. The function is called for
its side effects and does not return a value.

## Usage

``` r
write_raw_mds_data(raw_mds_data_ls, path_to_raw_dir_1L_chr, r_dir_1L_chr = "R")
```

## Arguments

- raw_mds_data_ls:

  Raw Minimum Dataset data (a list)

- path_to_raw_dir_1L_chr:

  Path to raw directory (a character vector of length one)

- r_dir_1L_chr:

  R directory (a character vector of length one), Default: 'R'

## Value

No return value, called for side effects.
