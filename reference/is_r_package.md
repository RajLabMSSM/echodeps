# Is R package

Check whether a GitHub repository is for an R package or not.

## Usage

``` r
is_r_package(pkg, owner = NULL, verbose = TRUE)
```

## Arguments

- pkg:

  Package name.

- owner:

  Name of the owner of the R packages' GitHub repository (optional).

- verbose:

  Print messages.

## Value

boolean

## Examples

``` r
if (FALSE) { # \dontrun{
is_pkg <- is_r_package(pkg="stats")
} # }
```
