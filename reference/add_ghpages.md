# Add GitHub Pages metadata

For each package, list the links to the dedicated GitHub Pages
website(s). If there are vignettes, add those as well. If the GitHub
Pages website for a package does not exist, `NULL` will be returned for
that package.

## Usage

``` r
add_ghpages(meta = "echoverse", add_vignettes = TRUE, verbose = TRUE)
```

## Arguments

- meta:

  A metadata
  [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
  produced by
  [package_metadata](https://rajlabmssm.github.io/echodeps/reference/package_metadata.md).
  Alternatively, can be a character vector of packages to gather
  metadata for, passed to
  [package_metadata](https://rajlabmssm.github.io/echodeps/reference/package_metadata.md).

- add_vignettes:

  Search for any vignettes on the GitHub Pages websites, and add them to
  a new column named "ghpages_vignettes".

- verbose:

  Print messages.

## Value

[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) of
metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- add_ghpages()
} # }
```
