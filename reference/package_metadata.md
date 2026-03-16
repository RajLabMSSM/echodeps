# Gather package metadata

Gather metadata from the DESCRIPTION file of each R package and
construct a
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html). By
default, returns metadata for all
[`echoverse`](https://github.com/topics/echoverse) packages.

## Usage

``` r
package_metadata(
  pkgs = "echoverse",
  fields = c("Package", "Title", "Description", "URL", "Version", "Depends", "Imports",
    "Suggests", "Remotes", "SystemRequirements"),
  verbose = TRUE
)
```

## Arguments

- pkgs:

  Packages to gather metadata for.

- fields:

  DESCRIPTION file fields. If a field is missing for a given package,
  will fill with `NULL`.

- verbose:

  Print messages.

## Value

[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) of
metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- echodeps::package_metadata()
} # }
```
