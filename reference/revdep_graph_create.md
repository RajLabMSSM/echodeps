# Create a reverse dependency graph

Create a reverse dependency graph.

## Usage

``` r
revdep_graph_create(
  pkg,
  exclude = NULL,
  method_seed = c("github", "devtools"),
  method = c("devtools", "github"),
  recursive = FALSE,
  node_size = NULL,
  add_metadata = TRUE,
  degrees = 2,
  verbose = TRUE
)
```

## Source

[igraph::union issue](https://stackoverflow.com/q/38428945)

## Arguments

- pkg:

  Package to search dependencies for.

- exclude:

  A subset of of the main package's (`pkg` ) dependencies to exclude in
  the graph.

- method_seed:

  Method to create the initial dependency graph with:

  "pkgnet" (`reverse=FALSE` only)

  :   Extracts all the R packages that the target R package depends on
      using
      [CreatePackageReport](https://uptake.github.io/pkgnet/reference/CreatePackageReport.html)

  "github" (`reverse=FALSE or TRUE`)

  :   Extracts all the GitHub repositories that depend on the respective
      GitHub repository of the target R package using
      [github_dependents](https://rdrr.io/pkg/echogithub/man/github_dependents.html)

  "devtools" (`reverse=TRUE`) only

  :   Extracts reverse dependencies using
      [revdep](https://devtools.r-lib.org/reference/revdep.html).

  If more than one option is provided, only the first is used.

- method:

  Seed method for extracting first-order reverse dependencies of `pkg`.

- recursive:

  If `TRUE` look for full set of recursive dependencies.

- node_size:

  Strategy for setting node sizes. Can be one of:

  NULL :

  :   Sets the target package node's size to 40 and the rest to 30

  \<numeric\> :

  :   User-provided node size. Sets all nodes to the same size.

  \<character\> :

  :   User-provided metadata column name. Scales node size to a vector
      of numeric values in the node metadata.

- add_metadata:

  Add metadata to the graph using
  [github_metadata](https://rdrr.io/pkg/echogithub/man/github_metadata.html).

- degrees:

  The number of degrees out from the main `pkg` node to extend the
  dependency graph.

- verbose:

  Print messages.

## Value

Named list.

## Examples

``` r
if (FALSE) { # \dontrun{
dgc_out <- revdep_graph_create(pkg = "rworkflows")
} # }
```
