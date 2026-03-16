# Create a dependency graph

Create a dependency graph.

## Usage

``` r
dep_graph_create(
  pkg,
  exclude = NULL,
  method = c("pkgnet", "github"),
  node_size = NULL,
  add_metadata = TRUE,
  verbose = TRUE
)
```

## Arguments

- pkg:

  Package to search dependencies for.

- exclude:

  A subset of of the main package's (`pkg` ) dependencies to exclude in
  the graph.

- method:

  Seed method for extracting first-order dependencies of `pkg`.

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

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
dgc_out <- dep_graph_create(pkg = "rworkflows",
                            method = "github")
} # }
```
