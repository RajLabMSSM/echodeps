# Create a dependency graph: GitHub

Create a dependency graph: GitHub.

## Usage

``` r
dep_graph_create_github(
  refs,
  exclude = NULL,
  add_traffic = TRUE,
  node_size = NULL,
  reverse = FALSE,
  add_metadata = TRUE,
  verbose = TRUE
)
```

## Arguments

- refs:

  Reference for one or more GitHub repository in owner/repo format
  (e.g.`"neurogenomics/rworkflows"`), or an R package name (e.g.
  `"rworkflows"`).

- exclude:

  A subset of of the main package's (`pkg` ) dependencies to exclude in
  the graph.

- add_traffic:

  Add traffic metadata with
  [github_traffic](https://rdrr.io/pkg/echogithub/man/github_traffic.html).

- node_size:

  Strategy for setting node sizes. Can be one of:

  NULL :

  :   Sets the target package node's size to 40 and the rest to 30

  \<numeric\> :

  :   User-provided node size. Sets all nodes to the same size.

  \<character\> :

  :   User-provided metadata column name. Scales node size to a vector
      of numeric values in the node metadata.

- reverse:

  `FALSE` (default)

  :   Create a forward dependency graph (packages that `pkg` depends on,
      and the packages that they depend on).

  `TRUE`

  :   Create a reverse dependency graph (packages that depend on `pkg`,
      and the packages that depend on them).

- add_metadata:

  Add metadata to the graph using
  [github_metadata](https://rdrr.io/pkg/echogithub/man/github_metadata.html).

- verbose:

  Print messages.
