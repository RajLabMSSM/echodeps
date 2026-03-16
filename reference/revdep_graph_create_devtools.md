# Create a reverse dependency graph: devtools

Create a reverse dependency graph.

## Usage

``` r
revdep_graph_create_devtools(
  refs,
  pkg_target = NULL,
  exclude = NULL,
  recursive = FALSE,
  add_metadata = TRUE,
  node_size = NULL,
  verbose = TRUE
)
```

## Arguments

- refs:

  Package names.

- exclude:

  A subset of of the main package's (`pkg` ) dependencies to exclude in
  the graph.

- recursive:

  If `TRUE` look for full set of recursive dependencies.

- add_metadata:

  Add metadata to the graph using
  [github_metadata](https://rdrr.io/pkg/echogithub/man/github_metadata.html).

- node_size:

  Strategy for setting node sizes. Can be one of:

  NULL :

  :   Sets the target package node's size to 40 and the rest to 30

  \<numeric\> :

  :   User-provided node size. Sets all nodes to the same size.

  \<character\> :

  :   User-provided metadata column name. Scales node size to a vector
      of numeric values in the node metadata.

- verbose:

  Print messages.

## Value

igraph
