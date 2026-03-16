# Subset graph

Subset a graph by nodes to include or exclude. Automatically omits
`include`/`exclude` nodes that don't exist in the graph to avoid errors.

## Usage

``` r
subset_graph(g, include = NULL, exclude = NULL, verbose = TRUE)
```

## Arguments

- g:

  A graph of class
  [igraph](https://r.igraph.org/reference/aaa-igraph-package.html).
  *Warning:* Large graphs may be slow to interact with.

- include:

  Names of nodes to include.

- exclude:

  Names of nodes to exclude.

- verbose:

  Print messages.

## Value

Subgraph.

## Examples

``` r
if (FALSE) { # \dontrun{
dgc_out <- dep_graph_create(pkg = "rworkflows",
                            method = "github",
                            node_size = "clones_uniques")
g <- dgc_out$graph
exclude <- grep("actions-marketplace-validations",
                names(igraph::V(g)),value = TRUE)
g2 <- subset_graph(g = g, exclude = exclude)
} # }
```
