# Merge graphs

Merge a list of graphs into a single graph. Can take in a list of
[igraph](https://r.igraph.org/reference/aaa-igraph-package.html)s or a
list of
[igraph](https://r.igraph.org/reference/aaa-igraph-package.html)s, and
can return a merged
[tbl_graph](https://tidygraph.data-imaginist.com/reference/tbl_graph.html)
or a merged
[igraph](https://r.igraph.org/reference/aaa-igraph-package.html).

## Usage

``` r
merge_graphs(
  graph_list,
  node_size,
  output = c("tidygraph", "igraph"),
  by = "ref",
  verbose = TRUE
)
```

## Arguments

- graph_list:

  A list of graphs.

- node_size:

  Strategy for setting node sizes. Can be one of:

  NULL :

  :   Sets the target package node's size to 40 and the rest to 30

  \<numeric\> :

  :   User-provided node size. Sets all nodes to the same size.

  \<character\> :

  :   User-provided metadata column name. Scales node size to a vector
      of numeric values in the node metadata.

- output:

  Output format to return the graph in.

- by:

  A join specification created with
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html), or
  a character vector of variables to join by.

  If `NULL`, the default, `*_join()` will perform a natural join, using
  all variables in common across `x` and `y`. A message lists the
  variables so that you can check they're correct; suppress the message
  by supplying `by` explicitly.

  To join on different variables between `x` and `y`, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification. For example, `join_by(a == b)` will match `x$a` to
  `y$b`.

  To join by multiple variables, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification with multiple expressions. For example,
  `join_by(a == b, c == d)` will match `x$a` to `y$b` and `x$c` to
  `y$d`. If the column names are the same between `x` and `y`, you can
  shorten this by listing only the variable names, like `join_by(a, c)`.

  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html) can
  also be used to perform inequality, rolling, and overlap joins. See
  the documentation at
  [?join_by](https://dplyr.tidyverse.org/reference/join_by.html) for
  details on these types of joins.

  For simple equality joins, you can alternatively specify a character
  vector of variable names to join by. For example, `by = c("a", "b")`
  joins `x$a` to `y$a` and `x$b` to `y$b`. If variable names differ
  between `x` and `y`, use a named character vector like
  `by = c("x_a" = "y_a", "x_b" = "y_b")`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  see
  [`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html).

- verbose:

  Print messages.

## Value

Merge
[tbl_graph](https://tidygraph.data-imaginist.com/reference/tbl_graph.html)
or [igraph](https://r.igraph.org/reference/aaa-igraph-package.html).

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(2023)
graph_list <- example_graphs()
g <- merge_graphs(graph_list)
plot(g, edge.label=igraph::E(g)$color)
} # }
```
