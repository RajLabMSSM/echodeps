# Example graphs

Generate a random list of
[tbl_graph](https://tidygraph.data-imaginist.com/reference/tbl_graph.html)
objects.

## Usage

``` r
example_graphs(
  n = c(4, 5, 10),
  p = 0.5,
  colors = pals::viridis(length(n)),
  seed = 2023
)
```

## Arguments

- n:

  Number of nodes for each graph.

- p:

  The probability of nodes connecting between graphs (on a 0-1 scale).

- colors:

  Color for each graph.

- seed:

  Random seed.

## Value

A list of
[tbl_graph](https://tidygraph.data-imaginist.com/reference/tbl_graph.html)
objects.

## Examples

``` r
if (FALSE) { # \dontrun{
grs <- example_graphs()
} # }
```
