# Star layout

Default layout function that arranges nodes in a star pattern with one
node (`pkg`) at its center.

## Usage

``` r
layout_star(graph, pkg, layout = "layout_as_star", ...)
```

## Arguments

- graph:

  : a visNetwork object

- pkg:

  Name of the node to place at the center of the plot (e.g.
  "echolocatoR").

- layout:

  : Character Name of igraph layout function to use. Default to
  "layout_nicely"

- ...:

  Additional arguments passed to
  [visIgraphLayout](https://rdrr.io/pkg/visNetwork/man/visIgraphLayout.html).
