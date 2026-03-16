# igraph layout

Layout function uses any
[visIgraphLayout](https://rdrr.io/pkg/visNetwork/man/visIgraphLayout.html)

## Usage

``` r
layout_igraph(graph, pkg, layout = "layout_with_kk", ...)
```

## Arguments

- graph:

  : a visNetwork object

- pkg:

  Unused argument; included here for compatibility with other layout
  functions.

- layout:

  : Character Name of igraph layout function to use. Default to
  "layout_nicely"

- ...:

  Additional arguments passed to
  [visIgraphLayout](https://rdrr.io/pkg/visNetwork/man/visIgraphLayout.html).
