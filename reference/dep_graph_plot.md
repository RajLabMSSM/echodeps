# Dependency graph plot

Plot a dependency graph between a set of R packages as an interactive
network.

## Usage

``` r
dep_graph_plot(
  g,
  pkg,
  shape = c("image", "hexagon"),
  image = "hex_sticker",
  layout = layout_igraph,
  hide_tooltip = TRUE,
  show_plot = list(r = TRUE, browser = TRUE),
  save_path = file.path(tempdir(), paste0(basename(pkg), ".dep_graph.html")),
  width = "100%",
  height = "90vh",
  colors = construct_colors(),
  font_face = "Tahoma",
  use_basename = FALSE,
  verbose = TRUE
)
```

## Arguments

- g:

  A graph of class
  [igraph](https://r.igraph.org/reference/aaa-igraph-package.html).
  *Warning:* Large graphs may be slow to interact with.

- pkg:

  Package to search dependencies for.

- shape:

  : String. Default to 'ellipse'. The shape defines what the node looks
  like. There are two types of nodes. One type has the label inside of
  it and the other type has the label underneath it. The types with the
  label inside of it are: ellipse, circle, database, box, text. The ones
  with the label outside of it are: image, circularImage, diamond, dot,
  star, triangle, triangleDown, hexagon, square and icon.

- image:

  : List or String. Default to undefined. When the shape is set to image
  or circularImage, this option should be the URL to an image. If the
  image cannot be found, the brokenImage option can be used.

  - "unselected" String. Unselected (default) image URL.

  - "selected" String. Selected image URL.

- layout:

  visNetwork layout function (e.g.
  [visLayout](https://rdrr.io/pkg/visNetwork/man/visLayout.html) or
  [visIgraphLayout](https://rdrr.io/pkg/visNetwork/man/visIgraphLayout.html)
  to specify plot layout. The function must take two arguments: "graph"
  and "pkg". See
  [`echodeps::layout_star`](https://rajlabmssm.github.io/echodeps/reference/layout_star.md)
  for an example. Alternatively, you may set `layout=NULL` to use the
  default force-directed
  [visIgraph](https://rdrr.io/pkg/visNetwork/man/visNetwork-igraph.html)
  layout, "layout_nicely".

- hide_tooltip:

  Hide the tooltips by default, and only show them when the cursor is
  hovering over the respective node.

- show_plot:

  A named list with two items:

  r

  :   Whether to show the dependency graph in R/Rstudio (will *not* show
      user-specific `background` color)

  browser

  :   Whether to show the dependency graph in default web browser (will
      show user-specific `background` color)

- save_path:

  Path to save the plot to, as an interactive, self-container HTML file.

- width:

  : String. Default to "100%". The width of the network in pixels or as
  a percentage.

- height:

  : String. Default to "100%". The height of the network in pixels or as
  a percentage.

- colors:

  Colors for each of the graph plot elements. Can use the
  [construct_colors](https://rajlabmssm.github.io/echodeps/reference/construct_colors.md)
  function to help create this.

- font_face:

  Font to use for plot text.

- use_basename:

  Only use the repo name to label each node.

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
dgc_out <- dep_graph_create(pkg = "rworkflows",
                            method = "github")
vis <- dep_graph_plot(g = dgc_out$subgraph,
                      pkg = dgc_out$pkg)
} # }
```
