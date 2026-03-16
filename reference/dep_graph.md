# Dependency graph plot

Create a dependency graph between a set of R packages and plot them as
an interactive network. By default, plots only packages within the
[`echoverse`](https://github.com/topics/echoverse).

## Usage

``` r
dep_graph(
  pkg,
  exclude = NULL,
  method_seed = c("pkgnet", "github", "devtools"),
  shape = c("image", "hexagon"),
  image = "hex_sticker",
  layout = layout_igraph,
  colors = construct_colors(),
  node_size = NULL,
  font_face = "Tahoma",
  show_plot = list(r = FALSE, browser = TRUE),
  save_path = file.path(tempdir(), paste0(basename(pkg), ".dep_graph.html")),
  width = "100%",
  height = "90vh",
  reverse = FALSE,
  recursive = FALSE,
  use_basename = TRUE,
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

- colors:

  Colors for each of the graph plot elements. Can use the
  [construct_colors](https://rajlabmssm.github.io/echodeps/reference/construct_colors.md)
  function to help create this.

- node_size:

  Strategy for setting node sizes. Can be one of:

  NULL :

  :   Sets the target package node's size to 40 and the rest to 30

  \<numeric\> :

  :   User-provided node size. Sets all nodes to the same size.

  \<character\> :

  :   User-provided metadata column name. Scales node size to a vector
      of numeric values in the node metadata.

- font_face:

  Font to use for plot text.

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

- reverse:

  `FALSE` (default)

  :   Create a forward dependency graph (packages that `pkg` depends on,
      and the packages that they depend on).

  `TRUE`

  :   Create a reverse dependency graph (packages that depend on `pkg`,
      and the packages that depend on them).

- recursive:

  If `TRUE` look for full set of recursive dependencies.

- use_basename:

  Only use the repo name to label each node.

- add_metadata:

  Add metadata to the graph using
  [github_metadata](https://rdrr.io/pkg/echogithub/man/github_metadata.html).

- verbose:

  Print messages.

## Examples

``` r
if (FALSE) { # \dontrun{
#### not run simply bc it causes weird errors with devtools::check() ####
res <- dep_graph(pkg = "echoverse",
                 layout=layout_star)
} # }
if (FALSE) { # \dontrun{
res <- dep_graph(pkg = "rworkflows",
                 method_seed = "github",
                 exclude=c("neurogenomics_rworkflows",
                           "neurogenomics_r_workflows",
                           "NA"),
                 recursive = TRUE,
                 node_size = "total_downloads",
                 reverse = TRUE)
} # }
```
