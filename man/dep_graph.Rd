% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dep_graph.R
\name{dep_graph}
\alias{dep_graph}
\title{Dependency graph plot}
\usage{
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
  width = "100\%",
  height = "90vh",
  reverse = FALSE,
  recursive = FALSE,
  use_basename = TRUE,
  add_metadata = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{pkg}{Package to search dependencies for.}

\item{exclude}{A subset of of the main package's (\code{pkg} )
dependencies to exclude in the graph.}

\item{method_seed}{Method to create the initial dependency graph with:
\itemize{
\item{"pkgnet" (\code{reverse=FALSE} only)}{
Extracts all the R packages that the target R package depends
on using \link[pkgnet]{CreatePackageReport}}
\item{"github" (\code{reverse=FALSE or TRUE})}{
Extracts all the GitHub repositories that depend on the
respective GitHub repository of the target R package
using \link[echogithub]{github_dependents}}
\item{"devtools" (\code{reverse=TRUE}) only}{
Extracts reverse dependencies using \link[devtools]{revdep}.}
}
If more than one option is provided, only the first is used.}

\item{shape}{: String. Default to 'ellipse'. The shape defines what the node looks like. There are two types of nodes. One type has the label inside of it and the other type has the label underneath it. The types with the label inside of it are: ellipse, circle, database, box, text. The ones with the label outside of it are: image, circularImage, diamond, dot, star, triangle, triangleDown, hexagon, square and icon.}

\item{image}{: List or String. Default to undefined. When the shape is set to image or circularImage, this option should be the URL to an image. If the image cannot be found, the brokenImage option can be used.
\itemize{
  \item{"unselected"}{ : String. Unselected (default) image URL.}
  \item{"selected"}{ : String. Selected image URL.}
}}

\item{layout}{\pkg{visNetwork} layout function
(e.g. \link[visNetwork]{visLayout} or \link[visNetwork]{visIgraphLayout}
to specify plot layout. The function must take two arguments:
"graph" and "pkg".
See \code{echodeps::layout_star} for an example.
Alternatively, you may set \code{layout=NULL} to use the
default force-directed \link[visNetwork]{visIgraph} layout, "layout_nicely".}

\item{colors}{Colors for each of the graph plot elements.
Can use the \link[echodeps]{construct_colors} function to help create this.}

\item{node_size}{Strategy for setting node sizes. Can be one of:
\itemize{
\item{NULL : }{Sets the target package node's size to 40 and the rest to 30}.
\item{<numeric> : }{User-provided node size.
Sets all nodes to the same size.}
\item{<character> : }{User-provided metadata column name.
Scales node size to a vector of numeric values in the node metadata.}
}}

\item{font_face}{Font to use for plot text.}

\item{show_plot}{A named list with two items:
\itemize{
\item{r}{Whether to show the dependency graph in R/Rstudio
(will \emph{not} show user-specific \code{background} color)}.
\item{browser}{Whether to show the dependency graph in default web browser
(will show user-specific \code{background} color)}.
}}

\item{save_path}{Path to save the plot to, as an interactive,
self-container HTML file.}

\item{width}{: String. Default to "100\%". The width of the network in pixels or as a percentage.}

\item{height}{: String. Default to "100\%". The height of the network in pixels or as a percentage.}

\item{reverse}{\itemize{
\item{\code{FALSE} (default)}{Create a forward dependency graph
(packages that \code{pkg} depends on,
and the packages that they depend on).}
\item{\code{TRUE}}{Create a reverse dependency graph
 (packages that depend on \code{pkg},
 and the packages that depend on them).}
}}

\item{recursive}{If \code{TRUE} look for full set of recursive dependencies.}

\item{use_basename}{Only use the repo name to label each node.}

\item{add_metadata}{Add metadata to the graph
using \link[echogithub]{github_metadata}.}

\item{verbose}{Print messages.}
}
\description{
Create a dependency graph between a set of R packages and plot them
as an interactive network. By default,
plots only packages within the
 \href{https://github.com/topics/echoverse}{\code{echoverse}}.
}
\examples{
\dontrun{
#### not run simply bc it causes weird errors with devtools::check() ####
res <- dep_graph(pkg = "echoverse",
                 layout=layout_star)
}
res <- dep_graph(pkg = "rworkflows",
                 method_seed = "github",
                 reverse = TRUE)
}
