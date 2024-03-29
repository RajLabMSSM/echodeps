% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dep_graph_create.R
\name{dep_graph_create}
\alias{dep_graph_create}
\title{Create a dependency graph}
\usage{
dep_graph_create(
  pkg,
  exclude = NULL,
  method = c("pkgnet", "github"),
  node_size = NULL,
  add_metadata = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{pkg}{Package to search dependencies for.}

\item{exclude}{A subset of of the main package's (\code{pkg} )
dependencies to exclude in the graph.}

\item{method}{Seed method for extracting first-order dependencies
of \code{pkg}.}

\item{node_size}{Strategy for setting node sizes. Can be one of:
\itemize{
\item{NULL : }{Sets the target package node's size to 40 and the rest to 30}.
\item{<numeric> : }{User-provided node size.
Sets all nodes to the same size.}
\item{<character> : }{User-provided metadata column name.
Scales node size to a vector of numeric values in the node metadata.}
}}

\item{add_metadata}{Add metadata to the graph
using \link[echogithub]{github_metadata}.}

\item{verbose}{Print messages.}
}
\description{
Create a dependency graph.
}
\examples{
dgc_out <- dep_graph_create(pkg = "rworkflows",
                            method = "github")
}
