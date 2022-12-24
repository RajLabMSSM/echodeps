#' Dependency graph plot
#'
#' Create a dependency graph between a set of R packages and plot them
#' as an interactive network. By default,
#' plots only packages within the
#'  \href{https://github.com/topics/echoverse}{\code{echoverse}}.
#' @param pkg Package to search dependencies for.
#' @param exclude A subset of of the main package's (\code{pkg} )
#' dependencies to exclude in the graph.
#' @param method Method to create the dependency graph with:
#' \itemize{
#' \item{"pkgnet"}{Extracts all the R packages that the target R package depends
#' on using \link[pkgnet]{CreatePackageReport}}
#' \item{"github"}{Extracts all the GitHub repositories that depend on the
#' respective GitHub repository of the target R package
#' using \link[echogithub]{github_dependents}}
#' }
#' If more than one option is provided, only the first is used.
#' @param show_plot A named list with two items:
#' \itemize{
#' \item{r}{Whether to show the dependency graph in R/Rstudio
#' (will \emph{not} show user-specific \code{background} color)}.
#' \item{browser}{Whether to show the dependency graph in default web browser
#' (will show user-specific \code{background} color)}.
#' }
#' @param layout \pkg{visNetwork} layout function
#' (e.g. \link[visNetwork]{visLayout} or \link[visNetwork]{visIgraphLayout}
#' to specify plot layout. The function must take two arguments:
#' "graph" and "pkg".
#' See \code{echodeps::layout_star} for an example.
#' Alternatively, you may set \code{layout=NULL} to use the
#' default force-directed \link[visNetwork]{visIgraph} layout, "layout_nicely".
#' @param colors Colors for each of the graph plot elements.
#' Can use the \link[echodeps]{construct_colors} function to help create this.
#' @param font_face Font to use for plot text.
#' @param node_size Strategy for setting node sizes. Can be one of:
#' \itemize{
#' \item{NULL : }{Sets the target package node's size to 40 and the rest to 30}.
#' \item{<numeric> : }{User-provided node size.
#' Sets all nodes to the same size.}
#' \item{<character> : }{User-provided metadata column name.
#' Scales node size to a vector of numeric values in the node metadata.}
#' }
#' @param reverse
#' \itemize{
#' \item{\code{FALSE} (default)}{Create a forward dependency graph
#' (packages that \code{pkg} depends on,
#' and the packages that they depend on).}
#' \item{\code{TRUE}}{Create a reverse dependency graph
#'  (packages that depend on \code{pkg},
#'  and the packages that depend on them).}
#' }
#' @param sep Character separator between owner and repo, to label each node.
#' @param use_basename Only use the repo name to label each node.
#' @param save_path Path to save the plot to, as an interactive,
#'  self-container HTML file.
#' @param verbose Print messages.
#' @inheritParams visNetwork::visNodes
#' @inheritParams visNetwork::visSave
#' @inheritParams visNetwork::visOptions
#' @inheritParams devtools::revdep
#' @export
#' @importFrom data.table :=
#' @examples
#' \dontrun{
#' #### not run simply bc it causes weird errors with devtools::check() ####
#' res <- dep_graph(pkg = "echoverse")
#' }
#' res <- dep_graph(pkg = "rworkflows",
#'                  method = "github",
#'                  use_basename = T, reverse=T)
dep_graph <- function(pkg = "echolocatoR",
                      exclude = NULL,
                      method = c("pkgnet","github"),
                      shape = c("image", "hexagon"),
                      image =
                        file.path(
                          "https://github.com/RajLabMSSM",
                          "Fine_Mapping/blob/master/echolocatoR",
                          "images/bat_silhouette.png?raw=true"
                        ),
                      layout = echodeps::layout_star,
                      colors = echodeps::construct_colors(),
                      node_size = NULL,
                      font_face = "Tahoma",
                      show_plot = list(r=FALSE,
                                       browser=TRUE),
                      save_path = file.path(
                        tempdir(), paste0(basename(pkg),
                                          ".dep_graph.html")),
                      width = "100%",
                      height = "500px",
                      reverse = FALSE,
                      recursive = FALSE,
                      use_basename = TRUE,
                      sep = "/\n",
                      verbose = TRUE){
  # echoverseTemplate:::source_all();
  # echoverseTemplate:::args2vars(fn = dep_graph);

  if(length(pkg)>1) {
    messager("Warning:: pkg has length >1.",
             "Only using the first package:",
             pkg[1],v=verbose)
  }
  pkg <- pkg[1]
  #### Gather dependency graph data ####
  if(isTRUE(reverse)){
      dgc_out <- revdep_graph_create(pkg = pkg,
                                     method_seed = method,
                                     # method = method,
                                     recursive = recursive,
                                     exclude = exclude,
                                     node_size = node_size,
                                     use_basename = use_basename,
                                     sep = sep,
                                     verbose = verbose)
      g <- dgc_out$graph
  } else {
      dgc_out <- dep_graph_create(pkg = pkg,
                                  exclude = exclude,
                                  method = method,
                                  node_size = node_size,
                                  use_basename = use_basename,
                                  sep = sep,
                                  verbose = verbose)
      g <- dgc_out$subgraph
  }
  #### Create interactive plot ####
  vis <- dep_graph_plot(g = g,
                        shape = shape,
                        image = image,
                        layout = layout,
                        pkg = dgc_out$pkg,
                        show_plot = show_plot,
                        save_path = save_path,
                        width = width,
                        height = height,
                        colors = colors,
                        font_face = font_face,
                        verbose = verbose)
  return(list(plot=vis,
              metadata=dgc_out$metadata,
              graph=dgc_out$graph,
              subgraph=dgc_out$subgraph,
              report=dgc_out$report,
              pkg=pkg,
              save_path=save_path))
}
