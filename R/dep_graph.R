#' Dependency graph plot
#'
#' Create a dependency graph between a set of R packages and plot them
#' as an interactive network. By default,
#' plots only packages within the
#'  \href{https://github.com/topics/echoverse}{\code{echoverse}}.
#' @param pkg Package to search dependencies for.
#' @param exclude A subset of of the main package's (\code{pkg} )
#' dependencies to exclude in the graph.
#' @param method_seed Method to create the initial dependency graph with:
#' \itemize{
#' \item{"pkgnet" (\code{reverse=FALSE} only)}{
#' Extracts all the R packages that the target R package depends
#' on using \link[pkgnet]{CreatePackageReport}}
#' \item{"github" (\code{reverse=FALSE or TRUE})}{
#' Extracts all the GitHub repositories that depend on the
#' respective GitHub repository of the target R package
#' using \link[echogithub]{github_dependents}}
#' \item{"devtools" (\code{reverse=TRUE}) only}{
#' Extracts reverse dependencies using \link[devtools]{revdep}.}
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
#' @param use_basename Only use the repo name to label each node.
#' @param save_path Path to save the plot to, as an interactive,
#'  self-container HTML file.
#' @param add_metadata Add metadata to the graph
#' using \link[echogithub]{github_metadata}.
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
#' res <- dep_graph(pkg = "echoverse",
#'                  layout=layout_star)
#' }
#' res <- dep_graph(pkg = "rworkflows", 
#'                  method_seed = "github",
#'                  exclude=c("neurogenomics_rworkflows",
#'                            "neurogenomics_r_workflows",
#'                            "NA"),
#'                  recursive = TRUE,
#'                  node_size = "total_downloads",
#'                  reverse = TRUE)
dep_graph <- function(pkg,
                      exclude = NULL,
                      method_seed = c("pkgnet","github","devtools"),
                      shape = c("image", "hexagon"),
                      image = "hex_sticker",
                      layout = layout_igraph,
                      colors = construct_colors(),
                      node_size = NULL,
                      font_face = "Tahoma",
                      show_plot = list(r=FALSE,
                                       browser=TRUE),
                      save_path = file.path(
                        tempdir(), 
                        paste0(basename(pkg),".dep_graph.html")
                      ),
                      width = "100%", 
                      height = "90vh",
                      reverse = FALSE,
                      recursive = FALSE,
                      use_basename = TRUE,
                      add_metadata = TRUE,
                      verbose = TRUE){

  # devoptera::args2vars(fn = dep_graph, reassign = TRUE);

  if(length(pkg)>1) {
    messager("Warning:: pkg has length >1.",
             "Only using the first package:",
             pkg[1],v=verbose)
  }
  pkg <- pkg[1]
  #### Gather dependency graph data ####
  if(isTRUE(reverse)){
      method_seed <- method_seed[method_seed!="pkgnet"][1]
      dgc_out <- revdep_graph_create(pkg = pkg,
                                     method_seed = method_seed,
                                     # method = method,
                                     recursive = recursive,
                                     exclude = exclude,
                                     node_size = node_size,
                                     add_metadata = add_metadata,
                                     verbose = verbose)
      g <- dgc_out$graph
  } else {
      method_seed <- method_seed[method_seed!="devtools"][1]
      dgc_out <- dep_graph_create(pkg = pkg,
                                  exclude = exclude,
                                  method = method_seed,
                                  node_size = node_size,
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
                        use_basename = use_basename,
                        verbose = verbose)
  return(list(plot=vis,
              graph=dgc_out$graph,
              subgraph=dgc_out$subgraph,
              report=dgc_out$report,
              pkg=pkg,
              save_path=save_path))
}
