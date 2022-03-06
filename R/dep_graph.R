#' Dependency graph plot
#'
#' Create a dependency graph between a set of R packages and plot them
#' as an interactive network. By default,
#' plots only packages within the
#'  \href{https://github.com/topics/echoverse}{\code{echoverse}}.
#' @param pkg_name Package to search dependencies for.
#' @param deps A subset of of the main package's (\code{pkg_name} )
#' dependencies to include in the plot visualization.
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
#' "graph" and "pkg_name".
#' See \code{echodeps::layout_star} for an example.
#' @param save_path Path to save the plot to, as an interactive,
#'  self-container HTML file.
#' @param verbose Print messages.
#' @inheritParams visNetwork::visNodes
#' @inheritParams visNetwork::visSave
#' @inheritParams visNetwork::visOptions
#' @export
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' #### not run simply bc it causes weird errors with devtools::check() ####
#' res <- echodeps::dep_graph()
#' }
dep_graph <- function(pkg_name = "echolocatoR",
                      deps = NULL,
                      shape = c("image", "hexagon"),
                      image =
                        file.path(
                          "https://github.com/RajLabMSSM",
                          "Fine_Mapping/blob/master/echolocatoR",
                          "images/bat_silhouette.png?raw=true"
                        ),
                      layout = echodeps::layout_star,
                      show_plot = list(r=FALSE,
                                       browser=TRUE),
                      save_path = file.path(
                        tempdir(), paste0(pkg_name,".dep_graph.html")
                        ),
                      background = "#25355c",
                      width = "100%",
                      height = "500px",
                      verbose = TRUE){
  requireNamespace("pkgnet")
  requireNamespace("igraph")
  # echodeps:::source_all(packages = "dplyr");
  # args2vars(fn = dep_graph);

  if(length(pkg_name)>1) {
    messager("Warning:: pkg_name has length >1. Only using the first package:",
             pkg_name[1],v=verbose)
  }
  pkg_name <- pkg_name[1]
  #### Gather dependency graph data ####
  dgc_out <- dep_graph_create(pkg_name = pkg_name,
                              deps = deps,
                              verbose = verbose)
  #### Create interactive plot ####
  vis <- dep_graph_plot(subgraph = dgc_out$subgraph,
                        shape = shape,
                        image = image,
                        layout = layout,
                        pkg_name = pkg_name,
                        show_plot = show_plot,
                        save_path = save_path,
                        width = width,
                        height = height,
                        background = background,
                        verbose = verbose)
  return(list(plot=vis,
              metadata=dgc_out$metadata,
              graph=dgc_out$graph,
              subgraph=dgc_out$subgraph,
              pkgnet_report=dgc_out$report,
              pkg_name=pkg_name,
              save_path=save_path))
}
