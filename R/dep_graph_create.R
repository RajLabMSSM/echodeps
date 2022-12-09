#' Create a dependency graph
#'
#' Create a dependency graph.
#' @inheritParams dep_graph
#' @export
#' @examples
#' dgc_out <- dep_graph_create(pkg_name = "rworkflows",
#'                             method = "github")
dep_graph_create <- function(pkg_name,
                             deps = NULL,
                             method = c("pkgnet","github"),
                             node_size = NULL,
                             verbose = TRUE){

    method <- tolower(method)[1]
    #### Select method ####
    if(method=="pkgnet"){
        dgc_out <- dep_graph_create_pkgnet(pkg_name=pkg_name,
                                           deps=deps,
                                           node_size=node_size,
                                           verbose=verbose)
   } else if(method=="github"){
       dgc_out <- dep_graph_create_github(pkg_name=pkg_name,
                                          deps=deps,
                                          node_size=node_size,
                                          verbose=verbose)
   } else {
        stopper("method must be one of:",
                paste("\n -",
                      shQuote(eval(formals(dep_graph_create)$method)),
                      collapse = ""))
   }
    return(dgc_out)
}
