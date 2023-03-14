#' Create a dependency graph
#'
#' Create a dependency graph.
#' @param method Seed method for extracting first-order dependencies
#'  of \code{pkg}.
#' @inheritParams dep_graph
#' @inheritParams subset_graph
#' @export
#' @examples
#' dgc_out <- dep_graph_create(pkg = "rworkflows",
#'                             method = "github")
dep_graph_create <- function(pkg,
                             exclude = NULL,
                             method = c("pkgnet","github"),
                             node_size = NULL,
                             add_metadata = TRUE,
                             verbose = TRUE){

    method <- tolower(method)[1]
    #### Select method ####
    if(method=="pkgnet"){
        dgc_out <- dep_graph_create_pkgnet(pkg=pkg,
                                           exclude=exclude,
                                           node_size=node_size,
                                           verbose=verbose)
   } else if(method=="github"){
       dgc_out <- dep_graph_create_github(refs=pkg,
                                          exclude=exclude,
                                          node_size=node_size,
                                          reverse=FALSE,
                                          add_metadata=add_metadata,
                                          verbose=verbose)
   } else {
        stopper("method must be one of:",
                paste("\n -",
                      shQuote(eval(formals(dep_graph_create)$method)),
                      collapse = ""))
   }
    return(dgc_out)
}
