#' Subset graph
#'
#' Subset a graph by nodes to include or exclude.
#' Automatically omits \code{include}/\code{exclude} nodes
#'  that don't exist in the graph to avoid errors.
#' @param include Names of nodes to include.
#' @param exclude Names of nodes to exclude.
#' @param verbose Print messages.
#' @inheritParams dep_graph_plot
#' @returns Subgraph.
#'
#' @export
#' @examples
#' dgc_out <- dep_graph_create(pkg = "rworkflows",
#'                             method = "github",
#'                             node_size = "clones_uniques")
#' g <- dgc_out$graph
#' exclude <- grep("actions-marketplace-validations",
#'                 names(igraph::V(g)),value = TRUE)
#' g2 <- subset_graph(g = g, exclude = exclude)
subset_graph <- function(g,
                         include=NULL,
                         exclude=NULL,
                         verbose=TRUE){
    requireNamespace("igraph")
    #### Nothing to subset ####
    if(all( sapply(list(include,exclude),is.null) )){
        return(g)
    }
    #### All nodes ####
    nodes <- names(igraph::V(g))
    #### Included nodes ####
    if(!is.null(include)){
        include_bool <- nodes %in% c(include,basename(include))
        messager("Including",
                 formatC(sum(include_bool),big.mark = ","),"nodes.",v=verbose)
        nodes <- nodes[include_bool]
    }
    #### Excluded nodes ####
    if(!is.null(exclude)){
        exclude_bool <- !nodes %in% c(exclude,basename(exclude))
        messager("Excluding",
                 formatC(sum(!exclude_bool),big.mark = ","),"nodes.",v=verbose)
        nodes <- nodes[exclude_bool]
    }
    #### Subset graph ####
    g2 <- igraph::induced_subgraph(
        graph = g,
        vids = igraph::V(g)[nodes]
    )
    return(g2)
}
