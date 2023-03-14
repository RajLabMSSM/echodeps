#' Merge graphs
#'
#' Merge a list of graphs into a single graph.
#' Can take in a list of \link[igraph]{igraph}s or
#' a list of \link[igraph]{igraph}s,
#' and can return a merged
#'  \link[tidygraph]{tbl_graph} or a merged \link[igraph]{igraph}.
#' @param graph_list A list of graphs.
#' @param output Output format to return the graph in.
#' @inheritParams dep_graph
#' @inheritParams tidygraph::graph_join
#' @returns Merge \link[tidygraph]{tbl_graph} or \link[igraph]{igraph}.
#'
#' @export
#' @importFrom tidygraph as_tbl_graph as.igraph
#' @import igraph
#' @examples
#' set.seed(2023)
#' graph_list <- example_graphs()
#' g <- merge_graphs(graph_list)
#' plot(g, edge.label=igraph::E(g)$color)
merge_graphs <- function(graph_list,
                         node_size,
                         output = c("tidygraph","igraph"),
                         by = "ref",
                         verbose = TRUE){

    output <- output[1]
    #### Make all node names the basename #####
    # graph_list <- lapply(graph_list, function(g){
    #     igraph::V(g)$name <- basename(igraph::V(g)$name)
    #     return(g)
    # })
    #### Convert to tidygraph ####
    tg_list <- lapply(graph_list, tidygraph::as_tbl_graph)
    g <- recursive_graph_join(grs = tg_list,
                              by = by)
    igraph::V(g)$name <- igraph::vertex_attr(g,by[1])
    #### Convert to igraph ####
    if(output=="igraph"){
        g <- tidygraph::as.igraph(g)
    }
    return(g)
}
