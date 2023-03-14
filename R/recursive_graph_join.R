#' Recursively join a list of \link[tidygraph]{tbl_graph}s
#'
#' Joins a list of graphs into a single graph.
#' This recursively implements \link[tidygraph]{graph_join}.
#' @param grs a list of \link[tidygraph]{tbl_graph} objects.
#' @inheritParams tidygraph::graph_join
#' @returns a single tidygraph object
#'
#' @export
#' @examples
#' set.seed(2023)
#' grs <- example_graphs()
#' g <- recursive_graph_join(grs)
#' plot(g, edge.label=igraph::E(g)$color)
recursive_graph_join <- function(grs,
                                 by = "name") {
    if (length(grs) == 1) {
        # base case
        return(grs[[1]])
    } else {
        # recurse
        GR <- tidygraph::graph_join(grs[[1]],
                                    recursive_graph_join(grs[-1], by = by),
                                    by = by)
    }
    return(GR)
}
