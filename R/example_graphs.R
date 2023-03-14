#' Example graphs
#'
#' Generate a random list of  \link[tidygraph]{tbl_graph} objects.
#' @param n Number of nodes for each graph.
#' @param p The probability of nodes connecting between graphs (on a 0-1 scale).
#' @param colors Color for each graph.
#' @param seed Random seed.
#' @returns A list of  \link[tidygraph]{tbl_graph} objects.
#'
#' @export
#' @import igraph
#' @import pals
#' @importFrom tidygraph create_star
#' @examples
#' grs <- example_graphs()
example_graphs <- function(n=c(4,5,10),
                           p=0.5,
                           colors = pals::viridis(length(n)),
                           seed=2023){
    set.seed(seed)
    fun <- function(i,p){
        ni <- n[i]
        s <- ni/p
        l <- sample(seq_len(s),ni)
        g <- tidygraph::create_star(n=ni)
        igraph::vertex_attr(g,"name") <- paste0("node",l)
        igraph::E(g)$color <- colors[i]
        igraph::E(g)$color <- ni
        return(g)
    }
    grs <- Map(fun,seq_len(length(n)), p=p)
    return(grs)
}
