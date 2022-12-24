set_node_size <- function(g,
                          meta,
                          pkg,
                          node_size){
    if(is.character(node_size)){
        node_size <- node_size[node_size %in% names(meta)]
    }
    if(length(node_size)==0){
        igraph::V(g)$value <- ifelse(names(igraph::V(g))==pkg, 40, 30)
    } else {
        igraph::vertex_attr(g,name = "value") <-
            meta[names(igraph::V(g)),][[node_size]]
    }
    return(g)
}
