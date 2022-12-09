set_node_size <- function(g2,
                          meta,
                          pkg_name,
                          node_size){
    if(is.character(node_size)){
        node_size <- node_size[node_size %in% names(meta)]
    }
    if(length(node_size)==0){
        igraph::V(g2)$value <- ifelse(names(igraph::V(g2))==pkg_name, 40, 30)
    } else {
        igraph::vertex_attr(g2,name = "value") <-
            meta[names(igraph::V(g2)),][[node_size]]
    }
    return(g2)
}
