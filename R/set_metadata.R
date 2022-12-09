set_metadata <- function(g2,
                         meta,
                         cols){

    cols <- cols[cols %in% names(meta)]
    if(length(cols)==0) {
        messager("WARNING: No valid metadata cols to add to graph.")
    } else {
        for(x in cols){
            igraph::vertex_attr(g2,name = x) <- meta[names(igraph::V(g2)),][[x]]
        }
    }
    return(g2)
}
