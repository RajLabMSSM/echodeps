set_metadata <- function(g,
                         meta,
                         cols){
    #### IMPORTANT! Must omit list columns####
    meta <- drop_list_cols(meta)
    cols <- cols[cols %in% names(meta)]
    #### IMPORTANT! Must omit columns with special igraph names ####
    cols <- cols[!cols %in% c("id","value","name","size","title")]
    #### Add metadata ####
    if(length(cols)==0) {
        messager("WARNING: No valid metadata cols to add to graph.")
    } else {
        for(x in cols){
            igraph::vertex_attr(g,name = x) <- meta[names(igraph::V(g)),][[x]]
        }
    }
    return(g)
}
