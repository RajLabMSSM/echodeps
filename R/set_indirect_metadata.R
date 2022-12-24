set_indirect_metadata <- function(g,
                                  meta,
                                  pkg_target,
                                  cols){
    owner_repo <- NULL;
    for(col in cols){
        if(col %in% names(meta)){
            #### Compute sum ####
            col_sum <- sum(meta[owner_repo!=pkg_target,][[col]],
                           na.rm = TRUE)
            #### Add to graph ####
            igraph::vertex_attr(g, paste0("indirect_",col)) <-
                lapply(names(igraph::V(g)), function(p){
                    if(p==pkg_target){
                        col_sum
                    } else {
                        meta[p,][[col]]
                    }
                }) |> unlist()
        }
    }
    return(g)
}
