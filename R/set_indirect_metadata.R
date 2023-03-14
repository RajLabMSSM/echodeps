set_indirect_metadata <- function(g,
                                  meta,
                                  pkg_target,
                                  cols){
    pkg_target_opts <- c(
        pkg_target,
        pkg_to_ref(g = g,
                   pkgs = pkg_target)
    ) |> unique()
    owner_repo <- NULL;
    for(col in cols){
        if(col %in% names(meta)){
            #### Compute sum ####
            col_sum <- sum(meta[!owner_repo %in% pkg_target_opts,][[col]],
                           na.rm = TRUE)
            #### Add to graph ####
            igraph::vertex_attr(g, paste0("indirect_",col)) <-
                lapply(names(igraph::V(g)), function(p){
                    if(p %in% pkg_target_opts){
                        col_sum
                    } else {
                        sum(meta[p,][[col]],na.rm = TRUE)
                    }
                }) |> unlist()
        }
    }
    return(g)
}
