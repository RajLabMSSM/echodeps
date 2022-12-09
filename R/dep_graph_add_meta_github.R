dep_graph_add_meta_github <- function(g2,
                                      pkg_name,
                                      meta,
                                      node_size=NULL){
    requireNamespace("data.table")
    requireNamespace("igraph")
    owner_repo <- NULL;

    #### Get cumulative metrics ####
    clones_uniques_sum <-
        sum(meta[owner_repo!=pkg_name,]$clones_uniques, na.rm = TRUE)
    views_uniques_sum <-
        sum(meta[owner_repo!=pkg_name,]$views_uniques, na.rm = TRUE)
    igraph::V(g2)$indirect_clones <- ifelse(
        names(igraph::V(g2))==pkg_name,
        paste("<strong>","Indirect clones","</strong>:",
              clones_uniques_sum), "")
    igraph::V(g2)$indirect_views <- ifelse(
        names(igraph::V(g2))==pkg_name,
        paste("<strong>","Indirect views","</strong>:",
              views_uniques_sum), "")

    g2 <- set_metadata(g2=g2,
                       meta=meta,
                       cols=c("owner","repo",
                              "clones_count","clones_uniques",
                              "views_count","views_uniques"))
    #### Set node size ####
    g2 <- set_node_size(g2 = g2,
                        meta = meta,
                        pkg_name = pkg_name,
                        node_size = node_size)
    igraph::V(g2)$group <- ifelse(igraph::V(g2)==pkg_name, 'y', 'n')
    igraph::V(g2)$outputs <- igraph::degree(g2, mode = "out")
    igraph::V(g2)$inputs <- igraph::degree(g2, mode = "in")

    igraph::V(g2)$title <- paste(
        paste0("<strong>Owner</strong>: ",
               meta[names(igraph::V(g2)),]$owner
        ),
        paste0("<strong>Repo</strong>: ",
               meta[names(igraph::V(g2)),]$repo
        ),
        paste0(
            "<strong>URL</strong>:<br> ","<a href='",igraph::V(g2)$url,"'",
            " target='_blank'>",igraph::V(g2)$url,"</a>"
        ),
        paste0("<strong>Description</strong>: ",
               meta[names(igraph::V(g2)),]$description
        ),
        paste0("<strong>Topics</strong>: ",
               lapply(meta[names(igraph::V(g2)),]$topics,paste,collapse="; ")
        ),
        paste0("<strong>Clones</strong>: ",
               meta[names(igraph::V(g2)),]$clones_uniques
        ),
        paste0("<strong>Views</strong>: ",
               meta[names(igraph::V(g2)),]$views_uniques
        ),
        paste0("<strong>Stars</strong>: ",
               meta[names(igraph::V(g2)),]$stargazers_count
        ),
        paste0("<strong>Forks</strong>: ",
               meta[names(igraph::V(g2)),]$forks_count
        ),
        igraph::V(g2)$indirect_clones,
        igraph::V(g2)$indirect_views,
        sep="<br>"
    )
    # igraph::vertex.attributes(g2)
    return(g2)
}
