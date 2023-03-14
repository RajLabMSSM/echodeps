add_meta_github <- function(g,
                            pkg,
                            meta,
                            node_size=NULL,
                            cols=names(meta),
                            verbose=TRUE){
    requireNamespace("data.table")
    requireNamespace("igraph")

    meta <- prep_metadata(meta=meta,
                          g=g,
                          verbose = verbose)
    ### Make all lowercase ####
    # names(meta) <- tolower(names(meta))
    # cols <- tolower(cols)
    #### Get cumulative metrics ####
    # df=data.table::as.data.table(g)
    g <- set_indirect_metadata(g = g,
                               meta = meta,
                               pkg_target = pkg,
                               cols = c("clones_uniques",
                                        "views_uniques",
                                        "total_downloads")
                               )
    #### Set most metadata vars ####
    g <- set_metadata(g=g,
                      meta=meta,
                      cols=cols)
    #### Set node size ####
    g <- set_node_size(g = g,
                       meta = meta,
                       pkg = pkg,
                       node_size = node_size)
    data.table::setkey(meta,"ref")
    igraph::V(g)$group <- ifelse(igraph::V(g)==pkg, 'y', 'n')
    igraph::V(g)$outputs <- igraph::degree(g, mode = "out")
    igraph::V(g)$inputs <- igraph::degree(g, mode = "in")

    igraph::V(g)$title <- paste(
        ##### Description data ####
        paste0("<strong>Package</strong>: ",
               meta[names(igraph::V(g)),]$Package
        ),
        paste0("<strong>Owner</strong>: ",
               meta[names(igraph::V(g)),]$owner
        ),
        paste0("<strong>Repo</strong>: ",
               meta[names(igraph::V(g)),]$repo
        ),
        paste0(
            "<strong>URL</strong>:<br> ","<a href='",meta[names(igraph::V(g)),]$URL,"'",
            " target='_blank'>",meta[names(igraph::V(g)),]$URL,"</a>"
        ),
        paste0("<strong>Description</strong>: ",
               meta[names(igraph::V(g)),]$Description
        ),
        paste0("<strong>Topics</strong>: ",
               lapply(meta[names(igraph::V(g)),]$topics,paste,collapse="; ")
        ),
        #### Traffic data ####
        paste0("<strong>Total downloads</strong>: ",
               meta[names(igraph::V(g)),]$total_downloads
        ),
        paste0("<strong>Clones</strong>: ",
               meta[names(igraph::V(g)),]$clones_uniques
        ),
        paste0("<strong>Views</strong>: ",
               meta[names(igraph::V(g)),]$views_uniques
        ),
        paste0("<strong>Stars</strong>: ",
               meta[names(igraph::V(g)),]$stargazers_count
        ),
        paste0("<strong>Forks</strong>: ",
               meta[names(igraph::V(g)),]$forks_count
        ),
        #### Indirect data ####
        paste0("<strong>Indirect total downloads</strong>: ",
               igraph::V(g)$indirect_total_downloads
        ),
        paste0("<strong>Indirect clones</strong>: ",
               igraph::V(g)$indirect_clones_uniques
        ),
        paste0("<strong>Indirect views</strong>: ",
               igraph::V(g)$indirect_views_uniques
        ),
        sep="<br>"
    )
    # igraph::vertex.attributes(g)
    return(g)
}
