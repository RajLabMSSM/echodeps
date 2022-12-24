merge_graphs <- function(res1,
                         res2,
                         node_size,
                         verbose=TRUE){

    #### Get formatted target package name ####
    pkg <- res1$pkg
    #### Merge graphs ####
    g <- igraph::union(res1$subgraph,
                       res2$subgraph)
    #### Identify duplicate coluns created by igraph::union ####
    #### Drop list columns ####
    # res1$metadata <- drop_list_cols(res1$metadata)
    # res2$metadata <- drop_list_cols(res2$metadata)
    # #### Find columns to merge by ####
    # join_names <- base::intersect( names(res2$metadata),
    #                                names(res1$metadata))
    # #### Merge metadata ####
    # meta <- data.table::merge.data.table(
    #     res1$metadata,
    #     res2$metadata,
    #     by=join_names,
    #     all = TRUE)
    ##### Easier to just collect new metadata ####
    meta <- echogithub::r_repos_data(include = unique(names(igraph::V(g))),
                                     add_downloads = TRUE,
                                     add_descriptions = TRUE,
                                     add_github = TRUE,
                                     cast = TRUE,
                                     verbose = verbose)
    data.table::setkey(meta,"repo")
    #### Add the fixed metadata back in ####
    g <- add_meta_github(g = g,
                         pkg = pkg,
                         meta = meta,
                         node_size = node_size)
    #### Remove the dup metadata columns from graph ####
    vat <- igraph::vertex.attributes(g)
    dup_names <- grep("_[0-9]$",names(vat), value = TRUE)
    for(x in dup_names){
        g <- igraph::remove.vertex.attribute(g,x)
    }
    # g2 =igraph::graph.disjoint.union(seed_deps$subgraph,dgc_out$subgraph)
    return(list(pkg=pkg,
                graph=g,
                metadata=meta))
}
