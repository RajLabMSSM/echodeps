add_meta_pkgnet <- function(g,
                            pkg,
                            meta,
                            node_size=NULL){
  requireNamespace("data.table")
  requireNamespace("igraph")

  g <- set_metadata(g=g,
                    meta=meta)
  #### Set node size ####
  g <- set_node_size(g = g,
                     meta = meta,
                     pkg = pkg,
                     node_size = node_size)
  igraph::V(g)$group <- ifelse(igraph::V(g)==pkg, 'y', 'n')
  igraph::V(g)$outputs <- igraph::degree(g, mode = "out")
  igraph::V(g)$inputs <- igraph::degree(g, mode = "in")
  igraph::V(g)$title <- paste(
    paste0("<strong>Package</strong>: ",
           meta[names(igraph::V(g)),]$package
    ),
    paste0(
      "<strong>URL</strong>:<br> ","<a href='",igraph::V(g)$URL,"'",
      " target='_blank'>",igraph::V(g)$github_url,"</a>"
    ),
    paste0("<strong>Title</strong>: ",
           meta[names(igraph::V(g)),]$Title
    ),
    paste0("<strong>Description</strong>: ",
           meta[names(igraph::V(g)),]$Description
    ),
    paste0("<strong>Version</strong>: ",
           meta[names(igraph::V(g)),][["Version"]]
    ),
    paste0("<strong>Depends</strong>: ",
           meta[names(igraph::V(g)),]$Depends
    ),
    paste0("<strong>Imports</strong>: ",
           lapply(meta[names(igraph::V(g)),]$Imports,length),
           " packages"
    ),
    paste0("<strong>Suggests</strong>: ",
           lapply(meta[names(igraph::V(g)),]$Suggests,length),
           " packages"
    ),
    paste0("<strong>Remotes</strong>: ",
           lapply(meta[names(igraph::V(g)),]$Remotes,length),
           " packages"
    ),
    paste0("<strong>SystemRequirements</strong>: ",
           meta[names(igraph::V(g)),]$SystemRequirements
    ),
    sep="<br>"
  )
  # igraph::vertex.attributes(g)
  return(g)
}
