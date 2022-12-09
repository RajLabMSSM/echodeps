dep_graph_add_meta_pkgnet <- function(g2,
                                      pkg_name,
                                      meta,
                                      node_size=NULL){
  requireNamespace("data.table")
  requireNamespace("igraph")

  g2 <- set_metadata(g2=g2,
                     meta=meta,
                     cols=c("URL","Version"))
  #### Set node size ####
  g2 <- set_node_size(g2 = g2,
                      meta = meta,
                      pkg_name = pkg_name,
                      node_size = node_size)
  igraph::V(g2)$group <- ifelse(igraph::V(g2)==pkg_name, 'y', 'n')
  igraph::V(g2)$outputs <- igraph::degree(g2, mode = "out")
  igraph::V(g2)$inputs <- igraph::degree(g2, mode = "in")
  igraph::V(g2)$title <- paste(
    paste0("<strong>Package</strong>: ",
           meta[names(igraph::V(g2)),]$Package
    ),
    paste0(
      "<strong>URL</strong>:<br> ","<a href='",igraph::V(g2)$URL,"'",
      " target='_blank'>",igraph::V(g2)$URL,"</a>"
    ),
    paste0("<strong>Title</strong>: ",
           meta[names(igraph::V(g2)),]$Title
    ),
    paste0("<strong>Description</strong>: ",
           meta[names(igraph::V(g2)),]$Description
    ),
    paste0("<strong>Version</strong>: ",
           meta[names(igraph::V(g2)),][["Version"]]
    ),
    paste0("<strong>Depends</strong>: ",
           meta[names(igraph::V(g2)),]$Depends
    ),
    paste0("<strong>Imports</strong>: ",
           lapply(meta[names(igraph::V(g2)),]$Imports,length),
           " packages"
    ),
    paste0("<strong>Suggests</strong>: ",
           lapply(meta[names(igraph::V(g2)),]$Suggests,length),
           " packages"
    ),
    paste0("<strong>Remotes</strong>: ",
           lapply(meta[names(igraph::V(g2)),]$Remotes,length),
           " packages"
    ),
    paste0("<strong>SystemRequirements</strong>: ",
           meta[names(igraph::V(g2)),]$SystemRequirements
    ),
    sep="<br>"
  )
  # igraph::vertex.attributes(g2)
  return(g2)
}
