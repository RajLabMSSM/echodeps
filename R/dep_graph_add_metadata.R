dep_graph_add_meta <- function(g2,
                               pkg_name,
                               meta){
  requireNamespace("data.table")
  requireNamespace("igraph")

  igraph::V(g2)$URL <- meta[names(igraph::V(g2)),]$URL
  igraph::V(g2)$Version <- meta[names(igraph::V(g2)),]$Version
  igraph::V(g2)$value <- ifelse(names(igraph::V(g2))==pkg_name, 40, 30)
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
