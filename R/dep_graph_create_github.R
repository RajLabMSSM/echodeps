dep_graph_create_github <- function(pkg_name,
                                    deps,
                                    add_traffic=TRUE,
                                    sep="/\n",
                                    node_size=NULL,
                                    verbose=TRUE){
    requireNamespace("echogithub")
    requireNamespace("igraph")
    owner_repo <- target <- NULL;

    messager("Generating `github` package report.",v=verbose)
    #### Get repo owner/name ####
    desc_file <- echogithub::description_find(repo = pkg_name,
                                              verbose = verbose)
    URL <- echogithub::description_extract(desc_file = desc_file,
                                           fields = "URL")[[1]]
    owner <- rev(strsplit(URL,"/")[[1]])[2]
    repo <- rev(strsplit(URL,"/")[[1]])[1]
    #### Get dependent from GitHub ####
    dt <- echogithub::github_dependents(owner = owner,
                                        repo = repo,
                                        verbose = verbose)
    if(is.null(dt)){
        stopper("Could not find any dependents on GitHub.")
    }
    dt[,owner_repo:=paste(owner,repo,sep=sep)]
    dt[,target:=gsub("/",sep,target)]

    #### Check second-order dependents ####
    # dt2 <- lapply(seq_len(nrow(dt)), function(i){
    #     echogithub::github_dependents(owner = dt[i,]$owner,
    #                                   repo = dt[i,]$repo,
    #                                   verbose = verbose)
    # })
    #### Create graph ####
    g <- igraph::graph_from_data_frame(
        d = dt[,c("owner_repo","target","stargazers_count")])
    #### Subset graph ####
    deps <- deps[deps %in% dt$repo]
    g2 <- subset_graph(g = g,
                       include = deps,
                       verbose = verbose)
    #### Get repo metadata ####
    meta <- lapply(seq_len(nrow(dt)), function(i){
        echogithub::github_metadata(owner = dt[i,]$owner,
                                    repo = dt[i,]$repo,
                                    add_traffic = TRUE,
                                    verbose = verbose)
    }) |> data.table::rbindlist(fill = TRUE)
    meta[,owner_repo:=paste(owner,repo,sep=sep)]
    meta[,URL:=paste(owner,repo,sep=sep)]
    data.table::setkeyv(meta,"owner_repo")
    ##### Add repo metadata ####
    pkg_name2 <- paste(owner,repo,sep=sep)
    g2 <- dep_graph_add_meta_github(g2 = g2,
                                    pkg_name = pkg_name2,
                                    meta = meta,
                                    node_size = node_size)
    #### Return ####
    return(list(pkg_name=pkg_name2,
                graph=g,
                subgraph=g2,
                report=dt,
                metadata=meta))
}
