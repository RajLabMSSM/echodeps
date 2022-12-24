#' Create a dependency graph: GitHub
#'
#' Create a dependency graph: GitHub.
#' @inheritParams dep_graph
#' @inheritParams revdep_graph_create
#' @inheritParams echogithub::github_metadata
#' @importFrom data.table := rbindlist
#' @importFrom echogithub github_metadata description_extract
#' @importFrom echogithub github_dependents github_dependencies
dep_graph_create_github <- function(pkg,
                                    exclude=NULL,
                                    add_traffic=TRUE,
                                    sep="/\n",
                                    use_basename=TRUE,
                                    node_size=NULL,
                                    reverse=FALSE,
                                    verbose=TRUE){
    requireNamespace("igraph")
    owner_repo <- target <- owner <- repo <- package <- NULL;

    messager("Generating `github` package report.",v=verbose)
    #### Get repo owner/name ####
    info <- echogithub::description_extract(repo = pkg,
                                            fields = c("github_url",
                                                       "owner",
                                                       "repo"),
                                            verbose = verbose)
    #### Get dependent from GitHub ####
    if(isTRUE(reverse)){
        dt <- echogithub::github_dependents(owner = info$owner,
                                            repo = info$repo,
                                            verbose = verbose)
    } else {
        dt <- echogithub::github_dependencies(owner = info$owner,
                                              repo = info$repo,
                                              verbose = verbose)
    }
    dt <- add_owner_repo(dt = dt,
                         use_basename = use_basename,
                         sep = sep)
    #### Exclude repos ####
    if(!is.null(exclude)) dt <- dt[!repo %in% exclude,]
    #### Check deps ####
    if(is.null(dt)){
        stopper("Could not find any dependents on GitHub.")
    }
    #### Create graph ####
    from_col <- if(isTRUE(use_basename)) "repo" else "owner_repo"
    size_col <- if(isTRUE(reverse)) "stargazers_count" else NULL
    g <- igraph::graph_from_data_frame(
        d = dt[,c(from_col,"target",size_col), with=FALSE])
    #### Get repo metadata ####
    meta <- lapply(seq_len(nrow(dt)), function(i){
        cbind(
            target=dt[i,]$target,
            workflow=dt[i,]$workflow,
            action=dt[i,]$action,
            subaction=dt[i,]$subaction,
            owner_repo=dt[i,]$owner_repo,
            echogithub::github_metadata(owner = dt[i,]$owner,
                                        repo = dt[i,]$repo,
                                        add_traffic = TRUE,
                                        verbose = verbose)
        )
    }) |> data.table::rbindlist(fill = TRUE)
    #### Postprocessing ####
    if(nrow(meta)==0){
        messager("WARNING: No metadata found.",v=verbose)
    } else {
        pm_out <- prep_metadata(meta=meta,
                                pkg=pkg,
                                use_basename=use_basename,
                                sep=sep)
        pkg_name2 <- pm_out$pkg_name2
        meta <- pm_out$meta
        g <- add_meta_github(g = g,
                             pkg = pkg_name2,
                             meta = meta,
                             node_size = node_size)
    }
    #### Subset graph ####
    g2 <- subset_graph(g = g,
                       exclude = exclude,
                       verbose = verbose)
    #### Return ####
    return(list(pkg=pkg_name2,
                graph=g,
                subgraph=g2,
                report=dt,
                metadata=meta))
}
