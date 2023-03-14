#' Create a dependency graph: GitHub
#'
#' Create a dependency graph: GitHub.
#' @inheritParams dep_graph
#' @inheritParams revdep_graph_create
#' @inheritParams echogithub::github_metadata
#' @inheritParams echogithub::description_extract
#' @importFrom data.table := rbindlist
#' @importFrom echogithub github_metadata description_extract add_owner_repo
#' @importFrom echogithub github_dependents github_dependencies
dep_graph_create_github <- function(refs,
                                    exclude=NULL,
                                    add_traffic=TRUE,
                                    node_size=NULL,
                                    reverse=FALSE,
                                    add_metadata=TRUE,
                                    verbose=TRUE){
    requireNamespace("igraph")
    target <- repo <- ref <-  NULL;

    messager("Generating `github` package report.",v=verbose)
    #### Get repo owner/name ####
    info <- echogithub:::description_extract(refs = refs,
                                             fields = c("github_url",
                                                        "owner",
                                                        "repo"),
                                             as_datatable = TRUE,
                                             verbose = verbose)
    #### Get dependent from GitHub ####
    dt <- lapply(seq_len(nrow(info)),
                 function(i){
        if(isTRUE(reverse)){
            d <- echogithub::github_dependents(owner = info[i,]$owner,
                                               repo = info[i,]$repo,
                                               verbose = verbose)
        } else {
            d <- echogithub::github_dependencies(owner = info[i,]$owner,
                                                 repo = info[i,]$repo,
                                                 verbose = verbose)
        }
        return(d)
    }) |> data.table::rbindlist(fill = TRUE)
    dt[,target_ref:=target]
    dt <- echogithub::add_owner_repo(dt = dt,
                                     ref_cols=c("subaction","ref",
                                                "owner_repo","repo",
                                                "package","Package")
                                     )
    #### Exclude repos ####
    if(!is.null(exclude)) dt <- dt[!repo %in% exclude,]
    #### Check deps ####
    if(is.null(dt)){
        stopper("Could not find any dependents on GitHub.")
    }
    #### Create graph ####
    from_col <- "ref"
    size_col <- if(isTRUE(reverse)) "stargazers_count" else NULL
    dt <- data.table::setcolorder(dt,c(from_col,"target",size_col))
    nodes <- rbind(
        dt[,name:=ref][,c("name")],
        dt[,name:=target_ref][,c("name")]
    ) |> unique()
    nodes[,ref:=name]
    g <- tidygraph::tbl_graph(
        nodes = nodes,
        edges =  dt[,c(from_col,"target_ref",size_col), with=FALSE])
#
#     #### Get repo metadata ####
#     if(isTRUE(add_metadata)){
#         meta <- lapply(seq_len(nrow(dt)), function(i){
#             cbind(
#                 target=dt[i,]$target,
#                 workflow=dt[i,]$workflow,
#                 action=dt[i,]$action,
#                 subaction=dt[i,]$subaction,
#                 owner_repo=dt[i,]$owner_repo,
#                 echogithub::github_metadata(owner = dt[i,]$owner,
#                                             repo = dt[i,]$repo,
#                                             add_traffic = TRUE,
#                                             verbose = verbose)
#             )
#         }) |> data.table::rbindlist(fill = TRUE)
#         #### Postprocessing ####
#         if(nrow(meta)==0){
#             messager("WARNING: No metadata found.",v=verbose)
#         } else {
#             messager("Metadata for",nrow(meta),"package repositories collected.",
#                      v=verbose)
#             g <- add_meta_github(g = g,
#                                  pkg = pkg,
#                                  meta = meta,
#                                  node_size = node_size)
#         }
#     } else {
#         meta <- NULL
#     }

    #### Subset graph ####
    g2 <- subset_graph(g = g,
                       exclude = exclude,
                       verbose = verbose)
    #### Return ####
    return(list(pkg=refs,
                graph=g,
                subgraph=g2,
                report=dt))
}
