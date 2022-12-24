#' Create a reverse dependency graph
#'
#' Create a reverse dependency graph.
#' @source \href{https://stackoverflow.com/q/38428945}{igraph::union issue}
#' @param method_seed Method to get the initial layer of
#'  reverse dependencies with.
#' @inheritParams dep_graph
#' @inheritParams subset_graph
#' @inheritParams devtools::revdep
#' @export
#' @examples
#' dgc_out <- revdep_graph_create(pkg = "rworkflows")
revdep_graph_create <- function(pkg,
                                exclude=NULL,
                                method_seed=c("github","devtools"),
                                method = c("devtools","github"),
                                use_basename=TRUE,
                                sep="/\n",
                                recursive=FALSE,
                                node_size=NULL,
                                verbose=TRUE){
    # templateR:::source_all()
    # templateR:::args2vars(revdep_graph_create)
    requireNamespace("igraph")

    method_seed <- tolower(method_seed)[1]
    method <- tolower(method)[1]
    #### Round 1 ####
    if(method_seed=="github"){
        seed_deps <- dep_graph_create_github(pkg=pkg,
                                             exclude=exclude,
                                             node_size=node_size,
                                             use_basename=use_basename,
                                             reverse = TRUE,
                                             sep=sep,
                                             verbose=verbose)
    } else if(method_seed=="devtools"){
        seed_deps <- revdep_graph_create_devtools(pkgs=pkg,
                                                  pkg_target=pkg,
                                                  exclude=exclude,
                                                  recursive=recursive,
                                                  use_basename=use_basename,
                                                  sep=sep,
                                                  verbose=verbose)
    } else {
        stopper("method must be one of:",
                paste("\n -",
                      shQuote(eval(formals(revdep_graph_create)$method)),
                      collapse = ""))
    }
    #### Check ####
    if(length(length(seed_deps$graph))==0){
        stopper("No reverse dependencies found.")
    }
    #### Exit early by skipping round 2####
    if(is.null(method)) return(seed_deps)

    #### Round 2 ####
    if(method=="github"){
        dgc_out <- dep_graph_create_github(pkg=seed_deps$metadata$repo,
                                           exclude=exclude,
                                           node_size=node_size,
                                           use_basename=use_basename,
                                           sep=sep,
                                           reverse = TRUE,
                                           verbose=verbose)
    } else if(method=="devtools"){
        dgc_out <- revdep_graph_create_devtools(pkgs=seed_deps$metadata$repo,
                                                pkg_target = pkg,
                                                exclude=exclude,
                                                recursive=recursive,
                                                use_basename=use_basename,
                                                sep=sep,
                                                verbose=verbose)
    } else {
        stopper("method must be one of:",
                paste("\n -",
                      shQuote(eval(formals(revdep_graph_create)$method)),
                      collapse = ""))
    }
    #### Merge graphs ####
    mg_out <- merge_graphs(res1 = seed_deps,
                           res2 = dgc_out,
                           node_size = node_size)
    #### Create report summary ####
    report <- report_summary(metadata = mg_out$metadata,
                             verbose = verbose)
    #### Return ####
    return(list(pkg = mg_out$pkg,
                graph = mg_out$graph,
                subgraph = seed_deps$subgraph,
                report = report,
                metadata = mg_out$metadata))
}
