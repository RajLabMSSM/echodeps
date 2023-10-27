#' Create a reverse dependency graph
#'
#' Create a reverse dependency graph.
#' @source \href{https://stackoverflow.com/q/38428945}{igraph::union issue}
#' @param method Seed method for extracting first-order reverse dependencies
#'  of \code{pkg}.
#' @param degrees The number of degrees out from the main \code{pkg} node to
#' extend the dependency graph.
#' @inheritParams dep_graph
#' @inheritParams subset_graph
#' @inheritParams devtools::revdep
#' @returns Named list.
#'
#' @export
#' @examples
#' dgc_out <- revdep_graph_create(pkg = "rworkflows")
revdep_graph_create <- function(pkg,
                                exclude=NULL,
                                method_seed=c("github","devtools"),
                                method = c("devtools","github"),
                                recursive=FALSE,
                                node_size=NULL,
                                add_metadata=TRUE,
                                degrees = 2,
                                verbose=TRUE){

    # devoptera::args2vars(dep_graph, reassign = TRUE)
    # devoptera::args2vars(revdep_graph_create, reassign = TRUE)
    first_degree <- NULL;
    method_seed <- tolower(method_seed)[1]
    method <- tolower(method)[1]
    #### Round 1 ####
    messager("Finding degree_1 dependents with 1 seed package.",
             parallel=TRUE, v=verbose)
    if(method_seed=="github"){
        seed_deps <- dep_graph_create_github(refs=pkg,
                                             exclude=exclude,
                                             node_size=node_size,
                                             add_metadata = FALSE,
                                             reverse = TRUE,
                                             verbose=verbose)
    } else if(method_seed=="devtools"){
        seed_deps <- revdep_graph_create_devtools(refs=pkg,
                                                  pkg_target=pkg,
                                                  exclude=exclude,
                                                  recursive=recursive,
                                                  add_metadata = FALSE,
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
    #### Exit early by skipping round 2 ####
    if(is.null(method) || degrees==1) return(seed_deps)
    #### Round 2-Inf ####
    extra_rounds <- stats::setNames(seq_len(degrees-1),
                                    paste0("degree_",seq_len(degrees-1)+1))
    seed_packages <- igraph::V(seed_deps$graph)$ref
    degrees_out <- lapply(extra_rounds,
                          function(i){
        messager(paste(
            "--- Finding",names(extra_rounds)[[i]],"dependents",
            "with",length(unique(seed_packages)),"seed packages.",
            "---"
        ),v=verbose, parallel = TRUE)
        if(method=="github"){
            #### !!!!!!!!!!!!!!!!!!!!!!!!!!!! ####
            #### !!!! UNDER CONSTRUCTION !!!! ####
            #### !!!!!!!!!!!!!!!!!!!!!!!!!!!! ####
            dgc_out <- dep_graph_create_github(refs=seed_packages,
                                               exclude=exclude,
                                               node_size=node_size,
                                               reverse = TRUE,
                                               add_traffic = FALSE,
                                               verbose=verbose)
        } else if(method=="devtools"){
            dgc_out <- revdep_graph_create_devtools(refs=seed_packages,
                                                    pkg_target = pkg,
                                                    exclude=exclude,
                                                    recursive=recursive,
                                                    add_metadata = FALSE,
                                                    verbose=verbose)
        } else {
            stopper("method must be one of:",
                    paste("\n -",
                          shQuote(eval(formals(revdep_graph_create)$method)),
                          collapse = ""))
        }
        #### Packages from previous round become the new seed packages ####
        seed_packages <- igraph::V(dgc_out$graph)$ref
        #### Return #####
        return(dgc_out)
    })
    #### Merge graphs ####
    graph_list <- c(list(degree_1=seed_deps$subgraph),
                    lapply(degrees_out,function(x){x$subgraph}))

    g <- merge_graphs(graph_list = graph_list,
                      by = c("ref","name"),
                      node_size = node_size)
    #### Add metadata ####
    if(isTRUE(add_metadata)){
        meta <- prep_metadata(g = g,
                              verbose = verbose)
        g <- add_meta_github(g = g,
                             pkg = pkg,
                             meta = meta,
                             node_size = node_size)
    }  else{
        meta <- data.table::as.data.table(g)
    }
    #### Add info about dependency degree ####
    meta[,first_degree:=basename(ref) %in% basename(
        igraph::V(seed_deps$graph)$ref)]
    #### Create report summary ####
    report <- report_summary(meta = meta,
                             verbose = verbose)
    #### Return ####
    return(list(pkg = pkg,
                graph = g,
                graph_list = graph_list,
                report = report))
}
