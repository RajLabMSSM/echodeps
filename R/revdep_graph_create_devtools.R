#' Create a reverse dependency graph: devtools
#'
#' Create a reverse dependency graph.
#' @param pkgs Package names.
#' @param add_metadata Add metadata to the graph
#' using \link[echogithub]{github_metadata}.
#' @inheritParams dep_graph
#' @inheritParams revdep_graph_create
#' @inheritParams subset_graph
#' @inheritParams devtools::revdep
#' @returns igraph
#'
#' @keywords internal
#' @importFrom echogithub r_repos_data
revdep_graph_create_devtools <- function(pkgs,
                                         pkg_target=NULL,
                                         exclude=NULL,
                                         recursive=FALSE,
                                         add_metadata=TRUE,
                                         use_basename=TRUE,
                                         sep="/\n",
                                         node_size=NULL,
                                         verbose=TRUE){
    requireNamespace("igraph")
    owner <- repo <- owner_repo <- NULL;

    messager("Generating `devtools` package report.",v=verbose)
    revdeps <- lapply(stats::setNames(pkgs,pkgs),
           function(p){
               messager("Finding reverse dependencies for:",p,v=verbose)
               d <- data.table::rbindlist(fill = TRUE,
                                          use.names = TRUE,
                                          idcol = "source",
                  list(
                      "CRAN"=
                          data.table::data.table(
                              package=suppressMessages(
                                  devtools::revdep(pkg = p,
                                                   recursive = recursive,
                                                   bioconductor = FALSE)
                              )
                          ),
                      "Bioc"=
                          data.table::data.table(
                              package=suppressMessages(
                                  devtools::revdep(pkg = p,
                                                   recursive = recursive,
                                                   bioconductor = TRUE)
                              )
                          ),
                      "local"=
                          data.table::data.table(
                              package=tools::dependsOnPkgs(pkgs = p,
                                                          recursive = recursive)
                          )
                  )
               )
               d[!duplicated(d$package),]
           }) |> data.table::rbindlist(fill = TRUE,
                                       use.names = TRUE,
                                       idcol = "target")
    #### Create graph ####
    g <- igraph::graph_from_data_frame(d = revdeps[,c("package","target")])
    #### Get repo metadata ####
    meta <- echogithub::r_repos_data(include = pkgs,
                                     # add_downloads = TRUE,
                                     # add_descriptions = TRUE,
                                     # add_github = TRUE,
                                     # cast = TRUE,
                                     verbose = verbose)
    if(nrow(meta)==0){
        messager("WARNING: No metadata found.",v=verbose)
        meta <- revdeps
    } else {
        pm_out <- prep_metadata(meta = meta,
                                pkg = pkg_target,
                                use_basename = use_basename,
                                sep = sep)
        pkg_target <- pm_out$pkg_name2
        meta <- pm_out$meta
        g <- add_meta_github(g = g,
                             pkg = pkg_target,
                             meta = meta,
                             node_size = node_size)
    }
    #### subset graph ####
    g2 <- subset_graph(g = g,
                       exclude = exclude,
                       verbose = verbose)
    #### Return ####
    return(list(pkgs=pkgs,
                pkg_target=pkg_target,
                graph=g,
                subgraph=g2,
                metadata=meta))
}

#### Other methods for getting reverse dependencies ####
# #### Works for non-installed Bioc/CRAN packages ####
# # rdeps1 <- revdepcheck::cran_revdeps(package = "ade4",
# #                                    bioc = TRUE)
# #### Works for non-installed Bioc/CRAN packages ####
# rdeps2 <- devtools::revdep(pkg = c("ade4","stats"),
#                            recursive = FALSE,
#                            bioconductor = TRUE)
#
# #### Only works for CRAN ####
# # rdeps3 <- crandep::get_dep_all_packages()
# # subset(rdeps3,reverse==TRUE)
#
# ### Only works for installed packages ####
# rdeps4 <- tools::dependsOnPkgs("ade4",
#                                recursive = FALSE)
# #### Only works for CRAN packages? ####
# rdeps5 <- tools::package_dependencies(packages = pkgs,
#                                       recursive = FALSE,
#                                       reverse = TRUE)
