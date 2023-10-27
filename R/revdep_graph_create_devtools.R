#' Create a reverse dependency graph: devtools
#'
#' Create a reverse dependency graph.
#' @param refs Package names.
#' @inheritParams dep_graph
#' @inheritParams revdep_graph_create
#' @inheritParams subset_graph
#' @inheritParams devtools::revdep
#' @returns igraph
#'
#' @keywords internal
revdep_graph_create_devtools <- function(refs,
                                         pkg_target=NULL,
                                         exclude=NULL,
                                         recursive=FALSE,
                                         add_metadata=TRUE,
                                         node_size=NULL,
                                         verbose=TRUE){
    requireNamespace("igraph")
    owner <- repo <- owner_repo <- target_ref<- package <- NULL;

    messager("Generating `devtools` package report.",v=verbose)
    revdeps <- lapply(stats::setNames(basename(refs),
                                      refs),
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
                              package=tools::dependsOnPkgs(
                                  pkgs = p,
                                  recursive = recursive)
                          )
                  )
               )
               d[!duplicated(package),]
           }) |> data.table::rbindlist(fill = TRUE,
                                       use.names = TRUE,
                                       idcol = "target_ref")
    #### Create graph ####
    ref_key <- refs[basename(refs) %in% revdeps$package]
    extra_pkgs <- revdeps$package[!revdeps$package %in% basename(refs)]
    dl <- rworkflows::get_description(refs = stats::setNames(unique(extra_pkgs),
                                                            unique(extra_pkgs)),
                                      use_repos = TRUE)
    revdeps2 <- echogithub::description_extract(
        desc_file  = dl,
        fields = c("Package","owner","repo","github_url")
        ) |>
        echogithub::add_owner_repo() |>
        data.table::setkey("package")
    ref_key <- c(stats::setNames(ref_key,basename(ref_key)),
                 stats::setNames(revdeps2$ref, revdeps2$Package))
    revdeps[,ref:=ref_key[package]]
    revdeps$ref <- stringr::str_split(revdeps$ref," ", simplify = TRUE)[,1]
    # revdeps <- echogithub::add_owner_repo(dt = revdeps)
    revdeps[,target_repo:=basename(target_ref)]
    #### Exclude repos ####
    if(!is.null(exclude)) revdeps <- revdeps[!package %in% exclude,]
    #### Create graph ####
    from_col <- "ref"
    revdeps <- data.table::setcolorder(revdeps,c(from_col,"target_ref"))
    #### Remove NAs #####
    revdeps <- revdeps[!is.na(ref) & !is.na(target_ref)]
    g <- tidygraph::as_tbl_graph(revdeps)
    igraph::V(g)$ref <- igraph::V(g)$name
    #### Get repo metadata ####
    # if(isTRUE(add_metadata)){
    #     meta <- lapply(stats::setNames(refs, refs), function(p){
    #         messager("Getting description:",p)
    #         tryCatch({
    #             echogithub::description_extract(ref = p,
    #                                             fields = c("owner","repo"),
    #                                             as_datatable = TRUE,
    #                                             verbose = FALSE)
    #         }, error=function(e){NULL})
    #     }) |> data.table::rbindlist(use.names = TRUE, idcol = "input")
    #     meta <- echogithub::add_owner_repo(dt = meta)
    #     if(nrow(meta)==0){
    #         messager("WARNING: No metadata found.",v=verbose)
    #         meta <- revdeps
    #     } else {
    #         g <- add_meta_github(g = g,
    #                              pkg = pkg_target,
    #                              meta = meta,
    #                              node_size = node_size)
    #     }
    # } else {
    #     meta <- revdeps
    # }
    #### subset graph ####
    g2 <- subset_graph(g = g,
                       exclude = exclude,
                       verbose = verbose)
    #### Return ####
    return(list(refs=refs,
                pkg_target=pkg_target,
                graph=g,
                subgraph=g2))
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
# rdeps5 <- tools::package_dependencies(packages = refs,
#                                       recursive = FALSE,
#                                       reverse = TRUE)
