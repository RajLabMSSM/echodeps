dep_graph_create_pkgnet <- function(pkg,
                                    exclude=NULL,
                                    node_size=NULL,
                                    verbose=TRUE){
    requireNamespace("pkgnet")
    requireNamespace("igraph")

    messager("Generating `pkgnet` package report.",v=verbose)
    if(pkg=="echoverse"){
        pkg_name <- "echolocatoR"
        include <- echoverse_modules()
        dep_types = c("Imports",
                      "Depends",
                      "Suggests")
    }  else {
        pkg_name <- pkg
        include <- NULL
        dep_types <- c("Imports",
                       "Depends")
    }
    pkg_reporters <- list(
        pkgnet::SummaryReporter$new(),
        pkgnet::DependencyReporter$new(dep_types = dep_types),
        pkgnet::FunctionReporter$new()
    )
    report <- pkgnet::CreatePackageReport(pkg_name = pkg_name,
                                          pkg_reporters = pkg_reporters)
    g <- report$DependencyReporter$pkg_graph$igraph
    #### Subset deps ####
    messager("Constructing dependency subgraph.",v=verbose)
    deps <- subset_deps(pkg = pkg,
                        include = include,
                        exclude = exclude,
                        report = report)
    ### Gather metadata ####
    # report$DependencyReporter$nodes
    meta <- package_metadata(pkgs = unique(c(pkg_name,deps)),
                             verbose = verbose)
    #### Add graph metadata ####
    g <- add_meta_pkgnet(g = g,
                         pkg = pkg,
                         meta = meta,
                         node_size = node_size)
    #### Subset graph ####
    g2 <- subset_graph(g = g,
                       include = deps,
                       verbose = verbose)
    #### Return ####
    return(list(pkg=pkg_name,
                graph=g,
                subgraph=g2,
                report=report,
                metadata=meta))
}
