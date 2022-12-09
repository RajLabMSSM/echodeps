dep_graph_create_pkgnet <- function(pkg_name,
                                    deps,
                                    node_size=NULL,
                                    verbose=TRUE){
    requireNamespace("pkgnet")
    requireNamespace("igraph")

    messager("Generating `pkgnet` package report.",v=verbose)
    report <- pkgnet::CreatePackageReport(pkg_name = pkg_name)
    g <- report$DependencyReporter$pkg_graph$igraph
    #### Subset deps ####
    messager("Constructing dependency subgraph.",v=verbose)
    deps <- subset_deps(pkg_name = pkg_name,
                        deps = deps,
                        report = report)
    ### Gather metadata ####
    # report$DependencyReporter$nodes
    meta <- package_metadata(pkgs = c(pkg_name,deps))
    all_pkgs <- unlist(meta$Package)
    #### Subset graph ####
    g2 <- subset_graph(g = g,
                       include = all_pkgs,
                       verbose = verbose)
    #### Add graph metadata ####
    g2 <- dep_graph_add_meta_pkgnet(g2 = g2,
                                    pkg_name = pkg_name,
                                    meta = meta,
                                    node_size = node_size)
    #### Return ####
    return(list(pkg_name=pkg_name,
                graph=g,
                subgraph=g2,
                report=report,
                metadata=meta))
}
