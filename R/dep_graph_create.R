dep_graph_create <- function(pkg_name,
                             deps,
                             verbose = TRUE){

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
    all_pkgs <- all_pkgs[all_pkgs %in% names(igraph::V(g))]
    g2 <- igraph::induced_subgraph(
        graph =  g,
        vids = igraph::V(g)[all_pkgs]
    )
    #### Add graph metadata ####
    g2 <- dep_graph_add_meta(g2 = g2,
                             pkg_name = pkg_name,
                             meta = meta)
    return(list(graph=g,
                subgraph=g2,
                report=report,
                metadata=meta))
}
