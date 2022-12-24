test_that("dep_graph_plot works", {

    #### Create graph ####
    dgc_out <- dep_graph_create(pkg = "rworkflows",
                                method = "github",
                                node_size = "clones_uniques")
    #### Subset graph ####
    g <- dgc_out$subgraph
    exclude <- grep("actions-marketplace-validations",
                    names(igraph::V(g)),value = TRUE)
    g2 <- subset_graph(g=g, exclude=exclude)
    #### Plot graph ####
    vis <- dep_graph_plot(g = g2,
                          shape = "hexagon",
                          pkg = dgc_out$pkg)
    testthat::expect_true(methods::is(vis,"visNetwork"))
})
