test_that("dep_graph works", {

    run_tests <- function(res, pkg){
        testthat::expect_true(methods::is(res$plot,"visNetwork"))
        testthat::expect_true(methods::is(res$metadata,"data.frame"))
        testthat::expect_true(methods::is(res$graph,"igraph"))
        testthat::expect_true(methods::is(res$subgraph,"igraph"))
        testthat::expect_true(methods::is(res$report,"PackageReport") |
                              methods::is(res$report,"data.table"))
        testthat::expect_equal(res$pkg,pkg)
    }

    #### echoverse ####
    res <- echodeps::dep_graph()
    run_tests(res = res, pkg = "echolocatoR")
    # testthat::expect_true(file.exists(res$save_path)) ## Doesnt exist?

    #### dplyr ####
    res2 <- echodeps::dep_graph(pkg = "dplyr",
                                shape = "hexagon",
                                layout = function(graph, pkg){
                                    visNetwork::visIgraphLayout(
                                        graph = graph,
                                        layout = "layout_nicely"
                                    )
                                })
    run_tests(res = res2, pkg = "dplyr")


    #### Using "github" ####
    res3 <- echodeps::dep_graph(pkg = "rworkflows",
                                method = "github",
                                shape = "hexagon")
    run_tests(res = res3, pkg = "rworkflows")


    #### REVERSE dependencies ####
    res4 <- echodeps::dep_graph(pkg = "rworkflows",
                                method = "github",
                                shape = "hexagon",
                                exclude = c("neurogenomics_r_workflows",
                                            "neurogenomics_rworkflows"),
                                node_size = "indirect_total_downloads",
                                layout = NULL,
                                reverse = TRUE)
    vat = igraph::vertex.attributes(res4$graph)

    res4$report
    # sum($stargazers_count, na.rm = T)
    run_tests(res = res4, pkg = "rworkflows")
})
