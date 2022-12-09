test_that("dep_graph works", {

    run_tests <- function(res, pkg_name){
        testthat::expect_true(methods::is(res$plot,"visNetwork"))
        testthat::expect_true(methods::is(res$metadata,"data.frame"))
        testthat::expect_true(methods::is(res$graph,"igraph"))
        testthat::expect_true(methods::is(res$subgraph,"igraph"))
        testthat::expect_true(methods::is(res$report,"PackageReport") |
                              methods::is(res$report,"data.table"))
        testthat::expect_equal(res$pkg_name,pkg_name)
    }

    #### echoverse ####
    res <- echodeps::dep_graph()
    run_tests(res = res, pkg_name = "echolocatoR")
    # testthat::expect_true(file.exists(res$save_path)) ## Doesnt exist?

    #### dplyr ####
    res2 <- echodeps::dep_graph(pkg_name = "dplyr",
                                shape = "hexagon",
                                layout = function(graph, pkg_name){
                                    visNetwork::visIgraphLayout(
                                        graph = graph,
                                        layout = "layout_nicely"
                                    )
                                })
    run_tests(res = res2, pkg_name = "dplyr")


    #### Using "github" ####
    res3 <- echodeps::dep_graph(pkg_name = "rworkflows",
                                method = "github",
                                shape = "hexagon")
    run_tests(res = res3, pkg_name = "rworkflows")
})
