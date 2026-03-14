test_that("package_metadata works", {

    #### echoverse ####
    meta <- echodeps::package_metadata()
    testthat::expect_gte(nrow(meta), 12)
    pkg_col <- if("Package" %in% colnames(meta)) "Package" else "package"
    testthat::expect_true(pkg_col %in% colnames(meta))
    ## All returned packages should be echoverse modules
    ## (some modules may be missing if their DESCRIPTION can't be fetched)
    testthat::expect_true(
        all(meta[[pkg_col]] %in% echodeps:::echoverse_modules())
    )

    #### other ####
    pkgs <- c("dplyr","data.table","utils")
    meta2 <- echodeps::package_metadata(pkgs = pkgs)
    pkg_col2 <- if("Package" %in% colnames(meta2)) "Package" else "package"
    testthat::expect_equal(sort(meta2[[pkg_col2]]),sort(pkgs))
})
