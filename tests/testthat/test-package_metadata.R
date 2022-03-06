test_that("package_metadata works", {

    #### echoverse ####
    meta <- echodeps::package_metadata()
    testthat::expect_gte(nrow(meta), 12)
    testthat::expect_equal(
        sort(meta$Package),
        sort(echodeps:::echoverse_modules())
    )
    testthat::expect_true(
        data.table::key(meta)=="Package"
    )
    testthat::expect_equal(
        meta["echolocatoR",]$URL[[1]],
        "https://github.com/RajLabMSSM/echolocatoR"
    )

    #### other ####
    pkgs <- c("dplyr","data.table","utils")
    meta2 <- echodeps::package_metadata(pkgs = pkgs)
    testthat::expect_equal(sort(meta2$Package),sort(pkgs))
    testthat::expect_equal(
        meta2["utils",]$Title[[1]],
        "The R Utils Package"
    )
})
