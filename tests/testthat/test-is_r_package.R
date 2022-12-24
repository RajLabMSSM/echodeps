test_that("is_r_package works", {

    is_pkg <- is_r_package(pkg="stats")
    testthat::expect_true(is_pkg)

    is_pkg <- is_r_package(pkg="echodeps")
    testthat::expect_true(is_pkg)

    testthat::expect_error(
        is_r_package(pkg="typooo")
    )

    testthat::expect_error(
        is_r_package(pkg="typooo",
                     owner = "typo_man")
    )
})
