test_that("source_all works", {
    
  ## NOTE: By default, this unit test will only work when being 
  ## run through R CMD checks.
  ## To manually run these tests, set `manual <- TRUE`.
    if(!is_32bit()){
        manual <- FALSE
        source_all(path = if(manual) "./R" else "../../R") 
        testthat::expect_true(exists("messager"))
        testthat::expect_true("base" %in% .packages())
    } else {
        testthat::expect_null(NULL)
    } 
})
