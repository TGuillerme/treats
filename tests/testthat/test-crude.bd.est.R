test_that("crude.bd.est works", {
    ## Sanitizing
    error <- capture_error(crude.bd.est("tree"))
    expect_equal(error[[1]], "tree must be of class phylo.")

    ## Just living species
    set.seed(1)
    liv <- rcoal(10)
    expect_equal(crude.bd.est(liv)$speciation(), 9)
    expect_equal(crude.bd.est(liv)$extinction(), 0)
    liv$root.time <- 10
    expect_equal(round(crude.bd.est(liv)$speciation(), 4), round(5.229518, 4))
    expect_equal(crude.bd.est(liv)$extinction(), 0)

    ## Also fossils
    set.seed(1)
    tree <- rtree(20)
    expect_equal(crude.bd.est(tree)$speciation(), 19)
    expect_equal(crude.bd.est(tree)$extinction(), 19)
})