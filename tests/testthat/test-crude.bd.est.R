test_that("crude.bd.est works", {
    ## Sanitizing
    error <- capture_error(crude.bd.est("tree"))
    expect_equal(error[[1]], "tree must be of class phylo.")

    ## Just living species
    set.seed(1)
    liv <- rcoal(10)
    expect_equal(round(crude.bd.est(liv, method = "count")$speciation(), 4), round(2.197225, 4))
    expect_equal(round(crude.bd.est(liv, method = "count")$extinction(), 4), round(0, 4))
    liv$root.time <- 10
    expect_equal(round(crude.bd.est(liv, method = "count")$speciation(), 3), round(1.276417, 3))
    expect_equal(round(crude.bd.est(liv, method = "count")$extinction(), 4), round(0, 4))

    ## Also fossils
    set.seed(1)
    tree <- rtree(20)
    expect_equal(round(crude.bd.est(tree, method = "count")$speciation(), 4), round(2.944439, 4))
    expect_equal(round(crude.bd.est(tree, method = "count")$extinction(), 4), round(2.944439, 4))


    ## Just living species
    set.seed(1)
    liv <- rcoal(10)
    expect_equal(round(crude.bd.est(liv, method = "estimate")$speciation(), 4), round(1.468996, 4))
    expect_equal(round(crude.bd.est(liv, method = "estimate")$extinction(), 4), round(0.6229002, 4))
    liv$root.time <- 10
    expect_equal(round(crude.bd.est(liv, method = "estimate")$speciation(), 4), round(1.468996, 4))
    expect_equal(round(crude.bd.est(liv, method = "estimate")$extinction(), 4), round(1.272554, 4))

    ## Also fossils
    set.seed(1)
    tree <- rtree(20)
    expect_equal(round(crude.bd.est(tree, method = "estimate")$speciation(), 4), round(0.95573, 4))
    expect_equal(round(crude.bd.est(tree, method = "estimate")$extinction(), 4), round(-0.1382479, 4))
})