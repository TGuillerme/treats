test_that("dispRitreats works", {

    ## Simulate the tree and traits
    sim_data <- treats(traits     = make.traits(n = 10),
                        bd.params  = make.bd.params(speciation = 1),
                        stop.rule  = list(max.taxa = 50),
                        replicates = 5)

    ## Generates the data correctly
    test <- dispRitreats(sim_data)
    expect_is(test, c("dispRity", "treats"))
    expect_is(test$data[[1]], "matrix")
    expect_is(test$tree[[1]], "phylo")
    expect_is(test$tree, "multiPhylo")
    expect_equal(length(test$data), 5)
})