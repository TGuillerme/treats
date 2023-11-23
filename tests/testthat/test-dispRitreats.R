test_that("dispRitreats works", {

    ## Simulate the tree and traits
    set.seed(1)
    sim_data <- treats(traits     = make.traits(n = 10),
                       bd.params  = make.bd.params(speciation = 1),
                       stop.rule  = list(max.taxa = 50),
                       replicates = 5)

    ## ONLY ONE SIMULATION:
    ## Simple test: just metric
    test <- dispRitreats(sim_data[[1]], metric = c(mean, centroids))
    expect_is(test, "dispRity")
    expect_equal(dim(summary(test)), c(1,3))
    expect_equal(round(summary(test)[1,3], 2), 4.56)

    ## Metric + optional args
    test <- dispRitreats(sim_data[[1]], metric = c(mean, centroids), centroid = 100)
    expect_is(test, "dispRity")
    expect_equal(dim(summary(test)), c(1,3))
    expect_equal(round(summary(test)[1,3], 2), 316.3)

    ## Metric + bootstrap + optional args
    test <- dispRitreats(sim_data[[1]], metric = c(mean, centroids), centroid = 100, bootstraps = 100)
    expect_is(test, "dispRity")
    expect_equal(dim(summary(test)), c(1,8))
    expect_equal(round(summary(test)[1,3], 2), 316.3)

    ## Metric + chrono.subsets
    test <- dispRitreats(sim_data[[1]], metric = c(mean, centroids), time = 5, method = "continuous", model = "acctran")
    expect_is(test, "dispRity")
    expect_equal(dim(summary(test)), c(5,3))
    expect_equal(round(summary(test)[1,3], 2)[[1]], 2.82)

    ## Metric + custom.subsets
    test <- dispRitreats(sim_data[[1]], metric = c(mean, centroids), group = list("A" = c("t1", "t2", "t3"), "B" = c("t1", "t3", "t4")))
    expect_is(test, "dispRity")
    expect_equal(dim(summary(test)), c(2,3))
    expect_equal(round(summary(test)[1,3], 2)[[1]], 3.71)

    ## Verbose
    tust <- capture_messages(test <- dispRitreats(sim_data, metric = c(mean, centroids), centroid = 100, bootstraps = 10, time = 5, method = "continuous", model = "acctran", verbose = TRUE))
    expect_equal(paste(tust, collapse = ""), "Calculating disparity:.....Done.\n")  

    ## MULTIPLE SIMULATIONS:
    ## Simple test: just metric
    test <- dispRitreats(sim_data, metric = c(mean, centroids))
    expect_is(test, "dispRity")
    expect_equal(dim(summary(test)), c(1,7))
    expect_equal(round(summary(test)[1,3], 2)[[1]], 4.56)

    ## Metric + optional args
    test <- dispRitreats(sim_data, metric = c(mean, centroids), centroid = 100)
    expect_is(test, "dispRity")
    expect_equal(dim(summary(test)), c(1,7))
    expect_equal(round(summary(test)[1,3], 2)[[1]], 316.3)

    ## Metric + bootstrap + optional args
    test <- dispRitreats(sim_data, metric = c(mean, centroids), centroid = 100, bootstraps = 100)
    expect_is(test, "dispRity")
    expect_equal(dim(summary(test)), c(1,8))
    expect_equal(round(summary(test)[1,3], 2)[[1]], 316.3)

    ## Metric + chrono.subsets
    # all_args <- list(metric = c(mean, centroids), time = 5, method = "continuous", model = "proximity", inc.nodes = TRUE)
    test <- dispRitreats(sim_data, metric = c(mean, centroids), time = 5, method = "continuous", model = "proximity", inc.nodes = TRUE)
    expect_is(test, "dispRity")
    expect_equal(dim(summary(test)), c(5,7))
    expect_equal(round(summary(test)[1,3], 2)[[1]], 0.36)
    expect_null(plot(test))

    ## Metric + custom.subsets
    test <- dispRitreats(sim_data, metric = c(mean, centroids), group = list("A" = c("t1", "t2", "t3"), "B" = c("t1", "t3", "t4")))
    expect_equal(dim(summary(test)), c(2,7))
    expect_equal(round(summary(test)[1,3], 2)[[1]], 4.86)
    expect_null(plot(test))


})

