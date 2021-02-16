## Test
test_that("sample.fun works", {

    ## Right output
    expect_is(sample.fun(), "numeric")
    expect_equal(length(sample.fun()), 1)
    ## Right behaviour
    expect_equal(sample.fun(runif, max = 42, min = 42), 42)

    ## Joint sampling
    expect_is(sample.fun(joint = TRUE), "numeric")
    test <- sample.fun(runif, joint = TRUE)
    expect_equal(names(test), c("speciation", "extinction"))
    expect_gt(test[1], test[2])
    ## Right error
    error <- capture_error(sample.fun(runif, max = 42, min = 42, joint = TRUE))
    expect_equal(error[[1]], "Impossible to sample a joint value with the speciation > extinction.")
})
