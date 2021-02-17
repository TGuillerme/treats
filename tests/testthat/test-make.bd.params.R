## Test
test_that("make.bd.params works", {

    ## Test default
    test <- make.bd.params()
    expect_equal(class(test), c("dads", "bd.params"))
    expect_equal(names(test), c("joint", "absolute", "speciation", "extinction", "call"))
    expect_equal(sample.from.bd.params(test), c("speciation" = 1, "extinction" = 0))
    out <- capture_output(print(test))
    expect_equal(out, c(" ---- dads birth-death parameters object ---- \nspeciation: 1.\nextinction: 0."))
    expect_null(plot(test))

    ## Test single values
    test <- make.bd.params(speciation = 10, extinction = c(1, 2), joint = TRUE)
    expect_equal(class(test), c("dads", "bd.params"))
    expect_equal(names(test), c("joint", "absolute", "speciation", "extinction", "call"))
    set.seed(1)
    expect_equal(sample.from.bd.params(test), c("speciation" = 10, "extinction" = 1))
    expect_equal(sample.from.bd.params(test), c("speciation" = 10, "extinction" = 2))
    out <- capture_output(print(test))
    expect_equal(out, c(" ---- dads birth-death parameters object ---- \njoint sampling for:\nspeciation: 10.\nextinction: 1, 2."))
    expect_null(plot(test))

    ## Test functions
    test <- make.bd.params(speciation = rnorm)
    expect_equal(class(test), c("dads", "bd.params"))
    expect_equal(names(test), c("joint", "absolute", "speciation", "extinction", "call"))
    set.seed(1)
    expect_equal(round(sample.from.bd.params(test), 4), c("speciation" = -0.6265, "extinction" = 0))
    expect_equal(round(sample.from.bd.params(test), 4), c("speciation" = 0.1836, "extinction" = 0))
    out <- capture_output(print(test))
    expect_equal(out, c(" ---- dads birth-death parameters object ---- \nspeciation: rnorm.\nextinction: 0."))
    expect_null(plot(test))

    ## Multiple functions, joint with extra args
    test <- make.bd.params(speciation = runif, extinction = rnorm, joint = TRUE, extinction.args = list(sd = 0.5), absolute = TRUE)
    expect_equal(class(test), c("dads", "bd.params"))
    expect_equal(names(test), c("joint", "absolute", "speciation", "extinction", "call"))
    set.seed(1)
    expect_equal(round(sample.from.bd.params(test), 4), c("speciation" = 0.2655, "extinction" = 0.1631))
    expect_equal(round(sample.from.bd.params(test), 4), c("speciation" = 0.9082, "extinction" = 0.4178))
    out <- capture_output(print(test))
    expect_equal(out, c(" ---- dads birth-death parameters object ---- \njoint sampling for:\nspeciation: runif.\nextinction: rnorm (with optional arguments).\n(using absolute values)"))
    expect_null(plot(test))

})
