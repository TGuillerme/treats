## Testing utilities

test_that("parent.traits works", {
    ## Some madeup lineage object
    lineage <- list(parents = c(0, 1, 1, 2, 3, 5, 5, 4, 6, 7),
                    livings = c(8, 9, 10),
                    drawn   = 3,
                    current = 5,
                    n       = 3,
                    split   = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))

    ## Some madeup trait values
    trait.values <- matrix(c(0,1,3,5,4,6,7), ncol = 1, dimnames = list(c(1,2,3,5,4,6,7)))

    ## Find the parent value of the current lineage
    expect_equal(parent.traits(trait.values, lineage), matrix(3, 1, 1, dimnames = list(c(3))))
    expect_equal(parent.traits(trait.values, lineage, current = FALSE), matrix(c(4,6,7), ncol = 1, dimnames = list(c(4,6,7))))
})

test_that("transition matrix works", {
    error <- capture_error(transition.matrix(type = "eR", states = 2, rates = runif, self = TRUE))
    expect_equal(error[[1]], "type must be one of the followings: equal rates, symmetric, all rates different, stepwise.")
    error <- capture_error(transition.matrix(type = "ER", states = "2", rates = runif, self = TRUE))
    expect_equal(error[[1]], "states must be of class numeric or integer.")
    error <- capture_error(transition.matrix(type = "ER", states = 2, rates = "runif", self = TRUE))
    expect_equal(error[[1]], "rates must be of class function or numeric or integer.")
    error <- capture_error(transition.matrix(type = "ER", states = 2, rates = runif, self = 1))
    expect_equal(error[[1]], "self must be of class logical.")

    set.seed(1)
    test <- transition.matrix(type = "ER", states = 2, rates = runif, self = TRUE)
    expect_is(test, "matrix")
    expect_equal(dim(test), c(2, 2))
    expect_equal(round(unique(c(test)), 4), round(0.2655087, 4))
    expect_equal(diag(transition.matrix(type = "ER", states = 2, rates = runif, self = FALSE)), c(0, 0))
    ## ARD
    test <- transition.matrix(type = "ARD", states = 10, rates = rnorm, self = TRUE)
    expect_true(all(c(test) > 0))
    expect_equal(dim(test), c(10, 10))
    expect_equal(length(unique(c(test))), 10^2)
    ## SYM
    test <- transition.matrix(type = "SYM", states = 3, rates = rnorm, self = TRUE)
    expect_equal(dim(test), c(3, 3))
    expect_equal(length(unique(c(test))), 4)
    ## Dollo
    test <- transition.matrix(type = "stepwise", states = 5, rates = 1)
    expect_equal(test[1,], c(1,1,0,0,0))
    expect_equal(test[2,], c(1,1,1,0,0))
    expect_equal(test[3,], c(0,1,1,1,0))
    expect_equal(test[4,], c(0,0,1,1,1))
    expect_equal(test[5,], c(0,0,0,1,1))
})