context("dads")

## Test
test_that("dads works for simple birth-deaths", {

    ## Naked birth death (just the tree)

    ## Sanitizing
    error <- capture_error(dads(bd.params = "hehe", stop.rule = list(max.taxa = 10)))
    expect_equal(error[[1]], "bd.params must be of class list.")
    error <- capture_error(dads(bd.params = list(1, 2), stop.rule = list(max.taxa = 10)))
    expect_equal(error[[1]], "bd.params must be a named list.")
    
    error <- capture_error(dads(bd.params = list(speciation = 2), stop.rule = list(max.taxa = 10)))
    expect_equal(error[[1]], "bd.params$speciation must be a single numeric value in the (0, 1] interval.")
    error <- capture_error(dads(bd.params = list(speciation = 0), stop.rule = list(max.taxa = 10)))
    expect_equal(error[[1]], "bd.params$speciation must be a single numeric value in the (0, 1] interval.")
    error <- capture_error(dads(bd.params = list(speciation = "ha"), stop.rule = list(max.taxa = 10)))
    expect_equal(error[[1]], "bd.params$speciation must be a single numeric value in the (0, 1] interval.")

    error <- capture_error(dads(bd.params = list(extinction = -2), stop.rule = list(max.taxa = 10)))
    expect_equal(error[[1]], "bd.params$extinction must be a single numeric value in the [0, 1) interval.")
    error <- capture_error(dads(bd.params = list(extinction = 1), stop.rule = list(max.taxa = 10)))
    expect_equal(error[[1]], "bd.params$extinction must be a single numeric value in the [0, 1) interval.")
    error <- capture_error(dads(bd.params = list(extinction = "ha"), stop.rule = list(max.taxa = 10)))
    expect_equal(error[[1]], "bd.params$extinction must be a single numeric value in the [0, 1) interval.")


    error <- capture_error(dads())
    expect_equal(error[[1]], "You must provide at least one stopping rule. For example:\nstop.rule <- list(max.taxa   = 10,\n                  max.living = 10,\n                  max.time   = 10)")
    error <- capture_error(dads(stop.rule = "ha"))
    expect_equal(error[[1]], "stop.rule must be of class list.")
    error <- capture_error(dads(stop.rule = list(1,2)))
    expect_equal(error[[1]], "stop.rule must be a named list of stopping rules. For example:\nstop.rule <- list(max.taxa   = 10,\n                  max.living = 10,\n                  max.time   = 10)")
    error <- capture_error(dads(stop.rule = list(something = 2)))
    expect_equal(error[[1]], "You must provide at least one stopping rule. For example:\nstop.rule <- list(max.taxa   = 10,\n                  max.living = 10,\n                  max.time   = 10)")

    ## Default birth.death
    test <- dads(stop.rule = list("max.living" = 10))
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)

    set.seed(1)
    test <- dads(bd.params = list(extinction = 0.2), stop.rule = list("max.taxa" = 10))
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)
})


test_that("dads works for trees + traits", {

    ## Tree + traits

    ## Sanitizing
    error <- capture_error(dads(stop.rule = list(max.taxa = 10), traits = 1))
    expect_equal(error[[1]], "traits must be of class \"dads\" \"traits\". Use make.traits() to format the object correctly.")
    

    ## Simple birth.death tree with one BM trait
    test <- dads(stop.rule = list("max.living" = 10), traits = make.traits())
    expect_is(test, "dads")
    expect_equal(names(test), c("tree", "data", "traits"))
    expect_is(test$tree, "phylo")
    expect_equal(dim(test$data), c(19, 1))
    
    ## A more complex trait
    complex_traits <- make.traits(process = c(BM.process, BM.process), n = c(2,3), process.args = list(list(Sigma = diag(2)), list(Sigma = matrix(1/3, 3, 3))), trait.names = c("bib", "bob"))
    test <- dads(stop.rule = list("max.living" = 10), traits = complex_traits)
    expect_is(test, "dads")
    expect_equal(names(test), c("tree", "data", "traits"))
    expect_is(test$tree, "phylo")
    expect_equal(dim(test$data), c(19, 5))
})


#test_that("dads works for trees + modifiers", {
#})

#test_that("dads works for trees + events", {
#})

#test_that("dads works for trees + traits + modifiers + events", {
#})
