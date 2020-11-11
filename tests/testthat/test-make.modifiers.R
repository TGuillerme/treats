context("make.modifiers")

## Test
test_that("make.modifiers works", {

    ## Sanitizing
    ## Wrong arguments for branch length
    wrong.bl <- function(x, y, n.taxa) return("ah")
    error <- capture_error(make.modifiers(branch.length = wrong.bl))
    expect_equal(error[[1]], "The branch.length function is missing the following arguments: bd.params, parent.lineage, trait.values, modify.fun. If they are not required, you can set them to NULL.")
    wrong.bl <- function(bd.params = NULL, parent.lineage = NULL, trait.values = NULL, modify.fun = NULL) return("ah")
    error <- capture_error(make.modifiers(branch.length = wrong.bl))
    expect_equal(error[[1]], "The branch.length function is missing the following argument: n.taxa. If it is not required, you can set it to NULL.")

    ## Wrong arguments for speciation
    wrong.sp <- function(x, y, n.taxa) return("ah")
    error <- capture_error(make.modifiers(speciation = wrong.sp))
    expect_equal(error[[1]], "The speciation function is missing the following arguments: bd.params, parent.lineage, trait.values, modify.fun. If they are not required, you can set them to NULL.")
    wrong.sp <- function(bd.params = NULL, parent.lineage = NULL, trait.values = NULL, modify.fun = NULL) return("ah")
    error <- capture_error(make.modifiers(speciation = wrong.sp))
    expect_equal(error[[1]], "The speciation function is missing the following argument: n.taxa. If it is not required, you can set it to NULL.")

    ## Wrong arguments for condition
    wrong.con <- function(x) return("a")
    error <- capture_error(make.modifiers(condition = wrong.con))
    expect_equal(error[[1]], "The condition function cannot recognise the x argument.")
    wrong.con <- function(n.taxa, y, ya) return("a")
    error <- capture_error(make.modifiers(condition = wrong.con))
    expect_equal(error[[1]], "The condition function cannot recognise the y, ya arguments.")

    # ## Wrong arguments for modify
    wrong.mod <- function(x, y) return("a")
    error <- capture_error(make.modifiers(modify = wrong.mod))
    expect_equal(error[[1]], "The modify function cannot recognise the y argument.")
    wrong.mod <- function(n.taxa, y) return("a")
    error <- capture_error(make.modifiers(modify = wrong.mod))
    expect_equal(error[[1]], "The modify function must have at least one x argument (you can use x = NULL).")

    ## Wrong arguments for add
    error <- capture_error(make.modifiers(add = TRUE))
    expect_equal(error[[1]], "modifiers can only be added to objects of class dads and modifiers.")

    ## Wrong arguments for test
    error <- capture_error(make.modifiers(test = "whatever"))
    expect_equal(error[[1]], "test must be of class logical.")

    ## Working fine?
    condition <- function(trait.values, parent.lineage) return(get.parent.traits(trait.values, parent.lineage) < 0)
    modify <- function(x, trait.values, parent.lineage) return(x * 20)
    test <- make.modifiers(branch.length = branch.length.trait,
                           speciation    = speciation.trait,
                           condition     = condition,
                           modify        = modify)
    expect_is(test, c("dads", "modifiers"))
    expect_equal(names(test), c("waiting", "speciating"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))

    ## Working correctly without condition and modify
    test <- make.modifiers(branch.length = branch.length.trait,
                           speciation    = speciation.trait)    
    expect_is(test, c("dads", "modifiers"))
    expect_equal(names(test), c("waiting", "speciating"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_true(test[[1]]$internal$condition())
    expect_equal(test[[1]]$internal$modify(42), 42)

    ## Working correctly with just condition
    always.false <- function(whatever) return(FALSE)
    error <- capture_error(
        test <- make.modifiers(branch.length = branch.length.trait,          
                           speciation    = speciation.trait,
                           condition     = always.false)
        )
    expect_equal(error[[1]], "The condition function cannot recognise the whatever argument.")
    always.false <- function() return(FALSE)
    test <- make.modifiers(branch.length = branch.length.trait,
                           speciation    = speciation.trait,
                           condition     = always.false)    
    expect_is(test, c("dads", "modifiers"))
    expect_equal(names(test), c("waiting", "speciating"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_false(test[[1]]$internal$condition())
    expect_equal(test[[1]]$internal$modify(42), 42)

    ## Working correctly with just modify
    always.one <- function() return(1)
    error <- capture_error(
        test <- make.modifiers(branch.length = branch.length.trait,
                           speciation    = speciation.trait,
                           modify        = always.one)
        )
    expect_equal(error[[1]], "The modify function must have at least one x argument (you can use x = NULL).")
    always.one <- function(x = NULL) return(1)
    test <- make.modifiers(branch.length = branch.length.trait,
                           speciation    = speciation.trait,
                           modify        = always.one)
    expect_is(test, c("dads", "modifiers"))
    expect_equal(names(test), c("waiting", "speciating"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_true(test[[1]]$internal$condition())
    expect_equal(test[[1]]$internal$modify(42), 1)

    ## Working correctly without speciation
    test <- make.modifiers(branch.length = branch.length.trait,
                           condition     = condition,
                           modify        = modify)
    expect_is(test, c("dads", "modifiers"))
    expect_equal(names(test), c("waiting", "speciating"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))

    ## Working correctly without branch.length
    test <- make.modifiers(speciation    = speciation.trait,
                           condition     = condition,
                           modify        = modify)
    expect_is(test, c("dads", "modifiers"))
    expect_equal(names(test), c("waiting", "speciating"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))

    ## Adds works correctly
    base <- make.modifiers()
    expect_is(base, c("dads", "modifiers"))
    expect_equal(names(base), c("waiting", "speciating"))
    expect_equal(names(base[[1]]), c("fun", "internal"))
    expect_equal(names(base[[2]]), c("fun", "internal"))
    expect_null(base[[1]]$internal$condition)
    expect_null(base[[1]]$internal$modify)

    ## Add a condition for speciation
    test <- NULL
    expect_message(test <- make.modifiers(speciation = speciation, condition = always.false, add = base))
    expect_is(test, c("dads", "modifiers"))
    expect_equal(names(test), c("waiting", "speciating"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_null(test[[1]]$internal$condition)
    expect_null(test[[1]]$internal$modify)
    expect_false(test[[2]]$internal$condition())
    expect_equal(test[[2]]$internal$modify(42), 42)

    test2 <- NULL
    expect_message(test2 <- make.modifiers(branch.length = branch.length, modify = always.one, add = test))
    expect_is(test2, c("dads", "modifiers"))
    expect_equal(names(test2), c("waiting", "speciating"))
    expect_equal(names(test2[[1]]), c("fun", "internal"))
    expect_equal(names(test2[[2]]), c("fun", "internal"))
    expect_true(test2[[1]]$internal$condition())
    expect_equal(test2[[1]]$internal$modify(42), 1)
    expect_false(test2[[2]]$internal$condition())
    expect_equal(test2[[2]]$internal$modify(42), 42)

    ## Print modifiers works correctly

})
