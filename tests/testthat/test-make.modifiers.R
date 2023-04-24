## Test
test_that("make.modifiers works", {

    ## Sanitizing
    ## Wrong arguments for branch length
    wrong.bl <- function(x, y, lineage) return("ah")
    error <- capture_error(make.modifiers(branch.length = wrong.bl))
    expect_equal(error[[1]], "The branch length function cannot recognise the x, y arguments.")
    wrong.bl <- function(bd.params = NULL, lineage = NULL, trait.values = NULL, modify.fun = NULL) return("ah")
    error <- capture_error(make.modifiers(branch.length = wrong.bl))
    expect_equal(error[[1]], "The branch length element from the modifiers did not produce a numeric value (it produced a character instead).")

    ## Wrong arguments for selection
    wrong.sl <- function(x, y, lineage) return("oh")
    error <- capture_error(make.modifiers(selection = wrong.sl))
    expect_equal(error[[1]], "The selection function cannot recognise the x, y arguments.")
    wrong.sl <- function(bd.params = NULL, lineage = NULL, trait.values = NULL, modify.fun = NULL) return("oh")
    error <- capture_error(make.modifiers(selection = wrong.sl))
    expect_equal(error[[1]], "The selection element from the modifiers did not produce a integer value (it produced a character instead).")

    ## Wrong arguments for speciation
    wrong.sp <- function(x, y, lineage) return("ah")
    error <- capture_error(make.modifiers(speciation = wrong.sp))
    expect_equal(error[[1]], "The speciation function cannot recognise the x, y arguments.")
    wrong.sp <- function(bd.params = NULL, lineage = NULL, trait.values = NULL, modify.fun = NULL) return("ah")
    error <- capture_error(make.modifiers(speciation = wrong.sp))
    expect_equal(error[[1]], "The speciation element from the modifiers did not produce a logical value (it produced a character instead).")

    ## Wrong arguments for condition
    wrong.con <- function(x) return("a")
    error <- capture_error(make.modifiers(condition = wrong.con))
    expect_equal(error[[1]], "The condition function cannot recognise the x argument.")
    wrong.con <- function(lineage, y, ya) return("a")
    error <- capture_error(make.modifiers(condition = wrong.con))
    expect_equal(error[[1]], "The condition function cannot recognise the y, ya arguments.")

    ## Wrong arguments for modify
    wrong.mod <- function(x, y) return("a")
    error <- capture_error(make.modifiers(modify = wrong.mod))
    expect_equal(error[[1]], "The modify function cannot recognise the y argument.")
    wrong.mod <- function(lineage, y) return("a")
    error <- capture_error(make.modifiers(modify = wrong.mod))
    expect_equal(error[[1]], "The modify function cannot recognise the y argument.")

    ## Wrong arguments for add
    error <- capture_error(make.modifiers(add = TRUE))
    expect_equal(error[[1]], "You can only add to a \"treats\" \"modifiers\" object. Check the documentation from the following function for helping designing such objects:\n    ?make.modifiers")

    ## Wrong arguments for test
    error <- capture_error(make.modifiers(test = "whatever"))
    expect_equal(error[[1]], "test must be of class logical.")

    ## Working fine?
    condition <- function(trait.values, lineage) return(parent.traits(trait.values, lineage) < 0)
    modify <- function(x, trait.values, lineage) return(x * 20)
    test <- make.modifiers(branch.length = branch.length,
                           selection     = selection,
                           speciation    = speciation,
                           condition     = condition,
                           modify        = modify)
    expect_is(test, c("treats", "modifiers"))
    expect_equal(names(test), c("waiting", "selecting", "speciating", "call"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_equal(names(test[[3]]), c("fun", "internal"))

    ## Working correctly without condition and modify
    test <- make.modifiers(branch.length = branch.length.trait,
                           speciation    = speciation.trait)    
    expect_is(test, c("treats", "modifiers"))
    expect_equal(names(test), c("waiting", "selecting", "speciating", "call"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_equal(names(test[[3]]), c("fun", "internal"))
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
    expect_is(test, c("treats", "modifiers"))
    expect_equal(names(test), c("waiting", "selecting", "speciating", "call"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_equal(names(test[[3]]), c("fun", "internal"))
    expect_false(test[[1]]$internal$condition())
    expect_equal(test[[1]]$internal$modify(42), 42)

    ## Working correctly with just modify
    always.one <- function() return(1)
    error <- capture_error(
        test <- make.modifiers(branch.length = branch.length.trait,
                           speciation    = speciation.trait,
                           modify        = always.one)
        )
    expect_null(error)
    always.one <- function(x = NULL) return(1)
    test <- make.modifiers(branch.length = branch.length.trait,
                           speciation    = speciation.trait,
                           modify        = always.one)
    expect_is(test, c("treats", "modifiers"))
    expect_equal(names(test), c("waiting", "selecting", "speciating", "call"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_equal(names(test[[3]]), c("fun", "internal"))
    expect_true(test[[1]]$internal$condition())
    expect_equal(test[[1]]$internal$modify(42), 1)

    ## Working correctly without speciation
    test <- make.modifiers(branch.length = branch.length,
                           condition     = condition,
                           modify        = modify)
    expect_is(test, c("treats", "modifiers"))
    expect_equal(names(test), c("waiting", "selecting", "speciating", "call"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_equal(names(test[[3]]), c("fun", "internal"))

    ## Working correctly without branch.length
    test <- make.modifiers(speciation    = speciation,
                           condition     = condition,
                           modify        = modify)
    expect_is(test, c("treats", "modifiers"))
    expect_equal(names(test), c("waiting", "selecting", "speciating", "call"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_equal(names(test[[3]]), c("fun", "internal"))

    ## Adds works correctly
    base <- make.modifiers()
    expect_is(base, c("treats", "modifiers"))
    expect_equal(names(base), c("waiting", "selecting", "speciating", "call"))
    expect_equal(names(base[[1]]), c("fun", "internal"))
    expect_equal(names(base[[2]]), c("fun", "internal"))
    expect_null(base[[1]]$internal$condition)
    expect_null(base[[1]]$internal$modify)

    ## Add a condition for speciation
    test <- NULL
    expect_message(test <- make.modifiers(speciation = speciation, condition = always.false, add = base))
    expect_is(test, c("treats", "modifiers"))
    expect_equal(names(test), c("waiting", "selecting", "speciating", "call"))
    expect_equal(names(test[[1]]), c("fun", "internal"))
    expect_equal(names(test[[2]]), c("fun", "internal"))
    expect_equal(names(test[[3]]), c("fun", "internal"))
    expect_null(test[[1]]$internal$condition)
    expect_null(test[[1]]$internal$modify)
    expect_null(test[[2]]$internal$condition)
    expect_null(test[[2]]$internal$modify)
    expect_false(test[[3]]$internal$condition())
    expect_equal(test[[3]]$internal$modify(42), 42)

    test2 <- NULL
    expect_message(test2 <- make.modifiers(branch.length = branch.length, modify = always.one, add = test))
    expect_is(test2, c("treats", "modifiers"))
    expect_equal(names(test2), c("waiting", "selecting", "speciating", "call"))
    expect_equal(names(test2[[1]]), c("fun", "internal"))
    expect_equal(names(test2[[2]]), c("fun", "internal"))
    expect_equal(names(test2[[3]]), c("fun", "internal"))
    expect_true(test2[[1]]$internal$condition())
    expect_equal(test2[[1]]$internal$modify(42), 1)
    expect_null(test[[2]]$internal$condition)
    expect_null(test[[2]]$internal$modify)
    expect_false(test2[[3]]$internal$condition())
    expect_equal(test2[[3]]$internal$modify(42), 42)

    ## Print modifiers works correctly
})

## Updating
test_that("make.modifiers(update) works", {

    ## Sanitizing
    error <- capture_error(make.modifiers(update = "bob"))
    expect_equal(error[[1]], "You can only update a \"treats\" \"modifiers\" object. Check the documentation from the following function for helping designing such objects:\n    ?make.modifiers")
    test <- make.modifiers()
    error <- capture_error(make.modifiers(update = test, branch.length = "what"))
    expect_equal(error[[1]], "function for branch length is not a function.")
    error <- capture_error(make.modifiers(update = test, add = test))
    expect_equal(error[[1]], "Impossible to add and update a modifiers object at the same time.")
    
    # ## Nothing happens
    test2 <- make.modifiers(update = test)
    expect_equal(capture_output(print(test)), capture_output(print(test2)))

    ## Speciation is updated
    test1 <- make.modifiers()
    test2 <- make.modifiers(update = test1, speciation = speciation)
    ## They are different speciating functions
    expect_true(capture_output(print(test1$speciating$fun)) != capture_output(print(test2$speciating$fun)))

    always.true <- function() return(TRUE)
    always.false <- function() return(FALSE)
    test1 <- make.modifiers(branch.length = branch.length, condition = always.true)
    test2 <- make.modifiers(update = test1, condition = always.false)
    ## They are different branch.length internals
    expect_true(capture_output(print(test1$waiting$internal)) != capture_output(print(test2$waiting$internal)))
    ## But the same functions
    expect_true(capture_output(print(test1$waiting$fun)) == capture_output(print(test2$waiting$fun)))
})

## Modifiers
# test_that("modifiers works", {

#     ## branch.length
#     bd.params <- list(speciation = 1, extinction = 0.1)
#     lineage <- list(n = 10)
#     branch.length(bd.params = list(speciation = 1, extinction = 0.1))





#     ## branch.length.trait
#     ## selection
#     ## speciation
#     ## speciation.trait

#     test2 <- NULL
#     expect_message(test2 <- make.modifiers(branch.length = branch.length, modify = always.one, add = test))
#     expect_is(test2, c("treats", "modifiers"))
#     expect_equal(names(test2), c("waiting", "selecting", "speciating", "call"))
#     expect_equal(names(test2[[1]]), c("fun", "internal"))
#     expect_equal(names(test2[[2]]), c("fun", "internal"))
#     expect_equal(names(test2[[3]]), c("fun", "internal"))
#     expect_true(test2[[1]]$internal$condition())
#     expect_equal(test2[[1]]$internal$modify(42), 1)
#     expect_null(test[[2]]$internal$condition)
#     expect_null(test[[2]]$internal$modify)
#     expect_false(test2[[3]]$internal$condition())
#     expect_equal(test2[[3]]$internal$modify(42), 42)

#     ## Print modifiers works correctly
# })
