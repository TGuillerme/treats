context("make.modifiers")

## Test
test_that("make.modifiers works", {

# branch.length <- branch.length.trait
# speciation <- speciation.trait
# condition <- function(trait.values, parent.lineage) return(get.parent.traits(trait.values, parent.lineage) < 0)
# modify <- function(x) return(x * 10)
# add = FALSE
# test = TRUE


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
    expect_equal(error[[1]], "modifiers can only be added to objects of class dads and modifiers")

    ## Wrong arguments for test
    error <- capture_error(make.modifiers(test = "whatever"))
    expect_equal(error[[1]], "test must be of class logical.")

})
