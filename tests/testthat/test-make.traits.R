context("make.traits")

## Test
test_that("make.traits works", {

    ## Sanitizing
    error <- capture_error(make.traits(process = "Hahaha"))
    expect_equal(error[[1]], "process must be of class function or list.")
    error <- capture_error(make.traits(n = "Hahaha"))
    expect_equal(error[[1]], "n must be of class integer or numeric.")
    error <- capture_error(make.traits(start = mean))
    expect_equal(error[[1]], "start must be of class integer or numeric.")
    error <- capture_error(make.traits(names = c("1", "2")))
    expect_equal(error[[1]], "names must be the same length as the number of characters.")
    error <- capture_error(make.traits(process.args = "nope"))
    expect_equal(error[[1]], "process.args must be of class list.")
    error <- capture_error(make.traits(process.args = "nope"))
    expect_equal(error[[1]], "process.args must be of class list.")
    error <- capture_error(make.traits(add = TRUE))
    expect_equal(error[[1]], "traits can only be added to objects of class dads and traits.")
    error <- capture_error(make.traits(test = "TRUE"))
    expect_equal(error[[1]], "test must be of class logical.")

    ## Default example works
    test <- make.traits()
    expect_is(test, c("dads", "traits"))
    expect_equal(length(test), 1)
    expect_equal(names(test[[1]]), c("process", "start", "trait_id"))
    expect_equal(test[[1]]$trait_id, 1)
    expect_equal(test[[1]]$start   , 0)
    expect_is(test[[1]]$process, "function")

    ## Good handling of n
    element.rank <- function(x0, ...) {
        return(x0 + 1)
    }

    test <- make.traits(n = 2)
    expect_is(test, c("dads", "traits"))
    expect_equal(length(test), 1)
    expect_equal(names(test[[1]]), c("process", "start", "trait_id"))
    expect_equal(test[[1]]$trait_id, c(1,2))
    expect_equal(test[[1]]$start   , c(0,0))
    expect_is(test[[1]]$process, "function")

    test <- make.traits(process = c(BM.process, element.rank), n = 2)
    expect_is(test, c("dads", "traits"))
    expect_equal(length(test), 2)
    expect_equal(names(test[[1]]), c("process", "start", "trait_id"))
    expect_equal(test[[1]]$trait_id, c(1,2))
    expect_equal(test[[1]]$start   , c(0,0))
    expect_is(test[[1]]$process, "function")
    expect_equal(test[[2]]$trait_id, c(3,4))


    test <- make.traits(process = c(BM.process, element.rank), n = c(1, 2))
    expect_is(test, c("dads", "traits"))
    expect_equal(length(test), 2)
    expect_equal(names(test[[1]]), c("process", "start", "trait_id"))
    expect_equal(test[[1]]$trait_id, 1)
    expect_equal(test[[1]]$start   , 0)
    expect_is(test[[1]]$process, "function")
    expect_equal(test[[2]]$trait_id, c(2,3))


    # ## Complex trait
    # branch.length <- function(x0, edge.length, ...) {
    #     return(edge.length)
    # }
    # expected <- list(
    #             "A" = list(process  = BM.process,
    #                        start    = c(0,0,0),
    #                        trait_id = 1:3),
    #             "B" = list(process  = branch.length,
    #                        start    = 0,
    #                        trait_id = 4,)
    #             )



})



#' ## A simple Brownian motion trait (default)
#' make.traits()
#' 
#' ## Two independent Brownian motion traits
#' make.traits(n = 2)
#' 
#' ## Two different traits with different process
#' ## (Brownian motion and Ornstein–Uhlenbeck)
#' make.traits(process = list(BM.process, step.OU))
#' 
#' ## A multidimensional Brownian motion trait with correlation
#' ## and different starting points
#' (my_traits <- make.traits(process.args = list(n = 4,
#'                                               sd = diag(4))),
#'                           start = c(0, 1, 2, 3))
#' 
#' ## Adding a Ornstein–Uhlenbeck trait to the previous trait object
#' make.traits(process = step.OU, names = "OU_trait",
#'             add = my_traits