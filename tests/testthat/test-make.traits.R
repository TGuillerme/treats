## Test
test_that("make.traits works", {

    ## Sanitizing
    error <- capture_error(make.traits(process = "Hahaha"))
    expect_equal(error[[1]], "process must be of class function or list.")
    error <- capture_error(make.traits(n = "Hahaha"))
    expect_equal(error[[1]], "n must be of class integer or numeric.")
    error <- capture_error(make.traits(start = mean))
    expect_equal(error[[1]], "start must be of class integer or numeric.")
    error <- capture_error(make.traits(trait.names = c("1", "2")))
    expect_equal(error[[1]], "trait.names must be the same length as the number of process(es).")
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


    ## Add works
    test2 <- make.traits(process = c(BM.process, element.rank), n = c(4,2), add = test, start = 1)
    expect_is(test2, c("dads", "traits"))
    expect_equal(length(test2), 3)
    expect_equal(names(test2$A), c("process", "start", "trait_id"))
    expect_equal(test2$A$trait_id, c(1,2))
    expect_equal(test2$A$start, rep(0, 2))
    expect_equal(test2$B$trait_id, c(3,4,5,6))
    expect_equal(test2$B$start, rep(1, 4))
    expect_equal(test2$C$trait_id, c(7,8))
    expect_equal(test2$C$start, rep(1, 2))

    ## Add works with proper name update
    test2 <- make.traits(process = c(BM.process, element.rank), n = c(4,2), add = make.traits(trait.names = "bob"))
    expect_is(test2, c("dads", "traits"))
    expect_equal(length(test2), 3)
    expect_equal(names(test2$bob), c("process", "start", "trait_id"))
    expect_equal(test2$bob$trait_id, c(1))
    expect_equal(test2$A$trait_id, c(2,3,4,5))
    expect_equal(test2$B$trait_id, c(6,7))

    ## cascade of adding traits (who the fuck would ever do that?)
    test <- make.traits(add = make.traits(add = make.traits(add = make.traits())))
    expect_equal(length(test), 4)
    expect_equal(names(test), LETTERS[1:4])
    


    

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
    expect_equal(test[[2]]$start, c(0,0))

    test <- make.traits(process = c(BM.process, element.rank), n = c(3, 1), start = 0, trait.names = c("A", "B"))
    expect_is(test, c("dads", "traits"))
    expect_equal(length(test), 2)
    expect_equal(names(test), c("A", "B"))
    expect_is(test$A$process, "function")
    expect_equal(test$A$start, c(0,0,0))
    expect_equal(test$A$trait_id, c(1,2,3))
    expect_is(test$B$process, "function")
    expect_equal(test$B$start, 0)
    expect_equal(test$B$trait_id, 4)

    ## Errors (not enough extra arguments)
    error <- capture_error(make.traits(process = c(BM.process, BM.process), process.args = list(Sigma = diag(1))))
    expect_equal(error[[1]], "You must provide additional arguments for every process (2). You can provide NULL arguments for processes that don't need extra arguments e.g.\n\n    process.args = list(list(NULL),\n                        list(extra.arg = some_extra_argument))\n\nwill only provide extra arguments to the second process.")
    ## Working well with a single extra argument
    test <- make.traits(n = 3, process.args = list(Sigma = diag(3)))    
    expect_is(test, c("dads", "traits"))
    expect_equal(names(test$A), c("process", "start", "trait_id", "Sigma"))
    ## Working well on multiple processes
    test <- make.traits(process = c(BM.process, BM.process), n = c(2,3), process.args = list(list(Sigma = diag(2)), list(Sigma = matrix(1/3, 3, 3))))
    expect_equal(test$A$Sigma, diag(2))
    expect_equal(test$B$Sigma, matrix(1/3, 3, 3))

    ## Working well on multiple processes with null arguments
    test <- make.traits(process = c(BM.process, OU.process), n = c(2, 3), process.args = list(list(Sigma = diag(2)), list(NULL)))
    expect_equal(names(test$A), c("process", "start", "trait_id", "Sigma"))
    expect_equal(names(test$B), c("process", "start", "trait_id"))

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