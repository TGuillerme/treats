## Test
test_that("make.traits works", {
    ## Sanitizing
    error <- capture_error(make.traits(process = "Hahaha"))
    expect_equal(error[[1]], "process must be of class function or list.")
    error <- capture_error(make.traits(n = "Hahaha"))
    expect_equal(error[[1]], "n must be of class integer or numeric.")
    error <- capture_error(make.traits(start = mean))
    expect_equal(error[[1]], "start must be of class integer or numeric.")
    # error <- capture_error(make.traits(trait.names = c("1", "2")))
    # expect_equal(error[[1]], "trait.names must be the same length as the number of process(es).")
    error <- capture_error(make.traits(process.args = "nope"))
    expect_equal(error[[1]], "process.args must be of class list.")
    error <- capture_error(make.traits(process.args = "nope"))
    expect_equal(error[[1]], "process.args must be of class list.")
    error <- capture_error(make.traits(add = TRUE))
    expect_equal(error[[1]], "You can only add to a \"treats\" \"traits\" object. Check the documentation from the following function for helping designing such objects:\n    ?make.traits")
    error <- capture_error(make.traits(test = "TRUE"))
    expect_equal(error[[1]], "test must be of class logical.")

    ## Default example works
    test <- make.traits()
    expect_is(test, c("treats", "traits"))
    expect_equal(length(test), 2)
    expect_equal(names(test), c("main", "background"))
    expect_equal(names(test$main[[1]]), c("process", "start", "trait_id"))
    expect_equal(test$main[[1]]$trait_id, 1)
    expect_equal(test$main[[1]]$start   , 0)
    expect_is(test$main[[1]]$process, "function")
    expect_null(test$background)


    ## Good handling of n
    element.rank <- function(x0, ...) {
        return(x0 + 1)
    }

    test <- make.traits(n = 2)
    expect_is(test, c("treats", "traits"))
    expect_equal(length(test), 2)
    expect_equal(names(test$main[[1]]), c("process", "start", "trait_id"))
    expect_equal(test$main[[1]]$trait_id, c(1,2))
    expect_equal(test$main[[1]]$start   , c(0,0))
    expect_is(test$main[[1]]$process, "function")


    ## Add works
    test2 <- make.traits(process = c(BM.process, element.rank), n = c(4,2), add = test, start = 1)
    expect_is(test2, c("treats", "traits"))
    expect_equal(length(test2), 2)
    expect_equal(length(test2$main), 3)
    expect_equal(names(test2$main$A), c("process", "start", "trait_id"))
    expect_equal(test2$main$A$trait_id, c(1,2))
    expect_equal(test2$main$A$start, rep(0, 2))
    expect_equal(test2$main$B$trait_id, c(3,4,5,6))
    expect_equal(test2$main$B$start, rep(1, 4))
    expect_equal(test2$main$C$trait_id, c(7,8))
    expect_equal(test2$main$C$start, rep(1, 2))

    ## Add works with proper name update
    test2 <- make.traits(process = c(BM.process, element.rank), n = c(4,2), add = make.traits(trait.names = "bob"))
    expect_is(test2, c("treats", "traits"))
    expect_equal(length(test2$main), 3)
    expect_equal(names(test2$main$bob), c("process", "start", "trait_id"))
    expect_equal(test2$main$bob$trait_id, c(1))
    expect_equal(test2$main$A$trait_id, c(2,3,4,5))
    expect_equal(test2$main$B$trait_id, c(6,7))

    ## cascade of adding traits (who the fuck would ever do that?)
    test <- make.traits(add = make.traits(add = make.traits(add = make.traits())))
    expect_equal(length(test$main), 4)
    expect_equal(names(test$main), LETTERS[1:4])

    test <- make.traits(process = c(BM.process, element.rank), n = 2)
    expect_is(test, c("treats", "traits"))
    expect_equal(length(test$main), 2)
    expect_equal(names(test$main[[1]]), c("process", "start", "trait_id"))
    expect_equal(test$main[[1]]$trait_id, c(1,2))
    expect_equal(test$main[[1]]$start   , c(0,0))
    expect_is(test$main[[1]]$process, "function")
    expect_equal(test$main[[2]]$trait_id, c(3,4))

    test <- make.traits(process = c(BM.process, element.rank), n = c(1, 2))
    expect_is(test, c("treats", "traits"))
    expect_equal(length(test$main), 2)
    expect_equal(names(test$main[[1]]), c("process", "start", "trait_id"))
    expect_equal(test$main[[1]]$trait_id, 1)
    expect_equal(test$main[[1]]$start   , 0)
    expect_is(test$main[[1]]$process, "function")
    expect_equal(test$main[[2]]$trait_id, c(2,3))
    expect_equal(test$main[[2]]$start, c(0,0))

    test <- make.traits(process = c(BM.process, element.rank), n = c(3, 1), start = 0, trait.names = c("A", "B"))
    expect_is(test, c("treats", "traits"))
    expect_equal(length(test$main), 2)
    expect_equal(names(test$main), c("A", "B"))
    expect_is(test$main$A$process, "function")
    expect_equal(test$main$A$start, c(0,0,0))
    expect_equal(test$main$A$trait_id, c(1,2,3))
    expect_is(test$main$B$process, "function")
    expect_equal(test$main$B$start, 0)
    expect_equal(test$main$B$trait_id, 4)

    ## Errors (not enough extra arguments)
    error <- capture_error(make.traits(process = c(BM.process, BM.process), process.args = list(Sigma = diag(1))))
    expect_equal(error[[1]], "You must provide additional arguments for every process (2). You can provide NULL arguments for processes that don't need extra arguments e.g.\n\n    process.args = list(list(NULL),\n                        list(extra.arg = some_extra_argument))\n\nwill only provide extra arguments to the second process.")
    ## Working well with a single extra argument
    test <- make.traits(n = 3, process.args = list(Sigma = diag(3)))    
    expect_is(test, c("treats", "traits"))
    expect_equal(names(test$main$A), c("process", "start", "trait_id", "Sigma"))
    ## Working well on multiple processes
    test <- make.traits(process = c(BM.process, BM.process), n = c(2,3), process.args = list(list(Sigma = diag(2)), list(Sigma = matrix(1/3, 3, 3))))
    expect_equal(test$main$A$Sigma, diag(2))
    expect_equal(test$main$B$Sigma, matrix(1/3, 3, 3))

    ## Working well on multiple processes with null arguments
    test <- make.traits(process = c(BM.process, OU.process), n = c(2, 3), process.args = list(list(Sigma = diag(2)), list(NULL)))
    expect_equal(names(test$main$A), c("process", "start", "trait_id", "Sigma"))
    expect_equal(names(test$main$B), c("process", "start", "trait_id"))
})

test_that("make.traits(update) works", {

    ## Sanitizing
    error <- capture_error(make.traits(update = "bob"))
    expect_equal(error[[1]], "You can only update a \"treats\" \"traits\" object. Check the documentation from the following function for helping designing such objects:\n    ?make.traits")
    test <- make.traits()
    error <- capture_error(make.traits(update = test, trait.names = "what"))
    expect_equal(error[[1]], "No process(es) called what to update.")
    error <- capture_error(make.traits(update = test, add = test))
    expect_equal(error[[1]], "Impossible to add and update a traits object at the same time.")

    ## Nothing happens
    test2 <- make.traits(update = test)
    expect_equal(capture_output(print(test)), capture_output(print(test2)))

    ## Process is updated
    test1 <- make.traits(n = 2)
    test2 <- make.traits(update = test1, process = OU.process)
    ## They are different processes
    expect_true(capture_output(print(test1$main[[1]]$process)) != capture_output(print(test2$main[[1]]$process)))

    test1 <- make.traits(process = list(BM.process, OU.process))
    test2 <- make.traits(process = BM.process, trait.names = "B", update = test1)
    ## They are different processes for test1
    expect_true(capture_output(print(test1$main[[1]]$process)) != capture_output(print(test1$main[[2]]$process)))
    ## But the same for test2
    expect_true(capture_output(print(test2$main[[1]]$process)) == capture_output(print(test2$main[[2]]$process)))

    my_correlations <- matrix(1, ncol = 3, nrow = 3)
    test <- make.traits(n = 3, start = c(0, 1, 3),
                        process.args = list(Sigma = my_correlations))
    my_correlation2 <- matrix(2, ncol = 3, nrow = 3) 
    test2 <- make.traits(update = test,
                         process.args = list(Sigma = my_correlation2))
    expect_equal(unique(c(test$main[[1]]$Sigma)), 1)
    expect_equal(unique(c(test2$main[[1]]$Sigma)), 2)

    ## Complex update
    traits <- make.traits(n = 2, process.args = list(Sigma = matrix(1, 2, 2)))
    traits <- make.traits(process = OU.process, trait.name = "noupdate", add = traits)

    ## Update just the Sigma for the two traits of the first process
    traits2 <- make.traits(update = traits, process.args = list(Sigma = matrix(c(10,3,3,2),2,2)), trait.name = "A", start = 1)

    expect_equal(traits$main$A$Sigma, matrix(1, 2, 2))
    expect_equal(traits2$main$A$Sigma, matrix(c(10,3,3,2),2,2))
    expect_equal(traits$main$A$start, c(0,0))
    expect_equal(traits2$main$A$start, c(1,1))
    expect_equal(traits2$main$noupdate$start, 0)
})

test_that("different processes works", {

    ## BM.process

    ## OU.process

    ## no.process

    ## multi.peak.process
    expect_equal(peak.diff(1, c(1:3)), 0:2)
    expect_equal(closest.peak(c(1:3), c(1:3)), 1)
    expect_equal(closest.peak(c(7, 8, 4), c(1:3)), 3)
    ## Single trait three optimums
    expect_equal(round(multi.peak.process(x0 = 0, peaks = c(0, 1, 8), alpha = 1000))[1], 0)
    expect_equal(round(multi.peak.process(x0 = 7, peaks = c(0, 1, 8), alpha = 1000))[1], 8)
    expect_equal(round(multi.peak.process(x0 = 2/3, peaks = c(0, 1, 8), alpha = 1000))[1], 1)
    expect_equal(round(multi.peak.process(x0 = 1000, peaks = c(0, 1, 8), alpha = 1000))[1], 8)
    ## three traits three optimums
    expect_equal(round(c(multi.peak.process(x0 = c(0, 0, 0), peaks = c(0, 1, 8), alpha = 1000))), c(0, 0, 0))
    expect_equal(round(c(multi.peak.process(x0 = c(10, 10, 10), peaks = c(0, 1, 8), alpha = 1000))), c(8, 8, 8))
    expect_equal(round(c(multi.peak.process(x0 = c(7, -2, 3), peaks = c(0, 1, 8), alpha = 1000))), c(8, 0, 1))
    ## Three traits with two optimums each
    optimums <- list(c(-1, 1), c(8, 0), c(5.5, 6))
    results <- c(multi.peak.process(x0 = c(7, -2, 3), peaks = optimums, alpha = 1000))
    expect_equal(round(results[1:2]), c(1, 0))
    expect_equal(round(results[3], 1), c(5.5))

    ## Discrete traits
    expect_equal(discrete.process(0, 1, transitions = transition.matrix("ER", 2, self = FALSE)), 1)
    expect_equal(discrete.process(0, 1, transitions = matrix(c(1,0,0,1), 2, 2)), 0)
    expect_equal(discrete.process(1, 100, transitions = matrix(c(1,0,0,1), 2, 2)), 1)
    set.seed(1)
    expect_equal(discrete.process(0, 1, transitions = transition.matrix("ER", 2)), 1)
    expect_equal(discrete.process(0, 1, transitions = transition.matrix("ER", 2)), 0)
})

test_that("bkg.traits works", {

    ### Error
    error <- capture_error(make.traits(background = "make.traits()"))
    expect_equal(error[[1]], "background must be a treats traits object. You can use the following function to generate one:\nmake.traits()")

    ## Background is the same as the trait
    test <- make.traits(background = make.traits())
    expect_equal(names(test), c("main", "background"))
    expect_equal(names(test$main[[1]]), c("process", "start", "trait_id"))
    expect_equal(test$main[[1]]$trait_id, 1)
    expect_equal(test$main[[1]]$start   , 0)
    expect_is(test$main[[1]]$process, "function")
    expect_equal(names(test$background$main[[1]]), c("process", "start", "trait_id"))
    expect_equal(test$background$main[[1]]$trait_id, 1)
    expect_equal(test$background$main[[1]]$start   , 0)
    expect_is(test$background$main[[1]]$process, "function")

    ## Something a bit more fancy
    error <- capture_error(test2 <- make.traits(process = c(BM.process, BM.process), n = c(4,2), background = make.traits(process = OU.process, start = 10, trait.names = "bg.OU")))
    expect_equal(error[[1]], "The background must have the same number of traits than the main process: 6 (4 + 2).")
    test2 <- make.traits(process = c(BM.process, BM.process), n = c(4,2), background = make.traits(process = OU.process, n = 6, start = 10, trait.names = "bg.OU"))

    expect_is(test2, c("treats", "traits"))
    expect_equal(length(test2$main), 2)
    expect_equal(names(test2$main$A), c("process", "start", "trait_id"))
    expect_equal(test2$main$A$trait_id, c(1,2,3,4))
    expect_equal(test2$main$A$start, rep(0, 4))
    expect_equal(test2$main$B$trait_id, c(5,6))
    expect_equal(test2$main$B$start, rep(0, 2))
    expect_equal(test2$background$main$bg.OU$trait_id, c(1,2,3,4,5,6))
    expect_equal(test2$background$main$bg.OU$start, rep(10, 6))
})

test_that("trait processes work", {

    ## BM.process
    set.seed(1)
    expect_equal(round(BM.process(), 5), matrix(round(-0.6264538, 5)))
    expect_is(plot(BM.process), "list")
    expect_null(plot(make.traits(BM.process)))

    ## OU.process
    set.seed(1)
    expect_equal(round(OU.process(), 5), matrix(round(-0.4119058, 5)))
    expect_is(plot(OU.process), "list")
    expect_null(plot(make.traits(OU.process)))

    ## no.process
    set.seed(1)
    expect_equal(round(no.process(), 5), c(round(-0.6264538, 5)))
    expect_null(plot(make.traits(no.process)))
    expect_null(plot(make.traits(no.process, process.args = list(fun = runif))))

    ## multi.peak.process
    set.seed(1)
    expect_equal(round(multi.peak.process(), 5), matrix(round(-0.4119058, 5)))
    expect_is(plot(multi.peak.process), "list")
    expect_null(plot(make.traits(multi.peak.process, process.args = list(peaks = c(1, 5, 10)))))
    expect_null(plot(make.traits(multi.peak.process, n = 3, start = c(1,2,3), process.args = list(peaks = list(1, 5, 10)))))

    ## repulsion.process
    set.seed(1)
    expect_equal(round(repulsion.process(), 5), c(round(-0.6264538, 5)))
    expect_null(plot(make.traits(repulsion.process)))
    expect_null(plot(make.traits(repulsion.process, process.args = list(repulsion = 5))))
})

test_that("conditional.process works", {




# ## First let's design a discrete islandness trait
# transition_matrix <- matrix(c(3, 0.2, 0.05, 3), 2, 2)

# ## The firs trait (upon which to check the condition)
# discrete_trait <- make.traits(discrete.process, process.args = list(transitions = transition_matrix), trait.name = "my_condition")

# ## The two traits to trigger depending on the condition
# BM_trait <- make.traits(BM.process)
# OU_trait <- make.traits(OU.process)

# ## Add the conditional process:
# conditional_trait <- make.traits(process = conditional.process,
#     ## The trait is going to be conditional on the one called "my_condition" (in the background)
#                                  conditional.trait = "my_condition",
#     ## The trait is going to be either BM_trait or OU_trait depending on
#     ## if "my_condition" is equal to 0 or to 1
#                                  conditions = list(c(`==`, 0), c(`==`, 1)),
#                                  processes  = list(BM_trait, OU_trait),
#                                  background = discrete_trait)

# #


# conditional.process <- function(x0 = 0, edge.length = 1, conditional.trait = 1, conditions = list(c(is, "numeric"))), processes = list(make.traits()), ...) {
#     ## Run through the conditions
#     if(c)
# }



# stop.rule  = list(max.taxa = 100)
#     stop.rule$max.living = Inf
#     stop.rule$max.time = Inf
# bd.params  = make.bd.params()
# traits     = make.traits(BM.process, add = discrete_trait)
# modifiers = NULL
# events = NULL
# null.error = FALSE
# check.results = TRUE
# save.steps = NULL


# conditional.process()


})