## Test
test_that("treats works for simple birth-deaths", {

    ## Naked birth death (just the tree)

    ## Sanitizing
    error <- capture_error(treats(bd.params = "hehe", stop.rule = list(max.taxa = 10)))
    expect_equal(error[[1]], "bd.params must be a named list of arguments or a \"treats\" \"bd.params\" object. You can use make.bd.params() to format the object correctly.")
    error <- capture_error(treats(bd.params = list(1, 2), stop.rule = list(max.taxa = 10)))
    expect_equal(error[[1]], "bd.params must be a named list of arguments or a \"treats\" \"bd.params\" object. You can use make.bd.params() to format the object correctly.")

    error <- capture_error(treats())
    expect_equal(error[[1]], "You must provide at least one stopping rule. For example:\nstop.rule <- list(max.taxa   = 10,\n                  max.living = 10,\n                  max.time   = 10)")
    error <- capture_error(treats(stop.rule = "ha"))
    expect_equal(error[[1]], "stop.rule must be of class list.")
    error <- capture_error(treats(stop.rule = list(1,2)))
    expect_equal(error[[1]], "stop.rule must be a named list of stopping rules. For example:\nstop.rule <- list(max.taxa   = 10,\n                  max.living = 10,\n                  max.time   = 10)")
    error <- capture_error(treats(stop.rule = list(something = 2)))
    expect_equal(error[[1]], "You must provide at least one stopping rule. For example:\nstop.rule <- list(max.taxa   = 10,\n                  max.living = 10,\n                  max.time   = 10)")

    ## Default birth.death
    set.seed(22)
    test <- treats(stop.rule = list("max.living" = 10))
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)

    set.seed(1)
    test <- treats(bd.params = list(extinction = 0.2), stop.rule = list("max.taxa" = 10))
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)



    ## Works with null.error set to a logical or an integer
    # set.seed(1)
    # test <- treats(bd.params = list(speciation = 1, extinction = 0.5), stop.rule = list("max.living" = 10), null.error = TRUE)
    # expect_null(test)
    set.seed(2)
    error <- capture_error(treats(bd.params = list(speciation = 1, extinction = 0.5), stop.rule = list("max.living" = 10)))
    expect_equal(error[[1]], "No tree generated with these parameters.")
    set.seed(123)
    prints <- capture_messages(test <- treats(bd.params = list(speciation = 1, extinction = 0.9), stop.rule = list("max.living" = 10), null.error = 50))
    expect_is(test, "phylo")
    expect_equal(length(prints), 6)

    ## Long sim warning
    messages <- capture_messages(treats(stop.rule = list(max.time = 6.1)))
    expect_equal(messages[[1]], "Stop rule was step higher than 6 time units.\nThis can take a considerable amount of time and RAM to simulate if no other stop rules are given.\n")
})

test_that("treats works for trees + traits", {

    ## Tree + traits

    ## Sanitizing
    error <- capture_error(treats(stop.rule = list(max.taxa = 10), traits = 1))
    expect_equal(error[[1]], "traits must be of class \"treats\" \"traits\". Use make.traits() to format the object correctly.")
    

    ## Simple birth.death tree with one BM trait
    test <- treats(stop.rule = list("max.living" = 10), traits = make.traits())
    expect_is(test, "treats")
    expect_equal(names(test), c("tree", "data", "bd.params", "traits"))
    expect_is(test$tree, "phylo")
    expect_equal(dim(test$data), c(19, 1))
    
    ## A more complex trait
    complex_traits <- make.traits(process = c(BM.process, BM.process), n = c(2,3), process.args = list(list(Sigma = diag(2)), list(Sigma = matrix(1/3, 3, 3))), trait.names = c("bib", "bob"))
    test <- treats(stop.rule = list("max.living" = 10), traits = complex_traits)
    expect_is(test, "treats")
    expect_equal(names(test), c("tree", "data", "bd.params", "traits"))
    expect_is(test$tree, "phylo")
    expect_equal(dim(test$data), c(19, 5))
})


#test_that("treats works for trees + modifiers", {
#})

#test_that("treats works for trees + events", {
#})

#test_that("treats works for trees + traits + modifiers + events", {
#})






# ## Vanity: some speed checks

# set.seed(1)
# start <- Sys.time()
# test <- treats(bd.params = list(extinction = 1/3), stop.rule = list(max.taxa = 1000))
# end <- Sys.time()
# end-start
# # 1000 taxa (no traits)
# # Time difference of 0.1825261 secs

# start <- Sys.time()
# test <- treats(bd.params = list(extinction = 1/3), stop.rule = list(max.taxa = 10000))
# end <- Sys.time()
# end-start
# # # 10000 taxa (no traits)
# # # 30.21827 secs

# start <- Sys.time()
# test <- treats(bd.params = list(extinction = 1/3), stop.rule = list(max.taxa = 1000), traits = make.traits())
# end <- Sys.time()
# end-start
# # 1000 taxa (1 traits)
# # Time difference of 1.056556 secs

# start <- Sys.time()
# test <- treats(bd.params = list(extinction = 1/3), stop.rule = list(max.taxa = 10000), traits = make.traits())
# end <- Sys.time()
# end-start
# # 10000 taxa (1 traits)
# # Time difference of 37.54625 secs


# complex_traits <- make.traits(process = c(BM.process, OU.process, BM.process), n = c(6,3,1))
# start <- Sys.time()
# test <- treats(bd.params = list(extinction = 1/3), stop.rule = list(max.taxa = 1000), traits = complex_traits)
# end <- Sys.time()
# end-start
# # 1000 taxa (1 traits)
# # Time difference of 1.674716 secs

# start <- Sys.time()
# test <- treats(bd.params = list(extinction = 1/3), stop.rule = list(max.taxa = 10000), traits = complex_traits)
# end <- Sys.time()
# end-start
# # 10000 taxa (10 traits)
# # Time difference of 46.44996 secs