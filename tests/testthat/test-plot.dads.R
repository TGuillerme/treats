context("plot.dads")

test_that("plot.dads works for traits", {
    
    list_of_traits <- make.traits(process = c(no.process, BM.process, OU.process), trait.names = c("No process (normal)", "Brownian motion", "Ornstein-Uhlenbeck"))
    ## Right output
    expect_null(plot.dads(list_of_traits))
    expect_null(plot.dads(list_of_traits, trait = 2))
    expect_null(plot.dads(list_of_traits, trait = 3))
})

test_that("plot.dads works for dads", {
    
    ## Simple example    
    test <- dads(stop.rule = list("max.living" = 10), traits = make.traits())
    ## Default plot
    expect_null(plot(test))
    ## Other options
    expect_null(plot(test, main = "what?", xlim = c(0, test$tree$root.time), col = c(tips = "pink", nodes = "red", edges = "purple"), ylab = "data"))

    ## A more complex trait
    complex_traits <- make.traits(process = c(BM.process, BM.process), n = c(2,3), process.args = list(list(Sigma = diag(2)), list(Sigma = matrix(1/3, 3, 3))), trait.names = c("bib", "bob"))
    test <- dads(stop.rule = list("max.living" = 10), traits = complex_traits)
    expect_null(plot(test, trait = 3))

    ## A big tree
    set.seed(12)
    big_test <- dads(bd.params = list(speciation = 1, extinction = 1/3), stop.rule = list(max.taxa = 1000), traits = make.traits())
    expect_null(plot(big_test, cex = 0.5))






    list_of_traits <- make.traits(process = c(no.process, BM.process, OU.process), trait.names = c("No process (normal)", "Brownian motion", "Ornstein-Uhlenbeck"))
    ## Right output
    expect_null(plot.dads(list_of_traits))
    expect_null(plot.dads(list_of_traits, trait = 2))
    expect_null(plot.dads(list_of_traits, trait = 3))
})
