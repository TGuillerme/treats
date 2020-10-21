context("plot.dads")

## Test
test_that("plot.dads works for traits", {
    
    list_of_traits <- make.traits(process = c(no.process, BM.process, OU.process), trait.names = c("No process (normal)", "Brownian motion", "Ornstein-Uhlenbeck"))
    ## Right output
    expect_null(plot.dads(list_of_traits))
    expect_null(plot.dads(list_of_traits, trait = 2))
    expect_null(plot.dads(list_of_traits, trait = 3))
})
