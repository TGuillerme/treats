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


test_that("example works", {
    ## Specifying a trait process
    my_trait <- make.traits()
    ## Plotting a trait process
    expect_null(plot(my_trait, main = "A Brownian Motion"))

    ## Simulating a tree with ten taxa
    my_tree <- dads(stop.rule = list(max.taxa = 10))
    ## Plotting a simple birth death tree (using ape::plot.phylo)
    expect_is(plot(my_tree, main = "A pure birth tree"), "list")

    ## Simulating a tree with traits
    my_data <- dads(stop.rule = list(max.taxa = 10),
                    traits    = my_trait)
    ## Plotting the tree and traits
    expect_null(plot(my_data))

    ## Specifying a 3D trait process
    my_3D_trait <- make.traits(n = 3)
    ## Simulating a birth death tree with that trait
    my_data <- dads(bd.params = list(extinction = 0.2),
                    stop.rule = list(max.living = 50),
                    traits    = my_3D_trait)

    ## Plotting the second trait and the tree (default)
    ## The colours are purple for nodes and blue for tips
    plot(my_data, trait = 2, col = c("purple", "blue"),
         edges = "pink", tips.nodes = "black")

    ## Plotting the first and third trait correlation
    ## The colours are a heat map based on the elements age
    plot(my_data, trait = c(1, 3), col = terrain.colors,
         edges = "grey", tips.nodes = "black")
    
    ## Plotting the first and third trait correlation in 3D
    plot(my_data, trait = c(1,3), col = rainbow,
         edges = "grey", tips.nodes = "black", use.3D = TRUE)

    ## Plotting all traits in 3D (without branch lengths)
    plot(my_data, trait = c(1:3), col = heat.colors,
         edges = "grey", tips.nodes = "black", use.3D = TRUE, type = "s", radius = 0.1)
})