library(dispRity)

## Test
test_that("simulating trees works", {

    ## Pure birth trees
    bd.params <- list(speciation = 1,
                      extinction = 0)
    stop.rule <- list(max.living = 10,
                      max.taxa   = Inf,
                      max.time   = Inf)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)
    expect_equal(Nnode(test), 9)
    ## All tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 10)

    stop.rule$max.living = Inf
    stop.rule$max.taxa   = 11
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 11)
    expect_equal(Nnode(test), 10)
    ## All tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 11)

    set.seed(1)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = Inf
    stop.rule$max.time   = 4
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    ## All tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), Ntip(test))
    ## The tree age is 4
    expect_equal(test$root.time, 4)
    expect_equal(max(tree.age(test)$age), 4)

    ## Birth death trees
    bd.params <- list(speciation = 1,
                      extinction = 0.2)
    stop.rule$max.living = 10
    stop.rule$max.taxa   = Inf
    stop.rule$max.time   = Inf
    set.seed(3)
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 14)
    expect_equal(Nnode(test), 13)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 10)

    set.seed(3)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = 10
    stop.rule$max.time   = Inf
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 10)
    expect_equal(Nnode(test), 9)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 7)

    set.seed(3)
    stop.rule$max.living = Inf
    stop.rule$max.taxa   = Inf
    stop.rule$max.time   = 6
    test <- birth.death.tree.traits(bd.params, stop.rule)$tree
    expect_is(test, "phylo")
    expect_equal(Ntip(test), 137)
    expect_equal(Nnode(test), 136)
    ## Not all tips are living
    expect_equal(length(which(tree.age(test)$age == 0)), 107)
    ## The tree age is 6
    expect_equal(test$root.time, 6)
    expect_equal(max(tree.age(test)$age), 6)
})

test_that("simulating trees + traits works", {

    ## Some processes to test
    true.answer <- function(x0, ...) {
        return(42)
    }
    expect_equal(true.answer(x0 = 0), 42)

    element.rank <- function(x0, ...) {
        return(x0 + 1)
    }
    expect_equal(element.rank(x0 = 0), 1)

    branch.length <- function(x0, edge.length, ...) {
        return(edge.length)
    }
    expect_equal(branch.length(x0 = 2, edge.length = 0.1), 0.1)

    element.depth <- function(x0, edge.length, ...) {
        return(x0 + edge.length)
    }
    expect_equal(element.depth(x0 = 2, edge.length = 0.1), 2.1)

    ## Sim.element.trait internals work (for one trait)
    one_trait <- list(
        trait_id = 1,
        process  = true.answer)
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 42)
    one_trait$process <- element.rank
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 3)
    one_trait$process <- branch.length
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 0.1)
    one_trait$process <- element.depth
    expect_equal(sim.element.trait(one_trait, parent.trait = 2, edge.length = 0.1), 2.1)

    element_rank_10 <- list(trait_id = 1, process = element.rank, start = 10)
    traits_list <- list("A" = element_rank_10)
    bd.params <- list(speciation = 1, extinction = 0.5)
    stop.rule <- list(max.living = Inf,
                      max.taxa   = 10,
                      max.time   = Inf)
    set.seed(1)
    test <- birth.death.tree.traits(bd.params, traits = traits_list, stop.rule = stop.rule)
    expect_is(test, "list")
    expect_equal(names(test), c("tree", "data"))
    expect_is(test[[1]], "phylo")
    expect_is(test[[2]], c("matrix", "array"))
    expect_equal(nrow(test[[2]]), Ntip(test$tree) + Nnode(test$tree))


    ## Visual checking
    stop.rule <- list(max.living = 20,
                      max.taxa   = Inf,
                      max.time   = Inf)
    set.seed(1)
    traits_list$A$start <- 10
    test <- birth.death.tree.traits(bd.params, traits = traits_list, stop.rule)
    ## Right dimensions
    expect_equal(dim(test$data), c(Ntip(test$tree) + Nnode(test$tree), 1))
    expect_equal(length(which(test$data == max(test$data))), 2)
    # ## Visual checking
    # tree_plot <- test$tree
    # tree_plot$edge.length <- rep(1, Nedge(tree_plot))
    # plot(tree_plot)
    # nodelabels(paste(test$tree$node.label, sep = ":", test$data[test$tree$node.label,1]), cex = 0.5)
    # tiplabels(paste(test$tree$tip.label, sep = ":", test$data[test$tree$tip.label,1]), cex = 0.5)

    stop.rule <- list(max.living = Inf,
                      max.taxa   = 10,
                      max.time   = Inf)
    set.seed(10)
    traits_list$A$process <- branch.length
    traits_list$B <- traits_list$A
    traits_list$C <- traits_list$A
    test <- birth.death.tree.traits(bd.params, traits = traits_list, stop.rule)

    ## The three traits are equal
    expect_equal(test$data[,1], test$data[,2])
    expect_equal(test$data[,2], test$data[,3])
    expect_equal(test$data[,1], test$data[,3])

    ## The first trait value is 10
    expect_equal(test$data[1,1], 10)

    ## The traits are equal to edge lengths
    expect_equal(unname(round(sort(test$data[-1,1]), 5)),
                 round(sort(test$tree$edge.length), 5))

    # ## Visual checking
    # tree_plot <- test$tree
    # plot(tree_plot)
    # nodelabels(paste(test$tree$node.label, sep = ":", round(test$data[test$tree$node.label,1], 2)), cex = 1)
    # tiplabels(paste(test$tree$tip.label, sep = ":", round(test$data[test$tree$tip.label,1], 2)), cex = 1)
    # edgelabels(round(test$tree$edge.length, 2))


    ## Complex traits
    complex_traits <- list(
                "A" = list(trait_id = 1:3,
                           process  = element.rank,
                           start    = c(0,10,20)),
                "B" = list(trait_id = 4,
                           process  = branch.length,
                           start    = 0)
                )
    set.seed(1)
    test <- birth.death.tree.traits(bd.params, traits = complex_traits, stop.rule)

    expect_equal(test$data[,1], test$data[,2] - 10)
    expect_equal(test$data[,1], test$data[,3] - 20)
    expect_equal(unname(test$data[,4]), c(0, test$tree$edge.length))


    ## Multidimensional brownian trait
    complex_traits <- list(
                "A" = list(trait_id = 1:3,
                           process  = BM.process,
                           start    = c(0,0,0)),
                "B" = list(trait_id = 4,
                           process  = branch.length,
                           start    = 0)
                )
    set.seed(1)
    test <- birth.death.tree.traits(bd.params, traits = complex_traits, stop.rule)
    expect_equal(dim(test$data), c(19, 4))
    expect_equal(unname(test$data[,4]), c(0, test$tree$edge.length))
})

# test_that("simulating trees + traits + modifiers work", {

#     ## Test with longer waiting time for traits with positive values


#     ## Test with extinction for traits with negative values


#     ## Test with longer average waiting time for traits with negative values

  
#     ## Test with average more extinction for traits with positive values


#     ## Visual tests

#     ## Setting up all parameters


# })

test_that("events work", {

    stop.rule <- list(max.time = 5, max.taxa = Inf, max.living = Inf)
    traits <- NULL
    modifiers <- NULL
    bd.params <- list(extinction = 0, speciation = 1)

    ## Taxa events
## Mass extinction at time t
time.condition <- function(bd.params, lineage, trait.values, time) {
    return(time > 4)
}
## Function for extinction at 08
extinction.08 <- function(bd.params, lineage, trait.values) {
        
        ## Set the variable
        extinction_strength <- 0.8

        ## Select a portion of the living species to go extinct
        extinct <- sample(lineage$n, round(lineage$n * extinction_strength))

        ## Update the lineage object
        lineage$livings <- lineage$livings[-extinct]
        lineage$n       <- lineage$n - length(extinct)
        return(lineage)
}
    
    ## Make a dummy events object
    events <- list(
        ## A triggering tracker (most events can only be triggered once)
        trigger      = 0L,
        ## A function that intakes bd.params, lineage, traits
        condition    = time.condition,
        ## A character string that is either taxa, bd.params, traits or modifiers
        target       = "taxa",
        modification = extinction.08)

    set.seed(1)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = NULL, modifiers = NULL, events = events)
    # plot(test$tree) ; axisPhylo()
    ## 165 taxa generated
    expect_equal(Ntip(test$tree), 165)
    ## 57 extinct
    expect_equal(sum(dispRity::tree.age(test$tree)$age[1:165] > 0), 102)
    ## Only two ages for tips (0 or 0.998)
    expect_equal(unique(dispRity::tree.age(test$tree)$age[1:115]), c(0.998, 0))


    ## Mass extinction based on trait values at time t
## Function for extinction trait
extinction.trait <- function(bd.params, lineage, trait.values) {
    ## Set the variable and the selector
    trait_limit <- 1 ; selector <- `<`

    ## Select the nodes be traits
    parent_traits <- parent.traits(trait.values, lineage, current = FALSE)
    selected_nodes <- as.numeric(names(which(selector(parent_traits[, 1], trait_limit))))

    ## Select the descendants that'll go extinct
    extinct <- which(lineage$parents %in% selected_nodes)

    ## Update the lineage object
    lineage$livings <- lineage$livings[!lineage$livings %in% extinct]
    lineage$n       <- length(lineage$livings)
    return(lineage)
}

    ## Make a dummy events object
    events <- list(
        trigger      = 0L,
        condition    = time.condition,
        target       = "taxa",
        modification = extinction.trait)

    set.seed(7)
    test <- birth.death.tree.traits(bd.params = bd.params, stop.rule = stop.rule, traits = make.traits(), modifiers = NULL, events = events)
    class(test) <- c("dads")
    plot(test)
    ## 244 taxa generated
    expect_equal(Ntip(test$tree), 244)
    ## 57 extinct
    expect_equal(sum(dispRity::tree.age(test$tree)$age[1:244] > 0), 89)
    ## Only two ages for tips
    expect_equal(unique(dispRity::tree.age(test$tree)$age[1:244]), c(0.974, 0))
    ## Trait values for living and extinct is different
    living <- test$data[test$tree$tip.label[dispRity::tree.age(test$tree)$age[1:244] == 0], ]
    extinct <- test$data[test$tree$tip.label[dispRity::tree.age(test$tree)$age[1:244] == 0.974], ]
    expect_equal(round(mean(living), 6), 1.929087)
    expect_equal(round(mean(extinct), 7), -0.5327465)


    ## bd.params events
    ## Adding extinction after reaching n taxa

    ## Reducing speciation after reaching time t




    ## traits events
    ## Changing a trait process after time t

    ## Changing a trait argument (e.g. sigma) when a trait reaches value x



    ## modifiers events
    ## Adding a speciation condition after reaching time t

    ## Adding a branch length condition when reaching n taxa

    ## Changing the condition of a modifier after reaching trait value x

    ## Changing the modify of a modifier after reaching time t





    ## founding events
    ## Events that generate a new process (founding effects)


})