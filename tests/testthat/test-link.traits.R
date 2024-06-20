
test_that("link.traits works", {

    ## First let's design a discrete islandness trait
    transition_matrix <- matrix(c(3, 0.2, 0.05, 3), 2, 2)

    ## The firs trait (upon which to check the condition)
    discrete_trait <- make.traits(discrete.process, process.args = list(transitions = transition_matrix), trait.name = "conditional.trait:A")

    ## The second and third traits
    OU_trait <- make.traits(OU.process, n = 2)
    BM_trait <- make.traits(BM.process, n = 2)

    link.type = "conditional"
    link.args = list("choose.OU" = function(x1) {x1 == 0}, "choose.BM" = function(x1) {x1 == 1}) 









    # stop.rule  = list(max.taxa = 100)
    #     stop.rule$max.living = Inf
    #     stop.rule$max.time = Inf
    # bd.params  = make.bd.params()
    # traits     = make.traits(BM.process, add = discrete_trait, trait.name = "A")
    # traits     = make.traits(OU.process, add = traits, trait.name = "A")
    # modifiers = NULL
    # events = NULL
    # null.error = FALSE
    # check.results = TRUE
    # save.steps = NULL




# ### Rename the trait "conditional.trait" internally
# # "conditional.trait:A" (both process lists must have the same name)

# ## Structure for main is a list with
#     #conditional.trait:A: contains the discrete process
#     #A: contains the list of two processes (GIVE THEM THE SAME ID! $trait_id)
#         #each element in A contains a condition test function

# ## Testing format


# ## First let's design a discrete islandness trait
# transition_matrix <- matrix(c(3, 0.2, 0.05, 3), 2, 2)
# discrete_trait <- make.traits(discrete.process, process.args = list(transitions = transition_matrix))
# ## The two traits to trigger depending on the condition
# BM_trait <- make.traits(BM.process)
# constant.process <- function(x0 = 0, edge.length = 1) {
#     return(42)
# }
# no_trait <- make.traits(process = constant.process)


# ## conditional.process is empty
# expect_equal(conditional.process(), "conditional.process")

# ## But it works with make.traits
# test <- make.traits(process = conditional.process,
#                     process.args = list(conditional.trait = discrete_trait,
#                                         conditions = list(function(x1) x1 == 0,
#                                                           function(x1) x1 == 1),
#                                         processes  = list(BM_trait, no_trait)),
#                     trait.names = "tust")

# expect_is(test, c("treats", "traits"))
# expect_equal(names(test), c("main", "background"))
# expect_equal(names(test$main), c("conditional.trait:tust", "tust"))
# ## The conditional
# expect_equal(names(test$main$`conditional.trait:tust`), c("process", "start", "trait_id"))
# expect_equal(test$main$`conditional.trait:tust`$trait_id, 1)

# ## The conditioned
# expect_equal(length(test$main$tust), 2)
# expect_equal(names(test$main$tust[[1]]), c("process", "start", "trait_id", "condition.test"))
# expect_equal(test$main$tust[[1]]$trait_id, 2)
# expect_equal(names(test$main$tust[[2]]), c("process", "start", "trait_id", "condition.test"))
# expect_equal(test$main$tust[[2]]$trait_id, 2)



# traits_tmp <- list()
# traits_tmp$`conditional.trait:A` <- traits$main[[1]]
# traits_tmp$A <- list(traits$main[[2]], traits$main[[3]])
# traits_tmp$A[[1]]$condition.test <- function(x1) {x1 == 0}
# traits_tmp$A[[2]]$condition.test <- function(x1) {x1 == 1}
# expect_length(traits_tmp, 2)
# expect_equal(names(traits_tmp), c("conditional.trait:A", "A"))
# traits <- traits_tmp 



## TODO test
# multiple conditionals
# works with save steps
# works with background


})