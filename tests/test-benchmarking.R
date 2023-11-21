# # ## Benchmarking with RPANDA and TreeSim

# library(TreeSim)
# library(diversitree)
# library(RPANDA)

# ## Function for repeating the simulation until a result appears
# safe.sim <- function(fun, args) {
#     out <- list(0)
#     null.error <- 0
#     while(!is(out[[1]], "phylo") && null.error < 100) {
#         cat(".")
#         out <- do.call(fun, args)
#         null.error <- null.error + 1
#     }
#     return(out)
# }

# sim_ClaDS_tree <- function(...) {
#     return(sim_ClaDS(...)$tree)
# }

# ###########
# # Testing simulating a tree
# ###########

# set.seed(1)
# ## Testing parameters
# age <- 3
# lambda <- 2.0
# mu <- 0.5
# numbsim <- 1

# params_sim.bd.age <- list(age = age, lambda = lambda, mu = mu, numbsim = 1, mrca = FALSE)
# params_tree.bd <- list(pars = c(lambda, mu), max.taxa = Inf, max.t = age, include.extinct = TRUE)
# params_sim_ClaDS <- list(lambda_0 = lambda, mu_0 = mu, condition = "time", time_stop = age, prune_extinct = FALSE)
# params_treats <- list(stop.rule = list(max.time = age), bd.params = list(speciation = lambda, extinction = mu), null.error = 100, verbose = FALSE)

# test_age <- microbenchmark(times = 5L,
#     TreeSim = safe.sim(sim.bd.age, params_sim.bd.age),
#     diversitree = safe.sim(tree.bd, params_tree.bd),
#     RPANDA = safe.sim(sim_ClaDS_tree, params_sim_ClaDS),
#     treats = safe.sim(treats, params_treats)
# )




# tree.bd(pars, max.taxa=Inf, max.t=time, include.extinct=FALSE)

# ###########
# # Testing simulating a tree with an extinction event
# ###########


# ###########
# # Testing simulating a tree and traits
# ###########



# ###########
# # Testing simulating a tree and traits with extinction event
# ###########




# plot(test_age, ylab = "microseconds", xlab = "")



# # Simulating trees with a fixed number of species having shifts in rate
# # and mass extinction events.
# # Between today and time 0.3 in the past, we have speciation rate 2,
# # extinction rate 0. At time 0.3, we have a mass extinction event which
# # 10% of the species survive. Prior to 0.3, we have a speciation rate
# # of 1 and an extinction rate of 0.3:
# sim.rateshift.taxa(n,numbsim,c(2,1),c(0,0.3), c(1,0.1),c(0,0.3),complete=TRUE)
# # The fraction 0.6 of the extant species is included into the final tree
# # (the tree has n species AFTER sampling, extinct and
# # non-sampled lineages are not included):
# sim.rateshift.taxa(n,numbsim,c(2,1),c(0,0.3), c(0.6,0.1),c(0,0.3),complete=FALSE)



# # n -> 10
# # numbsim<-1
# # # Simulating trees with a fixed number of species having shifts in rate
# # # and mass extinction events.
# # # Between today and time 0.3 in the past, we have speciation rate 2,
# # # extinction rate 0. At time 0.3, we have a mass extinction event which
# # # 10% of the species survive. Prior to 0.3, we have a speciation rate
# # # of 1 and an extinction rate of 0.3:
# # sim.rateshift.taxa(n,numbsim, c(2,1), c(0,0.3), c(1,0.1), c(0,0.3),complete=TRUE)



# # # https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12526
# # install.packages("RPANDA")
# # library(RPANDA)

# # ?sim_sgd