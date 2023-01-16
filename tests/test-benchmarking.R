## Benchmarking with RPANDA and TreeSim

library(TreeSim)


## Simulate 3 birth death tree to reach age = 2 
age<-3
lambda <- 2.0
mu <- 0.5
frac <-0.6
numbsim<-20
##
# Simulating trees with time age since the origin:
safe.sim <- function(fun, args) {
    out <- list(0)
    null.error <- 0
    while(!is(out[[1]], "phylo") && null.error < 50) {
        out <- do.call(fun, args)
        null.error <- null.error + 1
    }
    return(out)
}

set.seed(1)

test_age_2_fail <- microbenchmark(
    treeSim = sim.bd.age(age = 2, lambda = lambda, mu = mu, numbsim = 1),
    dads = dads(stop.rule = list(max.time = 2), bd.params = list(speciation = lambda, extinction = mu), null.error = TRUE))
test_age_3_fail <- microbenchmark(
    treeSim = sim.bd.age(age = 3, lambda = lambda, mu = mu, numbsim = 1),
    dads = dads(stop.rule = list(max.time = 3), bd.params = list(speciation = lambda, extinction = mu), null.error = TRUE))
test_age_4_fail <- microbenchmark(
    treeSim = sim.bd.age(age = 4, lambda = lambda, mu = mu, numbsim = 1),
    dads = dads(stop.rule = list(max.time = 4), bd.params = list(speciation = lambda, extinction = mu), null.error = TRUE))
test_age_5_fail <- microbenchmark(
    treeSim = sim.bd.age(age = 5, lambda = lambda, mu = mu, numbsim = 1),
    dads = dads(stop.rule = list(max.time = 5), bd.params = list(speciation = lambda, extinction = mu), null.error = TRUE))

n<-10
lambda <- 2.0
mu <- 0.5
frac <-0.6
numbsim<-1
##
# Simulating numbsim trees with n species under a birth-death process with
# speciation rate lambda an extinction rate mu:
sim.bd.taxa(n,numbsim,lambda,mu)


test_taxa_10_fail <- microbenchmark(
    treeSim = sim.bd.taxa(n = 10, lambda = lambda, mu = mu, numbsim = 1),
    dads = dads(stop.rule = list(max.living = 10), bd.params = list(speciation = lambda, extinction = mu), null.error = TRUE))
test_taxa_50_fail <- microbenchmark(
    treeSim = sim.bd.taxa(n = 50, lambda = lambda, mu = mu, numbsim = 1),
    dads = dads(stop.rule = list(max.living = 50), bd.params = list(speciation = lambda, extinction = mu), null.error = TRUE))
test_taxa_150_fail <- microbenchmark(
    treeSim = sim.bd.taxa(n = 150, lambda = lambda, mu = mu, numbsim = 1),
    dads = dads(stop.rule = list(max.living = 150), bd.params = list(speciation = lambda, extinction = mu), null.error = TRUE))
test_taxa_500_fail <- microbenchmark(
    treeSim = sim.bd.taxa(n = 500, lambda = lambda, mu = mu, numbsim = 1),
    dads = dads(stop.rule = list(max.living = 500), bd.params = list(speciation = lambda, extinction = mu), null.error = TRUE))


## On average, TreeSim is 2-3x faster





n -> 10
numbsim<-1
# Simulating trees with a fixed number of species having shifts in rate
# and mass extinction events.
# Between today and time 0.3 in the past, we have speciation rate 2,
# extinction rate 0. At time 0.3, we have a mass extinction event which
# 10% of the species survive. Prior to 0.3, we have a speciation rate
# of 1 and an extinction rate of 0.3:
sim.rateshift.taxa(n,numbsim, c(2,1), c(0,0.3), c(1,0.1), c(0,0.3),complete=TRUE)


















# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12526
install.packages("RPANDA")
library(RPANDA)

?sim_sgd