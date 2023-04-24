## This is a quick script to simulate some trees with various birth-death parameters and some traits with OU or BM processes

## Installing the development version of treats
devtools::install_github("TGuillerme/treats")
library(treats)


####################
##
## One simulation
## 
####################

## ------------
## Setting the tree generation parameters
## ------------
## Setting the birth death parameters to to uniform distributions with a joint estimation (speciation > extinction)
bd_params <- make.bd.params(speciation = runif,
                            extinction = runif,
                            joint      = TRUE)

## Setting the stopping rule at 100 living species (the simulation will stop when reaching 100 living species)
stop_rule <- list(max.living = 100)

## Setting the trait parameters
trait_process <- make.traits(process = BM.process)

## -----------
## Generating the trees and data
## -----------
## Generating a random tree with 100 living species
my_data <- treats(bd.params  = bd_params,
                stop.rule  = stop_rule,
                traits     = trait_process,
                null.error = 100) # the null.error parameter will loop through the parameters up to 100 times before returning an error (i.e. a failed tree)
## Visualising the results
plot(my_data)

## ------------
## Keeping only the living species
## ------------
my_data <- drop.fossil.treats(my_data)
plot(my_data)

## ------------
## Extracting the data
## ------------
generated_tree <- my_data$tree # class is phylo
generated_data <- my_data$data # class is matrix

####################
##
## Optional tips
## 
####################

## ------------
## Changing the trait process
## ------------
## You can change the trait process to an OU one to generate data with less trait variance by changing the trait process to:
trait_process <- make.traits(process = OU.process)
## And then running the simulations again (note that there are many more options for changing the trait process if you check ?make.traits)

## ------------
## Running multiple simulations
## ------------
## You can wrap all that into a replicate loop to generate a list of trees (here to generate 5 simulations)
all_data <- replicate(5, treats(bd.params  = bd_params,
                              stop.rule  = stop_rule,
                              traits     = trait_process,
                              null.error = 100), simplify = FALSE)
## Extracting all trees
all_trees <- lapply(all_data, function(x) return(x$tree))
all_traits <- lapply(all_data, function(x) return(x$data))