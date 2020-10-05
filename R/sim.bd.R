# ####
# # Inputs
# ####

# ## Trait controller
# traits <- list(
#     n = 1,
#     start = 0,
#     cor = 0)



# ####
# # Birth death modifiers
# ####

# ## The model for a modifier
# modifier = list(
#             # What to apply the modifier on (extinction, speciation or waiting time)
#             what = "extinction",
#             # What is the condition (<, <=, >=, ==, >)
#             condition = `<=`,
#             # What is the value for the condition (a numeric)
#             value = 0,
#             # The modifier to multiply the probability with
#             multiplier = 1
#     )






# ## Function for sampling the waiting time
# wait.time <- function(event_probability) {
#     rexp(1, event_probability)
# }

# ## Sampling modifier function
# use.modifier <- function(fun, args, modifier, trait) {
#     if(modifier$condition(trait, modifier$value)) {
#         return(fun(args) * modifier$multiplier)
#     } else {
#         return(fun(args))
#     }
# }

# ## Example for waiting time
# modifier <- list(what = "wait.time",
#                  condition = `<=`,
#                  value = 0,
#                  multiplier = 1)
# use.modifier(wait.time, event_probability, modifier, trait = 5)

# ## Example for speciation
# modifier <- list(what = "speciation",
#                  condition = `<=`,
#                  value = 0,
#                  multiplier = 1)
# speciation = 1
# extinction = 0
# use.modifier(fun = runif, args = 1, modifier = modifier, trait = 5) < (speciation/(speciation + extinction))
# # if TRUE, go speciate

# ## Example for extinction
# modifier <- list(what = "extinction",
#                  condition = `<=`,
#                  value = 0,
#                  multiplier = 1)
# speciation = 1
# extinction = 0.5
# use.modifier(fun = runif, args = 1, modifier = modifier, trait = 5) < (speciation/(speciation + extinction))
# # make sure this modifies the extinction probability




# ####
# # Event modifiers (e.g. make some mass extinction or change the trait correlation matrix after some time)
# ####

# ## One event
# event <- list(
#     when = c("taxa" = 0, "living" = 0, "time" = 0),
#     what = c("traits", "living", "taxa"),
#     event = function()
#     )

# ## Example of events
# events <- list(
#     "mass.extinction" = list(when = c("living" = 10),
#                              what = c("living"),
#                              event = list(fun = extinction, args = list(severity = 90))),
#     "trait.cor"       = list(when = c("time" = 5, "taxa" = 5),
#                              what = "traits",
#                              event = list(fun = function(t))) 
#     )













birth.death.tree.traits <- function(speciation, extinction, traits, stop.rule = list()) {
  
    ## Set up the stop rules
    stop.rule$max.taxa <- ifelse(is.null(stop.rule$max.taxa), Inf, stop.rule$max.taxa)
    stop.rule$max.live <- ifelse(is.null(stop.rule$max.live), Inf, stop.rule$max.live)
    stop.rule$max.time <- ifelse(is.null(stop.rule$max.time), Inf, stop.rule$max.time)

    ## Initialising the values
    parent <- edge_lengths <- time <- 0
    n_living_taxa <- lineages <- 1
    is_split <- FALSE
    # trait_start <- rep(0, traits$n)

    # trait_values <- matrix(mvrnorm(n = traits$n, mu = rep(traits$start, traits$n), Sigma = waiting.time * bm.rates), ncol = traits$n)

    ## Building the tree
    while(n_living_taxa > 0 && n_living_taxa <= stop.rule$max.live && sum(!is_split) <= stop.rule$max.taxa) {
        
        ## Get the probability of something happening
        event_probability <- sum(n_living_taxa * (speciation + extinction))
        ## Get the waiting time
        waiting_time <- rexp(1, event_probability)

        ## Update the global time (for the first waiting time)
        if(stop.rule$max.time != Inf && time == 0) {
            stop.rule$max.time <- stop.rule$max.time + waiting_time
        }
        #TODO: optimise this bit to be out of the while loop! (having a function in the while loop is longer)

        ## Update the global time
        time <- time + waiting_time

        ## Arrived at the max time (cut the branch lengths)
        if(time > stop.rule$max.time) {
            waiting_time <- waiting_time - (time - stop.rule$max.time)
            edge_lengths[lineages] <- edge_lengths[lineages] + waiting_time
            time <- stop.rule$max.time
            break
        }

        ## Updating branch length
        edge_lengths[lineages] <- edge_lengths[lineages] + waiting_time

        ## Pick a lineage for the event to happen to:
        selected_lineage <- sample(n_living_taxa, 1)
        lineage <- lineages[selected_lineage]

        ## Simulating a trait (placeholder)
        trait_values <- rbind(trait_values, matrix(mvrnorm(n = traits$n, mu = rep(traits$start, traits$n), Sigma = waiting.time * bm.rates), ncol = traits$n))

        trait <- 0
        modifier <- 0

        ## Randomly triggering an event
        if((modifier + runif(1)) < (speciation/(speciation + extinction))) {
            
            ## Speciating:
            if(n_living_taxa == stop.rule$max.live) {
                ## Don't add this one
                break
            }

            ## Creating the new lineages
            new_lineage <- length(is_split) + 1:2
            is_split[lineage] <- TRUE
            is_split[new_lineage] <- FALSE
            parent[new_lineage] <- lineage
            edge_lengths[new_lineage] <- 0
            n_living_taxa <- n_living_taxa + 1

            lineages <- c(lineages[-selected_lineage], new_lineage)

        } else {

            ## Go extinct
            lineages <- lineages[-selected_lineage]
            n_living_taxa <- n_living_taxa - 1
        }
    }   
    ## Summarise into a table (minus the initiation)
    table <- data.frame(vertex       = seq_along(is_split),
                        parent       = parent,
                        edge_lengths = edge_lengths,
                        is_split     = is_split)[-1, ]

    ## Error
    if(nrow(table) == 0) {
        stop("No tree generated with these parameters.")
    }


    ## Remove 0 edge split from max.taxa rule
    if(sum(!table$is_split) > stop.rule$max.taxa) {
        ## Remove the 0 edge split
        last_parent <- table$parent[nrow(table)]
        ## Remove the two last edges
        table <- table[-c(nrow(table), nrow(table)-1),]
        ## Change the node into a tip
        table$is_split[table$vertex == last_parent] <- FALSE
    } 

    ## Number of rows and tips
    Nnode  <- sum(!table$is_split) - 1
    n_tips <- sum(!table$is_split)

    ## Getting the edge table node/tips IDs
    table$vertex2 <- NA
    table$vertex2[!table$is_split] <- 1:n_tips
    table$vertex2[ table$is_split] <- order(table$vertex[table$is_split]) + n_tips + 1

    ## Getting the edge table nodes (left column)
    left_edges <- match(table$parent, table$vertex)
    table$parent2 <- table$vertex2[left_edges]
    table$parent2[is.na(table$parent2)] <- n_tips + 1

    ## Getting the tips and nodes labels
    tree <- list(edge        = cbind(table$parent2, table$vertex2),
                 Nnode       = Nnode,
                 tip.label   = paste0("t", 1:n_tips),
                 node.label  = paste0("n", 1:Nnode),
                 edge.length = table$edge_lengths)
    class(tree) <- "phylo"
    ## Adding the root time
    tree$root.time <- max(dispRity::tree.age(tree)$age)

    ## Output
    return(list(tree = tree, traits = matrix(NA)))
}



## This function is for updating one (or more) traits in the process
update.trait <- function(parent_edge, process) {
    ## Use the branch length to update the trait using the process function
}


## Make into a phylo object
convert.to.phylo <- function(bd_table) {
  ## Things in the table that are only in the right column get a tip name
  ## Things in the table that don't have a tip name get a node name
}
