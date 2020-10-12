


# # ####
# # # Birth death modifiers
# # ####

# ## Add the potential for dispersal (the information will be another (distance?) matrix)

# # ## The model for a modifier
# # modifier = list(
# #             # What to apply the modifier on (extinction, speciation or waiting time)
# #             what = "speciation",
# #             # What is the condition (<, <=, >=, ==, >)
# #             condition = `<`, # Distance from other traits or something like that
# #             # What is the value for the condition (a numeric)
# #             value = 0,
# #             # The modifier to multiply the probability with
# #             multiplier = 2 # Make this into a function (potentially)
# #     )






# # ## Function for sampling the waiting time
# # wait.time <- function(event_probability) {
# #     rexp(1, event_probability)
# # }

# # ## Sampling modifier function
# # use.modifier <- function(fun, args, modifier, trait) {
# #     if(modifier$condition(trait, modifier$value)) {
# #         return(fun(args) * modifier$multiplier)
# #     } else {
# #         return(fun(args))
# #     }
# # }

# # ## Example for waiting time
# # modifier <- list(what = "wait.time",
# #                  condition = `<=`,
# #                  value = 0,
# #                  multiplier = 1)
# # use.modifier(wait.time, event_probability, modifier, trait = 5)

# # ## Example for speciation
# # modifier <- list(what = "speciation",
# #                  condition = `<=`,
# #                  value = 0,
# #                  multiplier = 1)
# # speciation = 1
# # extinction = 0
# # use.modifier(fun = runif, args = 1, modifier = modifier, trait = 5) < (speciation/(speciation + extinction))
# # # if TRUE, go speciate

# # ## Example for extinction
# # modifier <- list(what = "extinction",
# #                  condition = `<=`,
# #                  value = 0,
# #                  multiplier = 1)
# # speciation = 1
# # extinction = 0.5
# # use.modifier(fun = runif, args = 1, modifier = modifier, trait = 5) < (speciation/(speciation + extinction))
# # # make sure this modifies the extinction probability




# ####
# # Event modifiers (e.g. make some mass extinction or change the trait correlation matrix after some time)
# ####

# ## One event
# event <- list(
#     when = c("taxa" = 0, "living" = 0, "time" = 0), # Distance
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

# ## Mass extinctions:
# bd.event <- list(
#     when = c("taxa" = 0, "living" = 0, "time" = 0),
#     what = c("traits", "living", "taxa"),
#     event = function()
#     )
# #TG: for this one, remove a proportion or number of taxa when the event occurs. It is possible to randomly remove the taxa or remove them as function of their trait value. 

# ## Change in the traits:
# trait.event <- list(
#     when = ...,
#     what = ...,
#     event = ...)
# #TG: for this one, just change traits$process or traits$cor when the event occurs












# ####
# # Inputs
# ####

# ## Trait controller
traits <- list(
    n = 1,
    start = 0,
    process = function(x0) return(x0 + 1),
    cor = diag(1, 1, 1))
    
# speciation = 1
# extinction = 0

# stop.rule <- list(max.taxa = 10)


birth.death.tree.traits <- function(speciation, extinction, traits = NULL, stop.rule = list()) {
  
    ############
    ## Initialising
    ############

    ## Set up the stop rules
    stop.rule$max.taxa <- ifelse(is.null(stop.rule$max.taxa), Inf, stop.rule$max.taxa)
    stop.rule$max.live <- ifelse(is.null(stop.rule$max.live), Inf, stop.rule$max.live)
    stop.rule$max.time <- ifelse(is.null(stop.rule$max.time), Inf, stop.rule$max.time)

    ## Set up the trait simulation
    do_traits <- !is.null(traits)

    ## Initialising the values
    parent <- edge_lengths <- time <- 0
    n_living_taxa <- lineages <- 1
    DEBUG_node_counter <- 0
    DEBUG_tip_counter <- 0
    is_split <- FALSE

    warning("DEBUG: TODO 1")
    ## If stop.rule$max.taxa exist, initiate the values to vectors (of length Nnode + Ntip)


    ############
    ## First node (root)
    ############

    ## Get the probability of something happening
    event_probability <- sum(n_living_taxa * (speciation + extinction))
    ## Get the waiting time
    waiting_time <- rexp(1, event_probability)
    ## Update the global time (for the first waiting time)
    if(stop.rule$max.time != Inf && time == 0) {
        stop.rule$max.time <- stop.rule$max.time + waiting_time
    }
    ## Update the global time
    time <- time + waiting_time
    ## Updating branch length
    edge_lengths[lineages] <- edge_lengths[lineages] + waiting_time

    ## Select the first lineage
    selected_lineage <- lineage <- 1

    ## Start the trait    
    if(do_traits) {
        trait_values <- cbind(parent = 1, matrix(traits$start, nrow = traits$n))
    } else {
        trait_values <- NA
    }

    # cat(paste0("node = ", lineage, "; trait = ", trait_values[nrow(trait_values), 2], "\n"))
    # cat(paste0("    parent ", parent[lineage], " trait = ", trait_values[which(trait_values[,1] == parent[lineage]), ][2],  "\n"))

    modifier <- 0

    ## Randomly triggering an event
    if((modifier + runif(1)) < (speciation/(speciation + extinction))) {
        ## Speciating:
        if(n_living_taxa == stop.rule$max.live) {
            ## Don't add this one
            stop("No tree generated with these parameters.")
        }

        ## Creating the new lineages
        new_lineage <- length(is_split) + 1:2
        is_split[lineage] <- TRUE
        is_split[new_lineage] <- FALSE
        parent[new_lineage] <- lineage
        edge_lengths[new_lineage] <- 0
        n_living_taxa <- n_living_taxa + 1
        lineages <- c(lineages[-selected_lineage], new_lineage)
        # cat(paste0(" lineage ", new_lineage[1], " (from ", parent[new_lineage[1]], ") + "))
        # cat(paste0(new_lineage[2], " (from ", parent[new_lineage[2]], ")\n"))
        # cat("\n")

        DEBUG_node_counter <- DEBUG_node_counter + 1
        cat(paste0("lineage ", lineage, " (edge n", DEBUG_node_counter, ")",  "; trait = ", trait_values[nrow(trait_values), 2], "\n"))
        cat(paste0("    parent ", parent[lineage], " trait = ", trait_values[which(trait_values[,1] == parent[lineage]), ][2],  "\n"))

    } else {
        ## Cannot go further
        stop("No tree generated with these parameters.")
    }

    ############
    ## Rest of the tree
    ############

    ## Build the rest of the tree
    while(n_living_taxa > 0 && n_living_taxa <= stop.rule$max.live && sum(!is_split) <= stop.rule$max.taxa) {
        
        ## Get the probability of something happening
        event_probability <- sum(n_living_taxa * (speciation + extinction))
        ## Get the waiting time
        waiting_time <- rexp(1, event_probability)

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
        
        ## Adding a new row to the trait_values matrix
        trait_values <- rbind(trait_values,
                              ## Creating the new row composed of the lineage ID
                              cbind(lineage,
                                    ## And the updated trait from the parent lineage
                                    trait_values[which(trait_values[,1] == parent[lineage]), -1] + 1
                                    )
                              )

        # cat(paste0("node = ", lineage, "; trait = ", trait_values[nrow(trait_values), 2], "\n"))
        # cat(paste0("    parent ", parent[lineage], " trait = ", trait_values[which(trait_values[,1] == parent[lineage]), ][2],  "\n"))

        modifier <- 0

        ## Randomly triggering an event
        if((modifier + runif(1)) < (speciation/(speciation + extinction))) {
            
            # cat(paste0("    speciation:"))

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

            # cat(paste0(" lineage ", new_lineage[1], " (from ", parent[new_lineage[1]], ") + "))
            # cat(paste0(new_lineage[2], " (from ", parent[new_lineage[2]], ")\n"))
            # cat("\n")
        
            DEBUG_node_counter <- DEBUG_node_counter + 1
            cat(paste0("lineage ", lineage, " (edge n", DEBUG_node_counter, ")",  "; trait = ", trait_values[nrow(trait_values), 2], "\n"))
            cat(paste0("    parent ", parent[lineage], " trait = ", trait_values[which(trait_values[,1] == parent[lineage]), ][2],  "\n"))

        } else {
            # cat(paste0("    extinction.\n\n"))
            ## Go extinct
            lineages <- lineages[-selected_lineage]
            n_living_taxa <- n_living_taxa - 1

            DEBUG_tip_counter <- DEBUG_tip_counter + 1
            cat(paste0("lineage ", lineage, " (edge t", DEBUG_tip_counter, " - tip)",  "; trait = ", trait_values[nrow(trait_values), 2], "\n"))
            cat(paste0("    parent ", parent[lineage], " trait = ", trait_values[which(trait_values[,1] == parent[lineage]), ][2],  "\n"))

        }
    }


    ############
    ## Cleaning the results
    ############


    ## Summarise into a table (minus the initiation)
    table <- data.frame(parent       = parent, # These are nodes
                        vertex       = seq_along(is_split), # These are tips or nodes
                        edge_lengths = edge_lengths,
                        is_split     = is_split)[-1, ]


    ## Add the trait_values to the table
    table$parent_trait <- trait_values[match(table$parent, trait_values[, "parent"]), 2]
    
    warning("DEBUG: TODO 2")
    ## Add the trait_values of the vertex to the table
    # table$vertex_trait <- trait_values[match(table$vertex, trait_values[, "parent"]), 2]


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

    ############
    ## Creating the tree object
    ############

    ## Number of rows and tips
    n_nodes <- sum(!table$is_split) - 1
    n_tips  <- sum(!table$is_split)

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
                 Nnode       = n_nodes,
                 tip.label   = paste0("t", 1:n_tips),
                 node.label  = paste0("n", 1:n_nodes),
                 edge.length = table$edge_lengths)
    class(tree) <- "phylo"
    ## Adding the root time
    tree$root.time <- max(dispRity::tree.age(tree)$age)

    ## Output
    return(list(tree = tree, traits = trait_values))
}


traits <- list(
    n = 1,
    start = 0,
    process = function(x0) return(x0 + 1),
    cor = diag(1, 1, 1))
speciation = 1
extinction = 0.5
stop.rule = list(max.taxa = 10)

set.seed(1)
test <- birth.death.tree.traits(speciation = 1, extinction = 0.5, traits = traits, stop.rule = list(max.taxa = 10))
tree <- test$tree
tree$edge.length
plot(tree)
nodelabels(tree$node.label)
test$traits


## This function is for updating one (or more) traits in the process
update.trait <- function(parent_edge, process) {
    ## Use the branch length to update the trait using the process function
}


## Make into a phylo object
convert.to.phylo <- function(bd_table) {
  ## Things in the table that are only in the right column get a tip name
  ## Things in the table that don't have a tip name get a node name
}
