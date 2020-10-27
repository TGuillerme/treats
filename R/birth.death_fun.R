
# ####
# # Event modifiers (e.g. make some mass extinction or change the trait correlation matrix after some time)
# ####

# ## One event
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













## Simulating traits for one element
sim.element.trait <- function(one.trait, parent.trait, edge.length) {
    ## Set the simulation arguments
    trait_args <- one.trait
    ## Remove the process and the n argument
    trait_args$process  <- NULL
    trait_args$trait_id <- NULL
    trait_args$start <- NULL
    ## Add the x0 (last step) + the edge length
    trait_args$x0 <- parent.trait[one.trait$trait_id]
    trait_args$edge.length <- edge.length
    return(do.call(one.trait$process, trait_args))
}

## Simulates one set of traits for the living species
sim.living.tips <- function(living, trait_table, traits) {
    return(unlist(
          lapply(traits,
                 sim.element.trait,
                 parent.trait = trait_table[which(trait_table[, "element"] == trait_table[living, "parent"]), -c(1:3), drop = FALSE],
                 edge.length  = trait_table[living, "edge"]
                 )
          )
    )
}

# stop("DEBUG birth.death_fun.R")
# bd.params <- list(speciation = 1, extinction = 1/3)
# traits <- make.traits()
# stop.rule <- list(max.taxa = 20, max.living = Inf, max.time = Inf)
# modifiers <- NULL
# events <- NULL
# null.error <- NULL
# # Add the potential for dispersal (the information will be another (distance?) matrix)

waiting.time <- function(bd.params, n_living_taxa, parent.trait) {

    ## Get the event probability
    event_probability <- sum(n_living_taxa * (bd.params$speciation + bd.params$extinction))

    ## Get the waiting time
    waiting_time <- rexp(1, event_probability)

    ## modifier placeholder
    modifier_placeholder <- NULL

    return(waiting_time)
}

trigger.speciation <- function(bd.params, parent.trait) {

    trigger_event <- runif(1)

    ## Speciate?
    do_speciate <- trigger_event < (bd.params$speciation/(bd.params$speciation + bd.params$extinction))

    ## modifier placeholder
    modifier_placeholder <- NULL
    
    return(do_speciate)
}



# waiting.time.modified <- function(bd.params, n_living_taxa, parent.trait) {

#     ## Get the event probability
#     event_probability <- sum(n_living_taxa * (bd.params$speciation + bd.params$extinction))

#     ## Get the waiting time
#     waiting_time <- rexp(1, event_probability)

#     ## Set condition
#     condition_placeholder <- NULL

#     return(waiting_time)
# }

# trigger.speciation.modified <- function(bd.params, parent.trait) {

#     trigger_event <- runif(1)

#     ## Speciate?
#     do_speciate <- trigger_event < (bd.params$speciation/(bd.params$speciation + bd.params$extinction))

#     ## modifier placeholder
#     modifier_placeholder <- NULL

#     return(do_speciate)
# }



# modifiers <- list("waiting"    = waiting.time,
#                   "speciating" = trigger.speciation.modified)

# set.seed(5)
# test <- birth.death.tree.traits(bd.params, stop.rule, traits, modifiers = modifiers)
# par(mfrow = c(2,1))
# plot(test$tree)
# class(test) <- "dads"
# plot.dads(test)

## Run a birth death process to generate both tree and traits
birth.death.tree.traits <- function(bd.params, stop.rule, traits = NULL, modifiers = NULL, events = NULL, null.error = FALSE) {
  
    ############
    ## Initialising
    ############

    ## Set up the traits, modifiers and events simulation
    do_traits    <- ifelse(is.null(traits), FALSE, TRUE)
    do_events    <- ifelse(is.null(events), FALSE, TRUE)

    ## Make the initial modifier (no modifier)
    initial.modifiers <- list("waiting"    = waiting.time,
                              "speciating" = trigger.speciation)
    ## Set the modifiers if null (no modifier)
    if(is.null(modifiers)) {
        modifiers <- initial.modifiers
    }

    ## Initialising the values
    parent <- edge_lengths <- 0
    is_split <- FALSE
    time <- 0
    n_living_taxa <- lineages <- 1
    parent.trait <- NULL

    ############
    ## First node (root)
    ############
    
    ## Get the waiting time
    waiting_time <- initial.modifiers$waiting(bd.params, n_living_taxa, parent.trait)

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
        trait_values <- rbind(NULL, "root" = c("element" =  1, unlist(lapply(traits, function(x) return(x$start)))))
    } else {
        trait_table <- NULL
    }

    ## Randomly triggering an event
    if(initial.modifiers$speciating(bd.params, parent.trait)) {
        ## Speciating:
        if(n_living_taxa == stop.rule$max.living ) {
            ## Don't add this one
            if(!null.error) {
                stop("No tree generated with these parameters.")
            } else {
                return(NULL)
            }
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
        ## Cannot go further
        if(!null.error) {
            stop("No tree generated with these parameters.")
        } else {
            return(NULL)
        }
    }

    ############
    ## Rest of the tree
    ############

    ## Build the rest of the tree
    while(n_living_taxa > 0 && n_living_taxa <= stop.rule$max.living  && sum(!is_split) <= stop.rule$max.taxa) {
        
        ## Pick a lineage for the event to happen to:
        selected_lineage <- sample(n_living_taxa, 1)
        lineage <- lineages[selected_lineage]

        ## Get the waiting time
        waiting_time <- modifiers$waiting(bd.params, n_living_taxa, parent.trait = parent_trait <- trait_values[which(trait_values[,1] == parent[lineage]), -1, drop = FALSE])

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

        ## Adding a new row to the trait_values matrix
        if(do_traits) {
            trait_values <- rbind(trait_values,
                                  ## Creating the new row composed of the lineage ID
                                  c(lineage,
                                    ## And the updated trait from the parent lineage
                                    unlist(lapply(traits,
                                                  sim.element.trait,
                                                  parent.trait = trait_values[which(trait_values[,1] == parent[lineage]), -1],
                                                  edge.length  = edge_lengths[lineage]))
                                    )
                                  )
        }

        ## Randomly triggering an event
        if(modifiers$speciating(bd.params, parent.trait = parent_trait <- trait_values[which(trait_values[,1] == parent[lineage]), -1, drop = FALSE])) {
            
            ## Speciating:
            if(n_living_taxa == stop.rule$max.living ) {
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

    ############
    ## Cleaning the results
    ############

    ## Summarise into a table (minus the initiation)
    table <- data.frame(parent       = parent, # These are nodes
                        element      = seq_along(is_split), # These are tips or nodes
                        edge_lengths = edge_lengths,
                        is_split     = is_split)[-1, ]

    ## Error
    if(nrow(table) == 0) {
        if(!null.error) {
            stop("No tree generated with these parameters.")
        } else {
            return(NULL)
        }
    }

    ## Remove 0 edge split from max.taxa rule
    if(sum(!table$is_split) > stop.rule$max.taxa) {
        ## Remove the 0 edge split
        last_parent <- table$parent[nrow(table)]
        ## Remove the two last edges
        table <- table[-c(nrow(table), nrow(table)-1),]
        ## Change the node into a tip
        table$is_split[table$element == last_parent] <- FALSE
    } 

    ## Adding the traits to the table
    if(do_traits) {
        trait_table <- cbind(parent = table$parent, element = table$element, edge = table$edge_lengths, trait_values[match(table$element, trait_values[, "element"]), -1])
        ## Add the root value
        trait_table <- rbind(c(parent = 0, element = 1, edge = 0, trait_values[1, -1]),
                             trait_table)
        ## Find the living tips
        living_tips <- which(is.na(trait_table[-1, 4]) & !table$is_split) + 1
        ## Simulate the traits for the living tips and add them to the trait table
        trait_table[living_tips, -c(1:3)] <- t(sapply(living_tips, sim.living.tips, trait_table, traits, simplify = TRUE))
    }

    ############
    ## Creating the tree object
    ############

    ## Number of rows and tips
    n_nodes <- sum(!table$is_split) - 1
    n_tips  <- sum(!table$is_split)

    ## Getting the edge table node/tips IDs
    table$element2 <- NA
    table$element2[!table$is_split] <- 1:n_tips
    table$element2[ table$is_split] <- order(table$element[table$is_split]) + n_tips + 1

    ## Getting the edge table nodes (left column)
    left_edges <- match(table$parent, table$element)
    table$parent2 <- table$element2[left_edges]
    table$parent2[is.na(table$parent2)] <- n_tips + 1

    ## Getting the tips and nodes labels
    tree <- list(edge        = cbind(table$parent2, table$element2),
                 Nnode       = n_nodes,
                 tip.label   = paste0("t", 1:n_tips),
                 node.label  = paste0("n", 1:n_nodes),
                 edge.length = table$edge_lengths)
    class(tree) <- "phylo"
    ## Adding the root time
    tree$root.time <- max(dispRity::tree.age(tree)$age)

    ## Renaming the trait_values
    if(do_traits) {
        trait_table <- trait_table[, -c(1:3), drop = FALSE]
        rownames(trait_table) <- c(tree$tip.label, tree$node.label)[c(n_tips+1, table$element2)]
        ## Add the column names (if missing)
        if(length(missing_names <- which(colnames(trait_table) == "")) > 0) {
            colnames(trait_table)[missing_names] <- names(traits)
        }
    }

    ## Output
    return(list(tree = tree, data = trait_table))
}

