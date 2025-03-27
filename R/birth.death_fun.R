## Simulating traits for one element
sim.element.trait <- function(one.trait, parent.trait, edge.length) {
    ## Simulate conditional if exist
    if(!is.null(one.trait$link)) {
        switch(one.trait$link$type, {
            "conditional" = sim.link <- sim.link.conditional
        }) 
        return(sim.link(one.trait, parent.trait, edge.length))
    }

    ## Set the simulation arguments
    trait_args <- one.trait
    if(!is.null(trait_args$process.args)) {
        trait_args <- trait_args$process.args[[1]]    
    }
    ## Remove unusablee arguments
    trait_args$process <- NULL
    trait_args$trait_id <- NULL
    trait_args$start <- NULL
    ## Add the x0 (last step) + the edge length
    trait_args$x0 <- parent.trait[one.trait$trait_id]
    trait_args$edge.length <- edge.length
    return(do.call(one.trait$process[[1]], trait_args))
}
multi.sim.element.trait <- function(one.trait, parent.traits, edge.lengths, select = NULL) {
    ## Select the trait (if needed)
    if(!is.null(select)) {
        one.trait <- one.trait[[select]]
    }
    ## Run all the traits
    return(do.call(rbind, lapply(as.list(1:dim(parent.traits)[1]), function(X, parent.traits, edge.lengths, one.trait) sim.element.trait(one.trait, parent.traits[X, , drop = FALSE], edge.lengths[X]), parent.traits, edge.lengths, one.trait)))
}
## Sim link traits
sim.link.conditional <- function(one.trait, parent.trait, edge.length) {
    select.trait.process <- function(one.trait, select) {
        selected_process <- one.trait
        selected_process$link <- NULL
        selected_process$process <- selected_process$process[select]
        if(!is.null(selected_process$process.args)) {
            selected_process$process.args <- selected_process$process.args[select]
            if(is.null(names(selected_process$process.args[[1]]))) {
                selected_process$process.args <- NULL
            }
        }
        return(selected_process)
    }

    ## Simulate the first trait
    first_trait <- select.trait.process(one.trait, select = 1)
    first_trait$trait_id <- one.trait$trait_id[1] 
    first_trait_val <- sim.element.trait(first_trait, parent.trait = parent.trait, edge.length = edge.length)
    
    ## Choose which trait
    selected_trait <- which(unlist(lapply(one.trait$link$conditional.test, function(x, x1) x(x1), x1 = first_trait_val)))[1]

    ## Select the second trait
    second_trait <- select.trait.process(one.trait, select = selected_trait+1)
    second_trait$trait_id <- second_trait$trait_id[-1]
    other_trait_val <- sim.element.trait(second_trait, parent.trait = parent.trait, edge.length = edge.length)
    return(c(first_trait_val, other_trait_val))
}

add.trait.value <- function(trait_values, traits, lineage, edge_lengths, type = "one_node") {

    ## Simulation selector
    sim.fun <- switch(type,
                     "one_node" = sim.element.trait,
                     "all_node" = multi.sim.element.trait)
    ## Parent traits selector
    parent_traits <- switch(type,
                            "one_node" = parent.traits(trait_values, lineage),
                            "all_node" = trait_values[match(lineage$parents[lineage$parents[lineage$livings]], rownames(trait_values)), , drop = FALSE])
    ## Edges selector
    edges <- switch(type,
                    "one_node" = edge_lengths[lineage$current],
                    "all_node" = edge_lengths[lineage$parents[lineage$livings]])

    ## Simulate the new trait values in batch
    new_trait_values <- lapply(traits, sim.fun, parent.trait = parent_traits, edge.length = edges)

    ## Output
    if(type == "one_node") {
        return(rbind(trait_values, unlist(new_trait_values), deparse.level = 0))
    }
    if(type == "all_node") {
        new_trait_values <- do.call(cbind, new_trait_values)
        rownames(new_trait_values) <- lineage$parents[lineage$livings]
        return(rbind(trait_values, new_trait_values))
    }   
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
## Check the events triggering (the first one gets triggered)
trigger.events <- function(one_event, bd.params, lineage, trait.values, time) {
    return(one_event$condition(bd.params = bd.params, lineage = lineage, trait.values = trait.values, time = time) && one_event$trigger < 1L)
}
## Creates a snapshot (creates a snapshot of the data (lineage or/and traits) at a specific time): single updates
bd.update.single.nodes <- function(lineage) {
    ## Creating the new lineage
    new_lineage <- lineage
    ## Adding the living species as new parents
    new_lineage$parents <- c(lineage$parents, lineage$livings)
    ## Get the last lineage generated
    last_lineage <- length(lineage$split)
    ## Updating the new species (descending of the parents)
    new_lineage$livings <- (last_lineage+1):(last_lineage+lineage$n)
    ## If drawn is missing, resample the drawn lineage so that it can't be a fossil
    if(is.na(lineage$livings[lineage$drawn])) {
        lineage$drawn <- new_lineage$drawn <- sample(1:length(new_lineage$livings), 1)
    }
    ## Re-select the sampled lineage
    new_lineage$current <- new_lineage$livings[lineage$drawn]
    ## The new nodes lead to non-splitting branches
    new_lineage$split   <- c(lineage$split, rep(FALSE, length(lineage$livings)))
    ## The old ones are now splitting (but not in two)
    new_lineage$split[lineage$livings] <- TRUE
    ## Done
    return(new_lineage)
}
bd.update.single.edges <- function(time, time.slice, lineage, edge_lengths) {
    ## Calculate the difference at the split time
    diff <- time - time.slice
    ## Updating the edge lengths
    edge_lengths_out <- edge_lengths
    ## Removing the time diff to the living lineages
    edge_lengths_out[lineage$parents[lineage$livings]] <- edge_lengths_out[lineage$parents[lineage$livings]] - diff
    ## Adding the time diff to the new branches
    edge_lengths_out[lineage$livings] <- diff
    return(edge_lengths_out)
}

## Run a birth death process to generate both tree and traits
birth.death.tree.traits <- function(stop.rule, bd.params, traits = NULL, modifiers = NULL, events = NULL, null.error = FALSE, check.results = TRUE, save.steps = NULL) {

 # bd.debug <- function(seed) {

    ############
    ## Initialising
    #############
    # warning("DEBUG in birth.death_fun.R::birth.death.tree.traits: update trait extinction")
    # bd.params <- make.bd.params(speciation = 1, extinction = 0)
    # stop.rule <- list(max.living = Inf, max.time = Inf, max.taxa = 100)
    # traits <- make.traits(process = BM.process)
    # constant.brlen <- function() {
    #     return(as.numeric(1))
    # }
    # ## Creating the modifiers object
    # select.last <- function(lineage) {
    #     return(as.integer(lineage$n))
    # }
    # constant_modifier <- make.modifiers(branch.length = constant.brlen, selection = select.last)
    # modifiers <- NULL
    # select.scale.to.absolute.trait.value <- function(trait.values, lineage) {
    #     return(as.integer(sample(lineage$livings, size = 1, prob = abs(trait.values[as.character(lineage$parents[lineage$livings]), ])+1 )))
    # }
    # modifiers <- make.modifiers(selection = select.scale.to.absolute.trait.value)
    # events <- make.events(target = "taxa",
    #                       condition = age.condition(1),
    #                       modification = trait.extinction(0))

    # null.error <- FALSE
    # check.results <- TRUE
    # save.steps = NULL
 
    # warning("DEBUG: bith.death_fun.R")
    # modifiers  = make.modifiers()
    # traits     = trait0
    # bd.params  = bd.params
    # stop.rule  = stop.rule
    # stop.rule$max.living = Inf
    # stop.rule$max.taxa = Inf
    # events     = events
    # save.steps = NULL
    # null.error = FALSE
    # check.results = TRUE
    # # seed = 1 ; warning("FUCKING INTERNAL SEED!")
    # set.seed(1)

    ## Set up the traits, modifiers and events simulation
    do_traits    <- ifelse(is.null(traits), FALSE, TRUE)
    do_events    <- ifelse(is.null(events), FALSE, TRUE)
    ## If do_events make empty founding trees lists
    founding_roots <- founding_trees <- founding_tree_root_ages <- list()

    ## Make the initial modifier (no modifier)
    initial.modifiers <- list("waiting"    = list(fun = branch.length.fast,
                                                  internal = NULL),
                              "selecting"  = list(fun = selection.fast,
                                                  internal = NULL),
                              "speciating" = list(fun = speciation.fast,
                                                  internal = NULL))
    ## Set the modifiers if null (no modifier)
    if(is.null(modifiers)) {
        modifiers <- initial.modifiers
    }

    ## Initialising the values
    time <- edge_lengths <- 0
    was_alive <- 0L # recording which lineage go extinct (when extinction happens). 0L is no extinction.
    founding_tree <- NULL
    current.save.steps <- save.steps

    ## Initialise the lineage tracker
    lineage <- list("parents" = 0L,   # The list of parent lineages
                    "livings" = 1L,   # The list of lineages still not extinct
                    "drawn"   = 1L,   # The lineage ID drawn (selected)
                    "current" = 1L,   # The current focal lineage
                    "n"       = 1L,   # The number of non extinct lineages
                    "split"   = FALSE)# The topology tracker (sum(!lineage$split) is total number of tips)

    ############
    ## First node (root)
    ############

    ## Get the waiting time
    first_waiting_time <- initial.modifiers$waiting$fun(
                            bd.params    = sample.from(bd.params),
                            lineage      = lineage,
                            trait.values = NULL,
                            modify.fun   = NULL)

    ### DEBUG: set starting time to 0
    # warning("DEBUG birth.death_fun.R") ; first_waiting_time <- 0

    ## Update the global time (for the first waiting time)
    if(stop.rule$max.time != Inf && time == 0) {
        stop.rule$max.time <- stop.rule$max.time + first_waiting_time
    }

    ## Update the global time
    time <- time + first_waiting_time
    ## Updating branch length
    edge_lengths[lineage$living] <- edge_lengths[lineage$living] + first_waiting_time

    ## Start the trait    
    if(do_traits) {

        trait_values <- rbind(NULL, c(unlist(lapply(traits$main, function(x) return(x$start)))))
        rownames(trait_values) <- 1
        
    } else {
        trait_table <- NULL
    }

    ## Randomly triggering an event
    if(initial.modifiers$speciating$fun(bd.params    = sample.from(bd.params),
                                        lineage      = NULL,
                                        trait.values = NULL,
                                        modify.fun   = NULL))
    {
        ## Speciating:
        if(lineage$n == stop.rule$max.living) {
            ## Don't add this one
            if(!null.error) {
                stop("No tree generated with these parameters.")
            } else {
                return(NULL)
            }
        }

        ## Creating the new lineages
        new_lineage <- length(lineage$split) + 1:2
        lineage$split[lineage$current] <- TRUE
        lineage$split[new_lineage] <- FALSE
        lineage$parents[new_lineage] <- lineage$current
        edge_lengths[new_lineage] <- 0
        lineage$n <- lineage$n + 1L
        lineage$livings <- c(lineage$livings[-lineage$drawn], new_lineage)
        was_alive <- 0L
    } else {
        ## Cannot go further
        if(!null.error) {
            stop("No tree generated with these parameters.")
        } else {
            return(NULL)
        }
    }
    # ##DEBUG: creating right handed trees
    # warning("DEBUG birth.death_fun.R") ; lineage$current <- 3 ; lineage$drawn   <- 2
    # warning("DEBUG"); lineage_base <- lineage; trait_values_base <- trait_values; edge_lengths_base <- edge_lengths
    # lineage <- lineage_base ; trait_values <- trait_values_base; edge_lengths <- edge_lengths_base
    ############
    ## Rest of the tree
    ############
    # warning("DEBUG birth.death_fun.R") ; counter <- 0
    # warning("DEBUG birth.death_fun.R") ; set.seed(8)
    # warning("DEBUG birth.death_fun.R") ; record <- c("start recording events:\n")
    # warning("DEBUG birth.death_fun.R")
    # step_counter <- 0
    # record_everything <- list()
    # step_counter <- step_counter + 1; record_everything[[step_counter]] <- list(lineage = lineage, edge_lengths = edge_lengths, trait_values = trait_values)

    ## Build the rest of the tree
    while(lineage$n > 0 && lineage$n <= stop.rule$max.living && sum(!lineage$split) <= stop.rule$max.taxa) {
        
        ## Pick a lineage for the event to happen to:
        lineage$drawn <- modifiers$selecting$fun(
                            bd.params    = sample.from(bd.params),
                            lineage      = lineage,
                            trait.values = trait_values,
                            modify.fun   = modifiers$selecting$internal)
        lineage$current <- lineage$livings[lineage$drawn]

        ## Get the waiting time
        waiting_time <- modifiers$waiting$fun(
                            bd.params    = sample.from(bd.params),
                            lineage      = lineage,
                            trait.values = trait_values,
                            modify.fun   = modifiers$waiting$internal)

        ## Update the global time
        time <- time + waiting_time

        ## Arrived at the max time (cut the branch lengths)
        if(time > stop.rule$max.time) {
            waiting_time <- waiting_time - (time - stop.rule$max.time)
            edge_lengths[lineage$livings] <- edge_lengths[lineage$livings] + waiting_time
            time <- stop.rule$max.time
            break
        }

        ## Updating branch length
        edge_lengths[lineage$livings] <- edge_lengths[lineage$livings] + waiting_time

        ## Saving steps during the tree growing
        if(!is.null(save.steps)) {

            while(!is.na(current.save.steps[1]) && first_waiting_time + current.save.steps[1] <= time) {
                ## Creating a time slice
                time.slice <- first_waiting_time + current.save.steps[1]
                
                ## Save a step by creating singles
                lineage      <- bd.update.single.nodes(lineage)
                edge_lengths <- bd.update.single.edges(time, time.slice, lineage, edge_lengths)
                if(do_traits) {
                    trait_values <- add.trait.value(trait_values, traits$main, lineage, edge_lengths, type = "all_node")
                }

                ## Updating the saving.steps
                if(length(save.steps) == 1) {
                    current.save.steps <- current.save.steps + save.steps
                } else {
                    current.save.steps <- current.save.steps[-1]
                }
            }
        }

        ## Adding a new row to the trait_values matrix
        if(do_traits) {
            # warning("DEBUG birth.death_fun.R") ; step_counter <- step_counter + 1; record_everything[[step_counter]] <- list(lineage = lineage, edge_lengths = edge_lengths, trait_values = trait_values)


            if(!is.null(traits$background)) {

                # time.slice <- first_waiting_time + time
                ## Update the lineage and create generate background traits
                lineage      <- bd.update.single.nodes(lineage)
                edge_lengths <- bd.update.single.edges(time, time.slice = time, lineage, edge_lengths)
                trait_values <- add.trait.value(trait_values, traits$background$main, lineage, edge_lengths, type = "all_node")

                ## Redo the current node
                trait_values <- add.trait.value(trait_values, traits$main, lineage, edge_lengths, type = "one_node")
                rownames(trait_values)[rownames(trait_values) == ""] <- lineage$current
                ## Duplicate the trait value for the ancestor of the current node?
                trait_values[lineage$parent[lineage$livings[lineage$drawn]], ] <- trait_values[nrow(trait_values),]

            } else {
                ## Simulate trait values for current node only
                trait_values <- add.trait.value(trait_values, traits$main, lineage, edge_lengths, type = "one_node")
                rownames(trait_values)[rownames(trait_values) == ""] <- lineage$current
            }
        }

        # warning("DEBUG birth.death_fun.R") ; step_counter <- step_counter + 1; record_everything[[step_counter]] <- list(lineage = lineage, edge_lengths = edge_lengths, trait_values = trait_values)

        ## Randomly triggering an event
        if(modifiers$speciating$fun(bd.params    = sample.from(bd.params),
                                    lineage      = lineage,
                                    trait.values = trait_values,
                                    modify.fun   = modifiers$speciating$internal))
        {
            ## Speciating:
            if(lineage$n == stop.rule$max.living) {
                ## Don't add this one
                break
            }

            ## Creating the new lineages
            new_lineage <- length(lineage$split) + 1:2
            lineage$split[lineage$current] <- TRUE
            lineage$split[new_lineage] <- FALSE
            lineage$parents[new_lineage] <- lineage$current
            edge_lengths[new_lineage] <- 0
            lineage$n <- lineage$n + 1L
            lineage$livings <- c(lineage$livings[-lineage$drawn], new_lineage)
            was_alive <- 0L
        } else {
            ## Go extinct
            was_alive <- lineage$livings[lineage$drawn]
            lineage$livings <- lineage$livings[-lineage$drawn]
            lineage$n <- lineage$n - 1L
        }

        # internal.plot.lineage(lineage, edge_lengths) ; warning("DEBUG")
        # warning("DEBUG"); reached_event <- TRUE

        ## Trigger events
        if(do_events) {
            ## Check all the events that can be triggered
            triggers <- unlist(lapply(events, trigger.events,
                                    bd.params    = sample.from(bd.params),
                                    lineage      = lineage,
                                    trait.values = trait_values,
                                    time         = time - first_waiting_time))

            if(any(triggers) && length(lineage$livings) != 0) {

                # warning("DEBUG birth.death_fun: event triggered"); break
                # lineage_bkp <- lineage ; edge_lengths_bkp <- edge_lengths ; trait_values_bkp <- trait_values
                # lineage_bkp -> lineage ; edge_lengths_bkp -> edge_lengths ; trait_values_bkp -> trait_values

                # internal.plot.lineage(lineage, edge_lengths)

                ## Selecting the first triggerable event
                selected_event <- which(triggers)[1]

                # if(selected_event == 2) {
                #     lineage_bkp <- lineage
                #     edge_lengths_bkp <- edge_lengths
                #     warning("DEBUG SECOND EVENT") ; break
                #     lineage <- lineage_bkp
                #     edge_lengths <- edge_lengths_bkp
                #     internal.plot.lineage(lineage, edge_lengths)
                # }

                ## Shift current selection (if current is a fossil)
                if(!is.na(lineage$livings[lineage$drawn]) && lineage$livings[lineage$drawn] != lineage$current) {
                    lineage$current <- lineage$livings[lineage$drawn]
                }

                ## Create trait snapshot at the time of the event
                time.slice   <- time#-first_waiting_time #TG: Check here if time-first_waiting_time is needed OR, if time condition, the proper extinction time is needed
                lineage      <- bd.update.single.nodes(lineage)
                edge_lengths <- bd.update.single.edges(time, time.slice, lineage, edge_lengths)
                if(do_traits) {
                    trait_values <- add.trait.value(trait_values, traits$main, lineage, edge_lengths, type = "all_node")
                }
                ## Update new_lineage as well
                new_lineage <- new_lineage + length(lineage$livings)

                ## Trigger the event
                switch(events[[selected_event]]$target,
                       taxa      = {
                            ## Modify the lineage object
                            lineage   <- events[[selected_event]]$modification(
                                bd.params    = sample.from(bd.params),
                                lineage      = lineage,
                                trait.values = trait_values)
                       },
                       bd.params = {
                            ## Modify the birth death parameters
                            bd.params <- events[[selected_event]]$modification(
                                traits       = traits$main,
                                bd.params    = bd.params,
                                lineage      = lineage,
                                trait.values = trait_values)
                       },
                       traits    = {
                            ## Modify the traits
                            traits    <- events[[selected_event]]$modification(
                                traits       = traits,
                                bd.params    = sample.from(bd.params),
                                lineage      = lineage,
                                trait.values = trait_values)
                       },
                       modifiers = {
                            ## Modify the modifiers
                            modifiers <- events[[selected_event]]$modification(
                                modifiers    = modifiers,
                                bd.params    = sample.from(bd.params),
                                lineage      = lineage,
                                trait.values = trait_values)
                       },
                       founding = {

                            ## Select the founding root 
                            if(!is.null(events[[selected_event]]$args) &&
                               !is.null(events[[selected_event]]$args$founding.root) &&
                                is(events[[selected_event]]$args$founding.root, "function")) {

                                ## Select the founding root using a function
                                select_root   <- events[[selected_event]]$args$founding.root(lineage, trait_values)
                                lineage       <- select_root$lineage
                                founding_root <- select_root$founding_root
                                founding_roots[[length(founding_roots) + 1]] <- founding_root

                            } else {
                                ## By default the founding root is either the last taxa (if extinction) or one of the two new taxa (if speciation)
                                if(was_alive == 0) {
                                    ## Select one of the two taxa from the speciation event to go extinct
                                    founding_root <- sample(new_lineage, 1)

                                    ## Adding a mini waiting time to avoid 0 brlen
                                    short_wait <- mean(edge_lengths)*0.01  ## Change short wait to the actual normal waiting time?
                                    # waiting_time <- modifiers$waiting$fun(bd.params    = sample.from(bd.params),
                                                                          # lineage      = lineage,
                                                                          # trait.values = trait_values,
                                                                          # modify.fun   = modifiers$waiting$internal)
                                    edge_lengths[lineage$livings] <- edge_lengths[lineage$livings] + short_wait#waiting_time
                                    time <- time + short_wait#waiting_time

                                    ## Make one of the two available tips extinct
                                    lineage$livings <- lineage$livings[!(lineage$livings %in% founding_root)]
                                    lineage$n <- lineage$n - 1L
                                } else {
                                    ## Record the founding root
                                    founding_root <- was_alive
                                }
                                ## Record the founding root age
                                founding_roots[[length(founding_roots) + 1]] <- founding_root
                                founding_tree_root_ages[[length(founding_tree_root_ages) + 1]] <- time - first_waiting_time
                            }

                            if(do_traits) {
                                ## Create a copy of the lineage object to select the founding root
                                lineage_root <- lineage
                                lineage_root$current <- founding_root
                                ## Get the root values from the founding root
                                root_values <- as.numeric(parent.traits(trait_values, lineage_root, current = TRUE))
                                ## Save some RAM
                                rm(lineage_root)
                            } else {
                                root_values <- NULL
                            }

                            ## Create a new independent birth death process
                            founding_trees[[length(founding_trees) + 1]] <- events[[selected_event]]$modification(
                                                                                        stop.rule = stop.rule,
                                                                                        time      = time,
                                                                                        lineage   = lineage,
                                                                                        root.trait = root_values)
                       })

                ## Toggle the trigger tracker
                events[[selected_event]]$trigger <- events[[selected_event]]$trigger + 1L
            }
        }
    }

    ############
    ## Cleaning the results
    ############

    ## Summarise into a table (minus the initiation)
    table <- data.frame(parent       = lineage$parents, # These are nodes
                        element      = seq_along(lineage$split), # These are tips or nodes
                        edge_lengths = edge_lengths,
                        is_node      = lineage$split)[-1, ]
    ## Error
    if(nrow(table) == 0) {
        if(!null.error) {
            stop("No tree generated with these parameters.")
        } else {
            return(NULL)
        }
    }

    ## Remove 0 edge split from max.taxa rule
    if(sum(!table$is_node) > stop.rule$max.taxa) {
        ## Remove the 0 edge split
        last_parent <- table$parent[nrow(table)]
        ## Remove the two last edges
        table <- table[-c(nrow(table), nrow(table)-1),]
        ## Change the node into a tip
        table$is_node[table$element == last_parent] <- FALSE
    }

    ## Remove the 0 edges split from background
    if(!is.null(traits$background)) {
        # removed_nodes <- integer()
        # warning("DEBUG") ; print(table)
        while(any(empty_edges <- table$edge_lengths == 0)) {
            ## Finding the edge to remove and to edit
            edge_to_remove <- which(empty_edges)[1]
            edge_to_edit   <- which(table[, "element"] == table[edge_to_remove, "parent"])
            ## Tracking the node being removed
            # removed_nodes <- c(removed_nodes, table[edge_to_remove, "parent"])
            ## Editing the node to keep
            table[edge_to_edit, "element"] <- table[edge_to_remove, "element"]
            table[edge_to_edit, "is_node"] <- table[edge_to_remove, "is_node"]
            ## Removing the extra node
            table <- table[-edge_to_remove, ]
        }
    }

    ############
    ## Creating the tree object
    ############

    ## Number of rows and tips
    n_nodes <- sum(table$is_node)+1
    n_tips  <- sum(!table$is_node)

    ## Getting the edge table node/tips IDs
    table$element2 <- NA
    table$element2[!table$is_node] <- 1:n_tips
    table$element2[ table$is_node] <- order(table$element[table$is_node]) + n_tips + 1

    ## Getting the edge table nodes (left column)
    left_edges <- match(table$parent, table$element)
    table$parent2 <- table$element2[left_edges]
    ## Add the root (is NA)
    table$parent2[is.na(table$parent2)] <- n_tips + 1

    ## Getting the tips and nodes labels
    tree_tips_labels <- paste0("t", 1:n_tips)
    tree_node_labels <- paste0("n", 1:n_nodes)
    tree <- list(edge        = cbind(table$parent2, table$element2),
                 Nnode       = n_nodes,
                 tip.label   = tree_tips_labels,
                 node.label  = tree_node_labels,
                 edge.length = table$edge_lengths)
    class(tree) <- "phylo"

    # warning("DEBUG birth.death_fun.R") ; plot(tree) ; nodelabels(paste(Ntip(tree)+1:Nnode(tree)+Ntip(tree), tree$node.label, sep = ":"), cex = 0.75)

    if(length(founding_trees) > 0) {

        ## The founding event
        found_event <- which(unlist(lapply(events, function(x) return(x$target))) %in% "founding")

        ## Using a prefix?
        get.prefix <- function(event) {
            if(!is.null(event$args) && !is.null(event$args$prefix) && is(event$args$prefix, "character")) {
                return(event$args$prefix)
            } else {
                return(NULL)
            }
        }
        tree_prefixes <- lapply(events[found_event], get.prefix)
        for(i in 1:length(tree_prefixes)) {
            if(is.null(tree_prefixes[[i]])) {
                tree_prefixes[[i]] <- paste0("founding_", found_event[i])
            }
        }

        ## Rename the tips and nodes of the founding tree
        add.prefix <- function(one_prefix, one_founding_tree, n_tips, n_nodes) {
            return(list(tip  = paste0(one_prefix, "t", (n_tips+1):(n_tips+Ntip(one_founding_tree$tree))),
                        node = paste0(one_prefix, "n", (n_nodes+1):(n_nodes+Nnode(one_founding_tree$tree)))))
        }
        updated_names <- mapply(add.prefix, tree_prefixes, founding_trees, MoreArgs = list(n_tips = n_tips, n_nodes = n_nodes), SIMPLIFY = FALSE)

        ## Rename the tips and nodes of the data
        if(do_traits) {
            update.rownames <- function(one_updated_name, one_founding_tree) {
                rownames(one_founding_tree$data) <- c(one_updated_name$tip, one_updated_name$node)[match(rownames(one_founding_tree$data), c(one_founding_tree$tree$tip.label, one_founding_tree$tree$node.label))]
                return(one_founding_tree)
            }
            founding_trees <- mapply(update.rownames, updated_names, founding_trees, SIMPLIFY = FALSE)
        }

        ## Rename the tips and nodes in the tree
        update.labels <- function(one_updated_name, one_founding_tree) {
            one_founding_tree$tree$tip.label <- one_updated_name$tip
            one_founding_tree$tree$node.label <- one_updated_name$node
            return(one_founding_tree)
        }
        founding_trees <- mapply(update.labels, updated_names, founding_trees, SIMPLIFY = FALSE)

        ## Get the binding position on the tree
        binding_positions <- lapply(founding_roots, function(x, table) cbind(table$parent2, table$element2)[which(table$element == x), 2], table = table)

        ## Combine the trees serially
        combined_tree <- tree
        tmp_trees <- founding_trees
        tmp_pos <- binding_positions
        while(length(tmp_trees) > 0) {
            combined_tree <- bind.tree(combined_tree, tmp_trees[[1]]$tree, where = tmp_pos[[1]])
            tmp_pos[[1]] <- tmp_trees[[1]] <- NULL
        }

        ## Adjust for the other stop rules (but time rule gets priority)
        if(stop.rule$max.time == Inf) {
            ## Getting the ages of each tips in both trees
            founding_ages <- lapply(founding_trees, function(x) tree.age(x$tree, digits = 8))
            tree_ages     <- tree.age(tree, digits = 8)
            combined_ages <- tree.age(combined_tree, digits = 8)

            ## Getting the list of living tips in both trees
            founding_livings <- lapply(founding_ages, function(x) x$elements[x$ages == min(x$ages)])
            tree_living     <- tree_ages$elements[tree_ages$ages == min(tree_ages$ages)]

            ## Getting the ages of the living tips in the combined tree
            comb_foun_ages <- lapply(founding_livings, function(x, combined_ages) {combined_ages$ages[combined_ages$elements %in% x]}, combined_ages)
            comb_tree_ages <- combined_ages$ages[combined_ages$elements %in% tree_living]

            ## Living tips ages difference
            living_diffs <- unlist(lapply(comb_foun_ages, function(x, comb_tree_ages) comb_tree_ages[1] - x[1], comb_tree_ages))

            if(any(living_diffs != 0)) {
                ## Update all the differences
                differences <- living_diffs
                counter <- 0
                while(length(differences) > 0) {
                    living_diff <- differences[1]
                    differences <- differences[-1]
                    counter <- counter + 1
                    if(living_diff != 0) {
                        ## Select which ones are the younger tips
                        if(living_diff > 0) {
                            older_tips <- tree_living
                        } else {
                            older_tips <- founding_livings[[counter]]
                        }
                        ## Adding the living_diff to their edges
                        #TODO: add the edge length to their tip or to their root?
                        older_tips_edges <- which(combined_tree$edge[, 2] %in% which(combined_tree$tip.label %in% older_tips))
                        combined_tree$edge.length[older_tips_edges] <- combined_tree$edge.length[older_tips_edges] + abs(living_diff)
                    }                   
                }
            }

            ## Adjust for number of living taxa
            if(stop.rule$max.living != Inf || stop.rule$max.taxa != Inf) {

                ## Selecting the criteria to optimise
                if(stop.rule$max.taxa == Inf) {
                    ## Don't do all tips (just living)
                    do_all_tips <- FALSE
                    max_criteria <- stop.rule$max.living
                } else {
                    ## Don't do all tips if max.taxa is lower than max.living (just living) else, do all tips
                    do_all_tips <- stop.rule$max.taxa < stop.rule$max.living
                    max_criteria <- ifelse(do_all_tips, stop.rule$max.taxa, stop.rule$max.living)
                }

                ## Go to a point in the past where there are less livings
                digits <- 6
                tips_ages <- round(dist.nodes(combined_tree)[,Ntip(combined_tree)+1], digits)

                ## Select the different criterias to optimise (living or all tips)
                if(!do_all_tips) {
                    tips_criteria <- sum(tips_ages == round(max(tips_ages), digits))
                } else {
                    tips_criteria <- Ntip(combined_tree)
                }
                
                ## Initialise values for the loop
                root_time <- max(tips_ages)
                time <- 0
                slider <- root_time * 0.01 # 1% of the root age

                crossed_edges <- NULL
                while(tips_criteria > max_criteria) {
                    ## Increase the time (decrease)
                    time <- time + slider
                    slice_time <- root_time - time
                    ## Find the number of living taxa (crossed edges then)
                    crossed_edges <- which((tips_ages[combined_tree$edge[, 1] ] < slice_time) & (tips_ages[combined_tree$edge[, 2] ] >= slice_time))
                    tips_criteria <- length(crossed_edges)
                        
                    ## Also count the fossils
                    if(do_all_tips) {
                        tips_criteria <- tips_criteria + sum(tips_ages[1:Ntip(combined_tree)] < slice_time)
                    }
                }

                ## Trimming the tree (inspired by the paleotree::timeSliceTree function)
                tips_to_drop <- numeric()
                bipartitions <- prop.part(combined_tree)
                combined_tips <- Ntip(combined_tree)
                
                if(!is.null(crossed_edges)) {
                    for(edge in crossed_edges){
                        descendant <- combined_tree$edge[edge ,2]
                        if(descendant > combined_tips) {   
                            all_descendants <- bipartitions[[descendant-combined_tips]]
                            tips_to_drop <- c(tips_to_drop, all_descendants[-1])
                        }
                    }
                    ## Trim the combined tree
                    combined_tree <- drop.tip(combined_tree, tips_to_drop)
                    nodes_depth <- node.depth.edgelength(combined_tree)
                    crossed_edges <- (nodes_depth[combined_tree$edge[, 2]] >= slice_time)
                    crossed_node_depth <- nodes_depth[combined_tree$edge[crossed_edges, 1]]
                    combined_tree$edge.length[crossed_edges] <- slice_time-crossed_node_depth
                }
            }
        }

        ## Replace the tree by the combined tree
        tree <- combined_tree
    }

    ## Adding the root time
    tree$root.time <- max(dispRity::tree.age(tree)$age)

    ## Check if it reached the proper stop.rule values
    ## Get the achieved values
    achieved <- list(max.living = length(which(dispRity::tree.age(tree)$age == 0)),
                     max.taxa   = Ntip(tree),
                     max.time   = tree$root.time + edge_lengths[1])

    ## Find if any matches the requested ones
    if(check.results) {
        ## Infinite rules are now NAs
        check.requested <- function(requested, achieved){
            if(requested == Inf) {NA} else {achieved == requested}
        }
        ## Check achievements
        achievements <- unlist(mapply(check.requested, stop.rule[names(achieved)], achieved))
        
        ## Warning or fail
        if(!any(achievements[-which(is.na(achievements))])) {
            if(!null.error) {
                warning("The simulation stopped before reaching any of the stopping rule.", call. = FALSE)
            } else {
                return(NULL)
            }
        }
    }

    ## Adding the traits to the table
    if(do_traits) {
        trait_table <- cbind(parent = table$parent, element = table$element, edge = table$edge_lengths, trait_values[match(table$element, rownames(trait_values)), ])
        ## Add the root value
        trait_table <- rbind(c(parent = 0, element = 1, edge = 0, trait_values[1, ]),
                             trait_table)
        ## Find the living tips
        living_tips <- which(is.na(rownames(trait_table)))
        ## Simulate the traits for the living tips and add them to the trait table
        if(length(living_tips) > 0) {
            if(is.null(traits$background)) {
                trait_table[living_tips, -c(1:3)] <- t(sapply(living_tips, sim.living.tips, trait_table, traits$main, simplify = TRUE))
            } else {
                trait_table[living_tips, -c(1:3)] <- t(sapply(living_tips, sim.living.tips, trait_table, traits$background$main, simplify = TRUE))
            }
        }

        ## Renaming the trait_values
        trait_table <- trait_table[, -c(1:3), drop = FALSE]
        rownames(trait_table) <- c(tree_tips_labels, tree_node_labels)[c(n_tips+1, table$element2)]

        ## Add the column names (if missing)
        if(length(missing_names <- which(colnames(trait_table) == "")) > 0) {
            colnames(trait_table)[missing_names] <- names(traits$main)
        }

        ## Adding the founding data to the trait_table
        if(length(founding_trees) > 0) {
            ## Combining the trait tables
            trait_table <- rbind(trait_table, do.call(rbind, lapply(founding_trees, `[[`, "data")))

            ## Removing any tips/nodes that ended up not being present in the tree
            trait_table <- trait_table[rownames(trait_table) %in% c(tree$tip.label, tree$node.label), , drop = FALSE]
        }
    }

    ## Output
    return(list(tree = tree, data = trait_table))
}