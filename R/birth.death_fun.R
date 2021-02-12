
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

## Check the events triggering (the first one gets triggered)
trigger.events <- function(one_event, bd.params, lineage, trait.values, time) {
    return(one_event$condition(bd.params = bd.params, lineage = lineage, trait.values = trait.values, time = time) && one_event$trigger < 1L)
}

## Run a birth death process to generate both tree and traits
birth.death.tree.traits <- function(bd.params, stop.rule, traits = NULL, modifiers = NULL, events = NULL, null.error = FALSE, check.results = TRUE) {
  
    ############
    ## Initialising
    ############

    ## Set up the traits, modifiers and events simulation
    do_traits    <- is.null(traits)
    do_events    <- is.null(events)

                                        ## Initialise the tree
                                        tree <- build.tree$new()
                                        ## First waiting time
                                        tree$first.wait(bd.params)

    ## Update the global time (for the first waiting time)
    if(stop.rule$max.time != Inf && tree$time == 0) {
        stop.rule$max.time <- stop.rule$max.time + tree$first_waiting_time
    }

    ## Start the trait    
    if(do_traits) {
        build.tree$first.trait()
        # trait_values <- rbind(NULL, c(unlist(lapply(traits, function(x) return(x$start)))))
        # rownames(trait_values) <- 1
    }

    ## Randomly triggering an event

    if(tree$modifiers[[1]]$speciating$fun(
        bd.params    = bd.params,
        lineage      = NULL,
        trait.values = NULL,
        modify.fun   = NULL)) {
        
        ## Speciating:
        if(tree$lineage$n == stop.rule$max.living) {
            ## Don't add this one
            if(!null.error) {
                stop("No tree generated with these parameters.")
            } else {
                return(NULL)
            }
        }

                                        ## Add a new lineage
                                        tree$new.lineage()

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
    # warning("DEBUG birth.death_fun.R") ; counter <- 0
    # warning("DEBUG birth.death_fun.R") ; set.seed(8)
    # warning("DEBUG birth.death_fun.R") ; record <- c("start recording events:\n")

    ## Build the rest of the tree
    while(tree$lineage$n > 0 &&
          tree$lineage$n <= stop.rule$max.living &&
          sum(!tree$lineage$split) <= stop.rule$max.taxa) {
        
        # warning("DEBUG birth.death_fun.R") ; counter <- counter+1
        # warning("DEBUG birth.death_fun.R") ; print(counter)
        # warning("DEBUG birth.death_fun.R") ; if(counter == 123) break 

                                ## Pick a lineage for the event to happen to:
                                tree$draw.lineage(bd.params)
                                ## Get the waiting time
                                tree$add.wait.time(bd.params)


        ## Arrived at the max time (cut the branch lengths)
        if(tree$time > stop.rule$max.time) {
            tree$waiting_time <- tree$waiting_time - (tree$time - stop.rule$max.time)
            tree$edge_lengths[tree$lineage$livings] <- tree$edge_lengths[tree$lineage$livings] + tree$waiting_time
            tree$time <- stop.rule$max.time
            break
        }

                                ## Updating branch length
                                tree$update.branch.length()


        ## Adding a new row to the trait_values matrix
        if(do_traits) {
            tree$update.trait()
            # trait_values <- rbind(trait_values,
            #                       ## Add the updated trait from the parent lineage
            #                       unlist(lapply(traits,
            #                                     sim.element.trait,
            #                                     parent.trait = parent.traits(trait_values, lineage),
            #                                     edge.length  = edge_lengths[lineage$current])#,
            #                                     #trait.values = trait_values,
            #                                     #lineage      = lineage,
            #                                     #trait        = trait
            #                                     ## Handle these internally and with make.traits
            #                                     )
            #                      , deparse.level = 0)
            # rownames(trait_values)[rownames(trait_values) == ""] <- lineage$current
        }

        ## Randomly triggering an event
        if(tree$modifiers[[1]]$speciating$fun(
            bd.params    = bd.params,
            lineage      = tree$lineage,
            trait.values = tree$trait_values,
            modify.fun   = tree$modifiers[[1]]$speciating$internal)) {
            
            ## Speciating:
            if(tree$lineage$n == stop.rule$max.living) {
                ## Don't add this one
                break
            }

                                        ## Add a new lineage
                                        tree$new.lineage()
        } else {

                                        ## Go extinct
                                        tree$go.extinct()

        }

        # warning("DEBUG"); reached_event <- TRUE

        ## Trigger events
        if(do_events) {
            ## Check all the events that can be triggered
            triggers <- unlist(lapply(events, trigger.events,
                                            bd.params = bd.params,
                                            lineage = lineage,
                                            trait.values = trait_values,
                                            time = time - first_waiting_time))

            if(any(triggers)) {
                
                ## Selecting the first triggerable event
                selected_event <- which(triggers)[1]

                ## Trigger the event
                switch(events[[selected_event]]$target,
                       taxa      = {
                            ## Modify the lineage object
                            lineage   <- events[[selected_event]]$modification(
                                bd.params    = bd.params,
                                lineage      = lineage,
                                trait.values = trait_values)
                       },
                       bd.params = {
                            ## Modify the birth death parameters
                            bd.params <- events[[selected_event]]$modification(
                                bd.params    = bd.params,
                                lineage      = lineage,
                                trait.values = trait_values)
                       },
                       traits    = {
                            ## Modify the traits
                            traits    <- events[[selected_event]]$modification(
                                traits       = traits,
                                bd.params    = bd.params,
                                lineage      = lineage,
                                trait.values = trait_values)
                       },
                       modifiers = {
                            ## Modify the modifiers
                            modifiers <- events[[selected_event]]$modification(
                                modifiers    = modifiers,
                                bd.params    = bd.params,
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
                            } else {
                                ## By default the founding root is either the last taxa (if extinction) or one of the two new taxa (if speciation)
                                if(was_alive == 0) {
                                    ## Select one of the two taxa from the speciation event to go extinct
                                    founding_root <- sample(new_lineage, 1)

                                    ## Adding a mini waiting time to avoid 0 brlen
                                    short_wait <- mean(edge_lengths)*0.01
                                    edge_lengths[lineage$livings] <- edge_lengths[lineage$livings] + short_wait
                                    time <- time + short_wait

                                    ## Make one of the two available tips extinct
                                    lineage$livings <- lineage$livings[!(lineage$livings %in% founding_root)]
                                    lineage$n <- lineage$n - 1L
                                } else {
                                    ## Record the founding root
                                    founding_root <- was_alive
                                }
                                ## Record the founding root age
                                founding_tree_root_age <- time - first_waiting_time
                            }

                            ## Create a new independent birth death process
                            founding_tree <- events[[selected_event]]$modification(
                                stop.rule = stop.rule,
                                time      = time,
                                lineage   = lineage)
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
    table <- data.frame(parent       = tree$lineage$parents, # These are nodes
                        element      = seq_along(tree$lineage$split), # These are tips or nodes
                        edge_lengths = tree$edge_lengths,
                        is_split     = tree$lineage$split)[-1, ]

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
    tree_tips_labels <- paste0("t", 1:n_tips)
    tree_node_labels <- paste0("n", 1:n_nodes)
    tree <- list(edge        = cbind(table$parent2, table$element2),
                 Nnode       = n_nodes,
                 tip.label   = tree_tips_labels,
                 node.label  = tree_node_labels,
                 edge.length = table$edge_lengths)
    class(tree) <- "phylo"

    if(!is.null(founding_tree)) {

        ## The founding event
        found_event <- which(unlist(lapply(events, function(x) return(x$target))) %in% "founding")[1]

        #TODO: bug if more than one founding event!

        ## Using a prefix?
        if(!is.null(events[[found_event]]$args) && !is.null(events[[found_event]]$args$prefix) && is(events[[found_event]]$args$prefix, "character")) {
            tree_prefix <- events[[found_event]]$args$prefix
        } else {
            tree_prefix <- NULL
        }

        ## Rename the tips and nodes of the founding tree
        tip_names_updated <- paste0(tree_prefix, "t", (n_tips+1):(n_tips+Ntip(founding_tree$tree)))
        node_names_updated <- paste0(tree_prefix, "n", (n_nodes+1):(n_nodes+Nnode(founding_tree$tree)))

        ## Rename the tips and nodes of the data
        if(do_traits) {
            rownames(founding_tree$data) <- c(tip_names_updated, node_names_updated)[match(rownames(founding_tree$data), c(founding_tree$tree$tip.label, founding_tree$tree$node.label))]
        }

        ## Update the tipe and node numbers
        founding_tree$tree$tip.label  <- tip_names_updated
        founding_tree$tree$node.label <- node_names_updated

        ## Get the binding position on the tree
        binding_position <- cbind(table$parent2, table$element2)[which(table$element == founding_root), 2]

        ## Combine both trees
        combined_tree <- bind.tree(tree, founding_tree$tree, where = binding_position)

        ## Adjust for the other stop rules (but time rule gets priority)
        if(stop.rule$max.time == Inf) {
            ## Getting the ages of each tips in both trees
            founding_ages <- tree.age(founding_tree$tree, digits = 8)
            tree_ages     <- tree.age(tree, digits = 8)
            combined_ages <- tree.age(combined_tree, digits = 8)

            ## Getting the list of living tips in both trees
            founding_living <- founding_ages$elements[founding_ages$ages == min(founding_ages$ages)]
            tree_living     <- tree_ages$elements[tree_ages$ages == min(tree_ages$ages)]

            ## Getting the ages of the living tips in the combined tree
            comb_foun_ages <- combined_ages$ages[combined_ages$elements %in% founding_living]
            comb_tree_ages <- combined_ages$ages[combined_ages$elements %in% tree_living]

            ## Living tips ages difference
            living_diff <- comb_tree_ages[1] - comb_foun_ages[1]

            if(living_diff != 0) {

                ## Select which ones are the younger tips
                if(living_diff > 0) {
                    older_tips <- tree_living
                } else {
                    older_tips <- founding_living
                }

                ## Adding the living_diff to their edges
                older_tips_edges <- which(combined_tree$edge[, 2] %in% which(combined_tree$tip.label %in% older_tips))
                combined_tree$edge.length[older_tips_edges] <- combined_tree$edge.length[older_tips_edges] + abs(living_diff)
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
        check.requested <- function(requested, achieved){
            if(requested == Inf) {TRUE} else {achieved == requested}
        }
        achieved <- unlist(mapply(check.requested, stop.rule[names(achieved)], achieved))
        
        ## Warning or fail
        if(any(!achieved)) {
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
            trait_table[living_tips, -c(1:3)] <- t(sapply(living_tips, sim.living.tips, trait_table, traits, simplify = TRUE))
        }

        ## Renaming the trait_values
        trait_table <- trait_table[, -c(1:3), drop = FALSE]
        rownames(trait_table) <- c(tree_tips_labels, tree_node_labels)[c(n_tips+1, table$element2)]

        ## Add the column names (if missing)
        if(length(missing_names <- which(colnames(trait_table) == "")) > 0) {
            colnames(trait_table)[missing_names] <- names(traits)
        }

        ## Adding the founding data to the trait_table
        if(!is.null(founding_tree)) {

            ## Combining the trait tables
            trait_table <- rbind(trait_table, founding_tree$data)

            ## Removing any tips/nodes that ended up not being present in the tree
            trait_table <- trait_table[rownames(trait_table) %in% c(tree$tip.label, tree$node.label), , drop = FALSE]
        }
    }

    ## Output
    return(list(tree = tree, data = trait_table))
}