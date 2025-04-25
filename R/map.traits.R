#' @title Maps a trait on a tree
#'
#' @description Simulates one or more trait specified through a "traits" onto one or multiple trees.
#'
#' @param traits A \code{"traits"} object (see \code{\link{make.traits}}).
#' @param tree   A \code{"phylo"} or \code{"multiPhylo"} object.
#' @param events Optional, an \code{"events"} object to apply to the tree (see \code{\link{make.events}}).
#' @param replicates Optional, a number of replicated traits to map.
#'
#' @return
#' A \code{"treats"} object containing the tree and the traits.
#' 
#' @details  
#' This function simulates the trait(s) on the tree using the tree's branch length.
#'
#' Note that when using an \code{"events"} with a \code{age.condition}, the age is calculated in time from the root of the tree, not in time from the present. So an event triggered with \code{age.condition(5)} will trigger 5 units from the root of the tree, not from the tips. To get it to trigger from the tips you can calculate it as \code{age.condition(my_tree$root.time-5)}.
#'
#' @examples
#' ## Simulating a random tree with branch length
#' my_tree <- rtree(20)
#' 
#' ## Creating three different traits objects:
#' ## A Brownian Motion
#' bm_process <- make.traits(process = BM.process)
#' ## An Ornstein-Uhlenbeck process
#' ou_process <- make.traits(process = OU.process)
#' ## No process (just randomly drawing values from a normal distribution)
#' no_process <- make.traits(process = no.process)
#' 
#' ## Mapping the three traits on the phylogeny
#' bm_traits <- map.traits(bm_process, my_tree)
#' ou_traits <- map.traits(ou_process, my_tree)
#' no_traits <- map.traits(no_process, my_tree)
#' 
#' ## Plotting the topology and the different traits
#' oldpar <- par(mfrow = c(2,2))
#' plot(my_tree, main = "Base topology")
#' plot(bm_traits, main = "Mapped BM")
#' plot(ou_traits, main = "Mapped OU")
#' plot(no_traits, main = "Mapped normal trait")
#' par(oldpar)
#'
#' @author Thomas Guillerme, Caleb Scutt


map.traits <- function(traits, tree, events = NULL, replicates) {
    ## Sanitizing
    check.class(traits, c("treats", "traits"), " must be of class \"traits\". You can generate such object using:\nmake.traits()")
    tree_class <- check.class(tree, c("phylo", "multiPhylo"))

    if(tree_class == "multiPhylo") {
        out <- lapply(tree, function(tree, traits, events, replicates) map.traits(traits, tree, events, replicates), traits = traits, events = events, replicates = replicates)
        class(out) <- "treats"
        return(out)
    }

    if(tree_class == "phylo") {
        tree <- list(tree)
    }
    ## Check node labels on all trees
    add.nodes.root <- function(tree) {
        if(is.null(tree$node.label)) {
            tree <- makeNodeLabel(tree, prefix = "n")
        }
        if(is.null(tree$root.time)) {
            tree <- set.root.time(tree)
        }
        return(tree)
    }
    tree <- lapply(tree, add.nodes.root)

    ## Check replicates
    if(missing(replicates)) {
        replicates <- 1
    } else {
        check.class(replicates, c("numeric", "integer"))
    }

    ## Check for edge lengths
    if(any(unlist(lapply(tree, function(x) is.null(x$edge.length))))) {
        stop(paste0("The input tree", ifelse(tree_class == "phylo", " has", "s have"), " no branch lengths."))
    }

    ## Ignore the background trait
    if(!is.null(traits$background)) {
        warning("Background traits cannot be mapped and is ignored.")
    }

    ## If events, trigger the recursion
    if(!is.null(events)) {
        if(!is(events, "treats") && is(events, "events")) {
            stop(paste0("Events needs to be of class \"events\". You can generate such object using:\nmake.events()"))
        }
        ## Check if there are multiple events
        n_events <- sum(abs(unlist(lapply(lapply(events, `[[`, "trigger"), function(x) ifelse(x == 0, 1, x)))))
        
        if(n_events > 1) {
            stop("map.traits does not currently work with more that one event (or a recurring one).")
            ## Do some nesting here if n_events > 1
        }
        ## Only works for traits
        if(events[[1]]$target != "traits") {
            stop("map.traits does currently only works when targeting traits")
        }

        ## If the events condition is traits, run the mapping first
        trait_condition <- FALSE

        ## Get the trigger condition time for the slicing
        slicing_time <- get.trigger.time(events, tree, traits)

        ## Placeholder for splitting the trees
        trees_list <- lapply(tree, tree.slice.caleb, slicing_time)
        parent_trees <- lapply(trees_list, `[[`, "parent_tree")
        class(parent_trees) <- "multiPhylo"
        orphan_trees <- lapply(lapply(trees_list, `[[`, "orphan_tree"), function(x) {class(x) <- "multiPhylo"; return(x)})

        ## Run the simulations on the parent_tree
        if(!trait_condition) {
            ## Run the normal map.traits
            parent_trees_traits <- map.traits(parent_trees, traits = traits, replicates = replicates)
            ## Make into a list of treats if it was a single tree
            if(!is(parent_trees_traits[[1]], "treats")) {
                parent_trees_traits <- list(parent_trees_traits)
            }
            ## Get the edges_values for the parent traits
            parent_trait_values <- lapply(parent_trees_traits, function(treats) {treats$data[grep("map.traits_split", rownames(treats$data)),, drop = FALSE]})

            ## Sort the parent traits as a list in the same order as the orphan_trees roots
            roots_order <- lapply(lapply(orphan_trees, lapply, function(x) x$node.label[1]), unlist, recursive = FALSE)
            traits_order <- mapply(match, roots_order, lapply(parent_trait_values, rownames), SIMPLIFY = FALSE)
            reorder.trait <- function(trait, order) {
                return(trait[order,, drop = FALSE])
            }
            parent_trait_values <- mapply(reorder.trait, parent_trait_values, traits_order, SIMPLIFY = FALSE)

            ## Get the orphan trees and new traits (with starting trait values)
            orphan_maps <- list()
            for(one_tree in 1:length(orphan_trees)) {
                orphan_maps[[one_tree]] <- mapply(prep.traits,
                                one_orphan_tree = orphan_trees[[one_tree]],
                                one_parent_trait_value = unlist(apply(parent_trait_values[[one_tree]], 1, list), recursive =  FALSE),
                                # TODO: if nested events, add the list of nested events here
                                MoreArgs = list(one_events = events[[1]], traits = traits, replicates = replicates), SIMPLIFY = FALSE)
            }

            ## Run all the orphan maps
            orphan_data <- lapply(orphan_maps, lapply, function(x) do.call(map.traits, args = x))

            ## Extract only the data
            all_orphan_data <- lapply(lapply(orphan_data, lapply, `[[`, "data"), function(x) do.call(rbind, x))
            ## Merge the data
            combined_trait_data <- mapply(function(x,y) rbind(x$data, y), parent_trees_traits, all_orphan_data, SIMPLIFY = FALSE)
            ## Remove "map.traits_split" elements
            cleaned_trait_data <- lapply(combined_trait_data, function(x) x[!grepl("map.traits_split", rownames(x)),, drop = FALSE])

            ## Make into treats objects
            class(tree) <- "multiPhylo"

            make.treats(tree[[1]], cleaned_trait_data[[1]])

            output <- mapply(make.treats, tree, cleaned_trait_data, SIMPLIFY = FALSE)
        }
    
    } else {
        ## Map the traits
        all_traits <- replicate(replicates, lapply(tree, map.traits_fun, traits = traits), simplify = FALSE)

        ## Output
        output <- lapply(all_traits, lapply, function(x) make.treats(x$tree, x$data))
    }

    ## Merge into treats

    ## Output
    if(tree_class == "phylo") {
        output <- lapply(output, function(x) return(x[[1]]))
    } else {
        output <- unlist(output, recursive = FALSE)
    }
    if(length(output) == 1) {
        output <- output[[1]]
    }
    class(output) <- c("treats")
    return(output)  
}

## Internal function for a single tree
map.traits_fun <- function(tree, traits) {

    ## Get the edge table
    edge_table <- cbind(tree$edge, tree$edge.length)

    ## Ladderise the edge table
    edge_table <- edge_table[order(edge_table[, 1]), , drop = FALSE]

    ## Initialise the trait table
    trait_values <- rbind(NULL, c(unlist(lapply(traits$main, function(x) return(x$start)))))
    rownames(trait_values)[1] <- Ntip(tree)+1

    ## Select only the nodes from the edge table
    nodes_table <- edge_table[which(edge_table[,2] > Ntip(tree)), , drop = FALSE]
    ## Creating the trait_table (first row is the edge from nothing to root)
    trait_table <- rbind(c(NA, Ntip(tree)+1, 0, trait_values[1,]), cbind(edge_table, matrix(NA, ncol = ncol(trait_values), nrow = nrow(edge_table))))
    colnames(trait_table) <- c("parent", "element", "edge", colnames(trait_values))

    ## Populate the trait values
    while(nrow(nodes_table) > 0) {
        ## Generate the trait
        trait_values <- rbind(trait_values,
                              unlist(lapply(traits$main,
                                            sim.element.trait,
                                            parent.trait = trait_values[(nodes_table[1, 1]-Ntip(tree)), ],
                                            edge.length  = nodes_table[1, 3])
                                            )
                             , deparse.level = 0)
        ## Name the trait
        rownames(trait_values)[nrow(trait_values)] <- nodes_table[1, 2]
        ## Decrease the node table
        nodes_table <- nodes_table[-1, , drop = FALSE]
    }

    ## Combine the edge tables to be the birth.death_fun format
    trait_table[match(as.integer(rownames(trait_values)), trait_table[,2]), -c(1:3)] <- trait_values
    ## Find the tips
    living_tips <- which(is.na(trait_table[, 4]))
    ## Generate the traits for the tips
    trait_table[living_tips, -c(1:3)] <- t(sapply(living_tips, sim.living.tips, trait_table, traits$main, simplify = TRUE))
    ## Clean the table
    rownames(trait_table) <- c(tree$tip.label, tree$node.label)[trait_table[,2]]
    trait_table <- trait_table[, -c(1:3), drop = FALSE]
    return(list(tree = tree, data = trait_table))
}

## Internal function for getting events trigger times
get.trigger.time <- function(events, tree, traits) {
    ## Get the time condition (from start)
    # age.condition
    if(as.character(body(events[[1]]$condition)[[2]][[2]]) == "time") {
        return(eval(body(events[[1]]$condition)[[2]][[3]], env = environment(events[[1]]$condition)))    
    } else {
        stop("map.traits currently only works with events triggered by age.condition", call. = FALSE)
    }

    #taxa.condition

    #trait.condition
}

## Internal function for preparing traits after an event
prep.traits <- function(one_orphan_tree, one_parent_trait_value, one_events, traits, replicates) {
    ## Create the new arguments for map.traits
    return(list(tree = one_orphan_tree,
                traits = one_events$modification(traits, start = as.numeric(one_parent_trait_value)),
                events = NULL,
                replicates = replicates))
}


## Function for adding branch at root to correctly rescale orphan trees
add.root.edge <- function(tree, new.root.edge) {
    tree$edge <- rbind(c(Ntip(tree)+1, Ntip(tree)+2), ifelse(tree$edge > Ntip(tree), tree$edge+1, tree$edge))
    tree$edge.length <- c(new.root.edge, tree$edge.length)
    tree$Nnode <- tree$Nnode + 1
    return(tree)
}

# get.orphan.tree.ages <- function(orphan_tree, full_tree) {
#     tree_age_data <- tree.age(full_tree)
#     return(tree_age_data$ages[as.character(tree_age_data$elements) %in% orphan_tree$node.label[1]])
# }

tree.slice.caleb <- function(tree, slice) {
    ## Slice the tree at the age
    splitted <- dispRity::slice.tree(tree, age = tree$root.time-slice, model = "acctran", keep.all.ancestors = TRUE)

    ## Precalculating tree ages (recycled in several places)
    tree_ages <- tree.age(tree, digits = 7)

    ## Getting the subtrees (orphans)

    ## Finds branches that cross slice
    splitted_branches <- splitted$tip.label[which(splitted$tip.label %in% tree$node.label)]
    ## Extracts trees that are descendant from the sliced branches
    orphan_trees <- castor::get_subtrees_at_nodes(tree, splitted_branches)$subtrees

    ## Find the missing branch length for the orphan
    # orphan_ages <- lapply(orphan_trees, get.orphan.tree.ages, full_tree = tree)
    orphan_ages <- lapply(orphan_trees, function(one_tree, tree_age_data) return(tree_age_data$ages[as.character(tree_age_data$elements) %in% one_tree$node.label[1]]), tree_age_data = tree_ages)

    ## Relabel nodes and tips at slice point with a tractable suffix
    map.traits_label <- paste0("map.traits_split_node", 1:length(splitted_branches)) 
    splitted$tip.label[splitted$tip.label %in% splitted_branches] <- map.traits_label

    ## Rescale the orphan trees (i.e. adding their extra branch length)
    added_br_length <- lapply(orphan_ages, function(ages, slice) slice - ages, slice = (tree$root.time-slice))
    ## Add the root edge
    rescaled_orphans <- Map(add.root.edge, orphan_trees, added_br_length)
    ## Add the new labels
    rescaled_orphans <- Map(function(tree, label) {
                            tree$node.label <- c(label, tree$node.label)  
                            return(tree)
                        }, rescaled_orphans, map.traits_label)

    ## Getting the singleton trees (orphans)

    ## Detecting which tip is a singleton (tip in sliced tree is at minimum age + tip in the input tree is not at slice time)
    splitted_ages <- tree.age(splitted, digits = 7)
    ## Find the singletons (ignores the tips that are already called map.traits)
    splitted_ages_singeltons <- splitted_ages[!grepl("map.traits_split_node", splitted_ages$elements), ]
    singletons_tips <- splitted_ages_singeltons$elements[which(splitted_ages_singeltons$ages == min(splitted_ages_singeltons$ages))]
    ## Dropping singletons that have the age of the slice time
    if(any(same_age <- tree_ages$ages[tree_ages$elements %in% singletons_tips] == (tree$root.time-slice))) {
        singletons_tips <- singletons_tips[-same_age]
    }
    ## Calculating the singletons edges length
    singletons_edges <- (tree$root.time-slice) - tree_ages$ages[tree_ages$elements %in% singletons_tips]

    ## Make the mini trees
    make.mini.tree <- function(tip, edge_length) {
        tree <- list(tip.label = tip,
                     node.label = paste0("map.traits_split_tip", tip),
                     Nnode = 1,
                     edge = matrix(c(2, 1), ncol = 2, byrow = TRUE),
                     edge.length = edge_length)
        class(tree) <- "phylo"
        return(tree)
    }
    singletons_trees <- mapply(make.mini.tree, as.list(singletons_tips), as.list(singletons_edges), SIMPLIFY = FALSE)

    ## Renaming the branching tips on splitted
    splitted$tip.label[splitted$tip.label %in% singletons_tips] <- paste0("map.traits_split_tip", singletons_tips)
 
    ## Combine the orphan trees
    orphan_trees <- c(rescaled_orphans, singletons_trees)

    ## Return parent and orphans
    return(list(parent_tree = splitted, orphan_tree = orphan_trees))
}