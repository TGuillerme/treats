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
    if(tree_class == "phylo") {
        tree <- list(tree)
    }

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


        ## If the events condition is traits, run the mapping first
        trait_condition <- FALSE

        ## Get the trigger condition time for the slicing
        slicing_time <- get.trigger.time(events, tree, traits)

        ## Placeholder for splitting the trees
        parent_trees <- list()
        orphan_trees <- list()

        ## Run the simulations on the parent_tree
        if(!trait_condition) {
            ## Run the normal map.traits
            parent_trees_traits <- map.traits(parent_trees, traits = traits, replicates = replicates)
            ## Get the edges_values for the parent traits
            parent_trait_values <- lapply(parent_trees_traits, function(treats) {treats$x[grep("map.traits_split", rownames(treats$x)),, drop = FALSE]})

            ## Get the orphan trees and new traits (with starting trait values)
            orphan_maps <- mapply(prep.traits, orphan_trees, parent_trait_values, MoreArgs = list(one_events = events[[1]], traits = traits, replicates = replicates), SIMPLIFY = FALSE)
            ## TODO: if nested events, add this here!

            ## Run all the orphan maps
            orphan_data <- do.call(map.traits, orphan_maps)

            ## Extract only the data
            if(replicates > 1) {
                all_traits <- do.call(cbind, lapply(orphan_data, lapply, `[[`, "x")) # OR rbind???
            } else {
                all_traits <- do.call(cbind, lapply(orphan_data, `[[`, "x")) # OR rbind???
            }

            ## Merge the data
            all_traits <- cbind(parent_trees_traits$x, all_traits)
            ## Remove duplicates
            all_traits <- all_traits[unique(rownames(all_traits)),, drop = FALSE]
        }
    

    } else {
        ## Map the traits
        all_traits <- replicate(replicates, lapply(tree, map.traits_fun, traits = traits), simplify = FALSE)

        ## Output
        output <- lapply(all_traits, lapply, function(x) make.treats(x$tree, x$data))
    }

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

    ## Generate node names
    if(is.null(tree$node.label)) {
        tree$node.label <- paste0("n", seq(1:Nnode(tree)))
    }

    ## Get the edge table
    edge_table <- cbind(tree$edge, tree$edge.length)

    ## Ladderise the edge table
    edge_table <- edge_table[order(edge_table[, 1]), ]

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
                traits = one_events$modification(traits, start = one_parent_trait_value),
                events = NULL,
                replicates = replicates))
}