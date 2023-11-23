#' @title Maps a trait on a tree
#'
#' @description Simulates one or more trait specified through a "traits" onto one or multiple trees.
#'
#' @param traits A \code{"traits"} object (see \code{\link{make.traits}}).
#' @param tree   A \code{"phylo"} or \code{"multiPhylo"} object.
#'
#' @return
#' A \code{"treats"} object containing the tree and the traits.
#' 
#' @details  
#' This function simulates the trait(s) on the tree using the tree's branch length.
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
map.traits <- function(traits, tree) {
    ## Sanitizing
    check.class(traits, c("treats", "traits"), " must be of class \"traits\". You can generate such object using:\nmake.traits()")
    tree_class <- check.class(tree, c("phylo", "multiPhylo"))
    if(tree_class == "phylo") {
        tree <- list(tree)
    }
    ## Check for edge lengths
    if(any(unlist(lapply(tree, function(x) is.null(x$edge.length))))) {
        stop(paste0("The input tree", ifelse(tree_class == "phylo", " has", "s have"), " no branch lengths."))
    }

    ## Ignore the background trait
    if(!is.null(traits$background)) {
        warning("Background traits cannot be mapped and is ignored.")
    }

    ## Map the traits
    all_traits <- lapply(tree, map.traits_fun, traits = traits)

    ## Output
    output <- lapply(all_traits, function(x) make.treats(x$tree, x$data))
    if(tree_class == "phylo") {
        return(output[[1]])
    } else {
        return(output)
    }
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
    nodes_table <- edge_table[which(edge_table[,2] > Ntip(tree)), ]
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