#' @title Drop things from a treats object
#' @name drop.things
#' @aliases drop.fossils drop.livings drop.singles
#'
#' @usage drop.things(treats, what)
#' @usage drop.fossils(treats)
#' @usage drop.livings(treats)
#' @usage drop.singles(treats)
#'
#' @description Remove fossils or living species or non-bifurcating nodes (singles) from \code{treats} objects or \code{phylo} objects.  
#'
#' @param treats \code{treats} data.
#' @param what what to drop. Can be \code{"fossils"}, \code{"livings"} or \code{"singles"} (non-bifurcating nodes).
#' 
#' @details \emph{NOTE} that dropping living or fossils species DOES NOT drop associated internal nodes and edge lengths. To drop both fossil/living taxa AND internal nodes, you can use for example: \code{drop.things(drop.things(my_data, what = "fossils"), what = "singles")}.
#'
#' @return
#' This function outputs either a \code{"phylo"} object if no traits where generated or a \code{treats} object that is a list of at least two elements: \code{$tree}, a \code{"phylo"} object and \code{$data}, a \code{"matrix"} of the trait values.
#'
#' @examples
#' ## A random tree with fossils and traits and internal nodes every 0.5 times
#' set.seed(3)
#' my_data <- treats(stop.rule = list(max.taxa = 20),
#'                 bd.params = list(speciation = 1, extinction = 1/3),
#'                 traits    = make.traits(), save.steps = 0.5)
#'
#' ## A tree with 20 tips and 54 nodes
#' my_data$tree
#' ## And a dataset with 74 rows
#' dim(my_data$data)
#' 
#' ## Removing the fossil species
#' drop.things(my_data, what = "fossils")$tree
#' dim(drop.fossils(my_data)$data)
#'
#' ## Removing the living species
#' drop.things(my_data, what = "livings")$tree
#' dim(drop.livings(my_data)$data)
#'
#' ## Removing the internal nodes
#' drop.things(my_data, what = "singles")$tree
#' dim(drop.singles(my_data)$data)
#'
#' ## Removing the internal nodes AND the fossils
#' drop.singles(drop.fossils(my_data))
#'
#' @seealso \code{\link{treats}} \code{\link{plot.treats}}
#' 
#' @author Thomas Guillerme
#' @export
drop.singles <- function(treats) {

    ## Check if treats has replicates
    if((class_out <- is.replicates(treats)) != "no") {
        output <- lapply(treats, drop.singles)
        class(output) <- class_out
        return(output)
    }

    if(is(treats, "phylo")) {
        ## Add a dummy tip
        dummy_tree <- treats + rtree(1, tip.label = "internal:drop.singles:dummy_tip_to_remove")
        ## Remove that tip and clean singles
        return(drop.tip(dummy_tree, tip = "internal:drop.singles:dummy_tip_to_remove", trim.internal = TRUE, collapse.singles = TRUE))
    } else {
        ## Add a dummy tip
        dummy_tree <- treats$tree + rtree(1, tip.label = "internal:drop.singles:dummy_tip_to_remove")
        ## Remove that tip and clean singles
        treats$tree <- drop.tip(dummy_tree, tip = "internal:drop.singles:dummy_tip_to_remove", trim.internal = TRUE, collapse.singles = TRUE)
        ## Drop nodes from the data
        treats$data <- treats$data[rownames(treats$data) %in% c(treats$tree$tip.label, treats$tree$node.label) , , drop = FALSE]
        return(treats)
    }
}

drop.tips <- function(treats, living) {

    ## Check if treats has replicates
    if((class_out <- is.replicates(treats)) != "no") {
        output <- lapply(treats, drop.tips, living)
        class(output) <- class_out
        return(output)
    }

    ## Find the tips
    tree_ages <- if(is(treats, "phylo")) {tree.age(treats)} else {tree.age(treats$tree)}
    ntips <- ifelse(is(treats, "phylo"), Ntip(treats), Ntip(treats$tree))
    if(living) {
        tips <- tree_ages$elements[1:ntips][which(tree_ages$ages[1:ntips] == 0)]
    } else {
        tips <- tree_ages$elements[1:ntips][which(tree_ages$ages[1:ntips] != 0)]
    }
    ## No tips to remove
    if(length(tips) == 0){
        return(treats)
    }

    ## Dropping tips for the tree
    if(is(treats, "phylo")) {
        return(ape::drop.tip(treats, tip = tips, collapse.singles = FALSE))
    }

    ## Dropping tips for the whole object
    treats$tree <- ape::drop.tip(treats$tree, tip = tips, collapse.singles = FALSE)
    ## Drop tips from the data
    treats$data <- treats$data[rownames(treats$data) %in% c(treats$tree$tip.label, treats$tree$node.label) , , drop = FALSE]
    return(treats)
}
drop.fossils <- function(treats) {
    return(drop.tips(treats, living = FALSE))
}
drop.livings <- function(treats) {
    return(drop.tips(treats, living = TRUE))
}
drop.things <- function(treats, what) {
    check.class(treats, c("multiPhylo", "phylo", "treats"))
    check.class(what, "character")
    check.method(what, c("fossils", "livings", "singles"))
    switch(what,
        "fossils" = return(drop.fossils(treats)),
        "livings" = return(drop.livings(treats)),
        "singles" = return(drop.singles(treats))
    )
}

## Checking treats replicates
is.replicates <- function(treats) {
    ## Check it's a treats
    if(is(treats, "treats")) {
        ## Check if it's a list of treats or just a treats
        classes <- unique(unlist(lapply(treats, class)))
        if(length(classes) == 1 && classes == "treats") {
            ## It's a replicate containing treats
            return("treats")
        } else {
            ## It's a single treats
            return("no")
        }
    } else {
        if(is(treats, "phylo")) {
            ## It's a single treats
            return("no")
        } else {
            if(is(treats, "multiPhylo")) {
                return("multiPhylo")
            }
        }
    }
    return("no")
}