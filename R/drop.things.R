#' @title Drop things from a tree
#' @name drop.things
#' @aliases drop.fossils drop.livings drop.singles
#'
#' @description Remove fossils or living species or non-bifurcating nodes (singles) from \code{dads} objects or \code{phylo} objects.  
#'
#' @param dads \code{dads} data.
#' @param what what to drop. Can be \code{"fossils"}, \code{"livings"} or \code{"singles"} (non-bifurcating nodes).
#' 
#' @details \emph{NOTE} that dropping living or fossils species DOES NOT drop associated internal nodes and edge lengths. To drop both fossil/living taxa AND internal nodes, you can use for example: \code{drop.things(drop.things(my_data, what = "fossils"), what = "singles")}.
#'
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

drop.singles <- function(dads) {
    if(is(dads, "phylo")) {
        ## Add a dummy tip
        dummy_tree <- dads + rtree(1, tip.label = "internal:drop.singles:dummy_tip_to_remove")
        ## Remove that tip and clean singles
        return(drop.tip(dummy_tree, tip = "internal:drop.singles:dummy_tip_to_remove", trim.internal = TRUE, collapse.singles = TRUE))
    } else {
        ## Add a dummy tip
        dummy_tree <- dads$tree + rtree(1, tip.label = "internal:drop.singles:dummy_tip_to_remove")
        ## Remove that tip and clean singles
        dads$tree <- drop.tip(dummy_tree, tip = "internal:drop.singles:dummy_tip_to_remove", trim.internal = TRUE, collapse.singles = TRUE)
        ## Drop nodes from the data
        dads$data <- dads$data[rownames(dads$data) %in% c(dads$tree$tip.label, dads$tree$node.label) , , drop = FALSE]
        return(dads)
    }
}
drop.tips <- function(dads, living) {
    ## Find the tips
    tree_ages <- if(is(dads, "phylo")) {tree.age(dads)} else {tree.age(dads$tree)}
    ntips <- ifelse(is(dads, "phylo"), Ntip(dads), Ntip(dads$tree))
    if(living) {
        tips <- tree_ages$elements[1:ntips][which(tree_ages$ages[1:ntips] == 0)]
    } else {
        tips <- tree_ages$elements[1:ntips][which(tree_ages$ages[1:ntips] != 0)]
    }
    ## No tips to remove
    if(length(tips) == 0){
        return(dads)
    }

    ## Dropping tips for the tree
    if(is(dads, "phylo")) {
        return(ape::drop.tip(dads, tip = tips, collapse.singles = FALSE))
    }

    ## Dropping tips for the whole object
    dads$tree <- ape::drop.tip(dads$tree, tip = tips, collapse.singles = FALSE)
    ## Drop tips from the data
    dads$data <- dads$data[rownames(dads$data) %in% c(dads$tree$tip.label, dads$tree$node.label) , , drop = FALSE]
    return(dads)
}
drop.fossils <- function(dads) {
    return(drop.tips(dads, living = FALSE))
}
drop.livings <- function(dads) {
    return(drop.tips(dads, living = TRUE))
}
drop.things <- function(dads, what) {

    check.class(dads, c("phylo", "dads"))
    check.class(what, "character")
    check.method(what, c("fossils", "livings", "singles"))

    switch(what,
        "fossils"    = return(drop.fossils(dads)),
        "livings"    = return(drop.livings(dads)),
        "singles"  = return(drop.singles(dads))

    )
}
