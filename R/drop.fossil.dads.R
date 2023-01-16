#' @title Drop things from a tree
#' @name drop.things
#' @aliases drop.fossils drop.livings drop.singletons
#'
#' @description Remove fossils or living species or non-bifurcating nodes (singletons) from \code{dads} objects or \code{phylo} objects.
#'
#' @param dads \code{dads} data.
#' @param what what to drop. Can be \code{"fossils"}, \code{"livings"} or \code{"singletons"} (non-bifurcating nodes).
#' @param tol a numeric value giving the tolerance to consider a species as extinct.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

drop.things <- function(dads,  what, tol = 1e-8) {
    switch(what,
        "fossils"    = return(drop.fossils(dads, tol)),
        "singletons" = return(drop.singletons(dads)),
        "livings"    = return(drop.livings(dads))
    )
}

drop.fossils <- function(phy, tol = 1e-8) {

    if(class(phy) == "phylo") {
        ## Simply run drop.fossil
        return(ape::drop.fossil(phy, tol = tol))
    } else {
        ## Drop fossils from the tree
        phy$tree <- ape::drop.fossil(phy$tree, tol = tol)
        ## Drop fossils from the data
        phy$data <- phy$data[rownames(phy$data) %in% c(phy$tree$tip.label, phy$tree$node.label) , , drop = FALSE]
        return(phy)
    }
}

drop.singletons <- function(phy) {

    #TG: TODO!

    if(class(phy) == "phylo") {
        ## Simply run drop.fossil
        return(ape::drop.fossil(phy, tol = tol))
    } else {
        ## Drop fossils from the tree
        phy$tree <- ape::drop.fossil(phy$tree, tol = tol)
        ## Drop fossils from the data
        phy$data <- phy$data[rownames(phy$data) %in% c(phy$tree$tip.label, phy$tree$node.label) , , drop = FALSE]
        return(phy)
    }
}

drop.livings <- function(phy) {

    #TG: TODO!

    if(class(phy) == "phylo") {
        ## Simply run drop.fossil
        return(ape::drop.fossil(phy, tol = tol))
    } else {
        ## Drop fossils from the tree
        phy$tree <- ape::drop.fossil(phy$tree, tol = tol)
        ## Drop fossils from the data
        phy$data <- phy$data[rownames(phy$data) %in% c(phy$tree$tip.label, phy$tree$node.label) , , drop = FALSE]
        return(phy)
    }
}