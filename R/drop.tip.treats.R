#' @title drop.tip.treats
#' @aliases keep.tip.treats
#'
#' @description Drop or keep tips from a \code{"treats"} object.
#'
#' @param phy   an object of class \code{"treats"}.
#' @param tip   a vector of mode numeric or character specifying the tips to delete or to keep.
#' @param ...   any additional argument to be passed to \code{\link[ape]{drop.tip.phylo}}. 
#'
#' @details
#' This function allows to remove or keep tips from a \code{"treats"} object the same way as the \code{\link[ape]{drop.tip.phylo}} function.
#'
#' @return
#' This function outputs either a \code{"phylo"} object if no traits where generated or a \code{treats} object that is a list of at least two elements: \code{$tree}, a \code{"phylo"} object and \code{$data}, a \code{"matrix"} of the trait values.
#'
#' @examples
#'## A treats object with one trait and 20 tips
#'my_treats <- treats(stop.rule = list(max.taxa = 20),
#'                    traits = make.traits())
#'
#'## Removing five tips
#'drop.tip.treats(my_treats, tip = c("t1", "t2", "t3", "t4", "t5"))
#'
#'## Keeping these five tips
#'drop.tip.treats(my_treats, tip = c("t1", "t2", "t3", "t4", "t5"))
#'
#' @seealso \code{\link{treats}}
#' 
#' @author Thomas Guillerme
#' @export

drop.tip.treats <- function(phy, tip, ...) {
    ## Drop the tips in the tree
    tree <- drop.tip.phylo(phy$tree, tip, ...)
    ## Output the updated object
    return(match.tree.to.data(phy, tree))
}

keep.tip.treats <- function(phy, tip, ...) {
    ## Keep the tips in the tree
    tree <- keep.tip.phylo(phy$tree, tip, ...)
    ## Output the updated object
    return(match.tree.to.data(phy, tree))
}

match.tree.to.data <- function(treats, tree) {
    ## Match the ones in the data
    to_keep <- rownames(treats$data) %in% c(tree$node.label, tree$tip.label)
    data <- treats$data[to_keep, , drop = FALSE]
    ## Update the object
    treats$tree <- tree
    treats$data <- data
    class(treats) <- "treats"
    return(treats)
}
