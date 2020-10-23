#' @title Drop fossils
#'
#' @description Remove fossils from \code{dads} objects or \code{phylo} objects (using \code{\link[ape]{drop.fossil}}).
#'
#' @param phy \code{dads} data.
#' @param tol a numeric value giving the tolerance to consider a species as extinct.
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

drop.fossil.dads <- function(phy, tol = 1e-8) {

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