#' @title Crudely estimates extinction and speciation
#' @name crude.bd.est
#'
#' @usage crude.bd.est(tree)
#'
#' @description Very crudely estimates the extinction and speciaton rate of a tree.  
#'
#' @param tree a \code{"phylo"} object.
#' 
#' @details This function calculates the extinction rate as the number of extinction events in the tree divided by the tree age (expressed in tree age units - e.g. million years). The speciation rate is calculated as the number of speciation events divided by the tree age. If the input tree has no \code{$root.time} element, the speciation and extinction rate are just the number of speciation and extinction events.
#' \emph{NOTE} that this function is a very crude way to estimate rates of extinction and speciation: the rates are calculated from the raw data (not estimated), ignore sampling biases of the data and the variability in rates. For correct model base approaches see for example \code{\link[ape]{birthdeath}} or \code{\link[ape]{bd.ext}}. 
#'
#' @return
#' A \code{"bd.params"} object to be fed to \code{\link{treats}}.
#'
#' @examples 
#' set.seed(1)
#' ## Generating a random tree
#' my_tree <- rtree(20)
#' ## Calculating the number of speciations and extinctions events
#' crude.bd.est(my_tree)
#'
#' ## Adding a root time
#' my_tree$root.time <- 5
#' ## Calculating the number of speciations and extinctions
#' ## per units of time
#' crude.bd.est(my_tree)
#'
#' @seealso \code{\link{treats}} \code{\link{make.bd.params}}
#' 
#' @author Thomas Guillerme
#' @export

crude.bd.est <- function(tree) {
    ## Check class
    check.class(tree, "phylo")

    ## Has a root time?
    if(is.null(tree$root.time)) {
        tree$root.time <- 1
    }

    ## Get the elements ages
    tree_ages <- dispRity::tree.age(tree)

    ## Get the speciation rate
    speciation <- Nnode(tree)/(tree$root.time-min(tree_ages[, 1]))

    ## Get the extinction rate
    extinction <- sum(tree_ages[1:Ntip(tree), 1] != min(tree_ages[, 1]))/(tree$root.time-min(tree_ages[, 1]))

    ## Get the estimations
    return(make.bd.params(speciation = speciation, extinction = extinction))
}
