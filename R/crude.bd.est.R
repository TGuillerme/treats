#' @title Crudely estimates extinction and speciation
#' @name crude.bd.est
#'
#' @description Crudely estimates the extinction and speciaton rate of a tree based on \code{geiger::bd.km} and \code{geiger::bd.ms} 
#'
#' @param tree a \code{"phylo"} object.
#' @param method either \code{"count"} or \code{"estimate"}. See details.
#' @param ... any additional arguments to be passed to \code{geiger::bd.km} and \code{geiger::bd.ms}.
#' 
#' @details This function calculates the extinction and speciation rates using two methods:
#' \itemize{
#'      \item \code{"estimate"} estimates the rates using the algorithm from \code{geiger::bd.km} and \code{geiger::bd.ms} based on the Magallon and Sanderson 2000 method. Note that this function provides more of a "guestimate" of extinction and speciation rates which can be especially wrong with low sampling (either missing fossil or living data). This can lead to estimating erroneous negative extinction rates.
#'      \item \code{"count"} This function calculates the extinction rate as the logged number of extinction events in the tree divided by the tree age (expressed in tree age units - e.g. million years). The speciation rate is calculated as the logged number of speciation events divided by the tree age. If the input tree has no \code{$root.time} element, the speciation and extinction rate are just the number of speciation and extinction events. Although \emph{very crude} this method is slightly better at handling under sampled trees.
#'}
#' For more accurate model base approaches see for example \code{\link[ape]{birthdeath}} or \code{\link[ape]{bd.ext}}.
#'
#' @return
#' A \code{"bd.params"} object to be fed to \code{\link{treats}}.
#'
#' @examples 
#' set.seed(1)
#' ## Generating a random tree
#' my_tree <- rcoal(20)
#' ## Estimate the number of speciations and extinctions events
#' crude.bd.est(my_tree, method = "estimate")
#'
#' ## Adding a root time
#' my_tree$root.time <- 5
#' ## Count the number of speciations and extinctions
#' ## per units of time
#' crude.bd.est(my_tree, method = "count")
#'
#' @seealso \code{\link{treats}} \code{\link{make.bd.params}}
#'
#' @references Magallon S and MJ Sanderson. 2000. Absolute diversification rates in angiosperm clades. Evolution 55:1762-1780.
#' @author Thomas Guillerme
#' @export

crude.bd.est <- function(tree, method, ...) {

    dots <- list(...)

    ## Check class
    check.class(tree, "phylo")
    check.method(method, c("count", "estimate"))

    ## Has a root time?
    if(is.null(tree$root.time)) {
        tree$root.time <- 1
    }

    if(method == "estimate") {
        ## Change root time to root.edge for geiger
        phy <- tree
        phy$root.edge <- tree$root.time

        args <- dots
        args$phy <- phy
        if(is.null(args$crown)) {
            args$crown <- phy$root.edge == 0
        }

        ## Get the speciation rate
        speciation <- do.call(geiger::bd.km, args)

        ## Get the extinction rate 
        diversification <- do.call(geiger::bd.ms, args)#  (diversification = speciation - extinction)
        extinction <- speciation - diversification 
    } else {
        ## Get the elements ages
        tree_ages <- dispRity::tree.age(tree)
        time <- tree$root.time-min(tree_ages[, 1])

        ## Get the speciation rate
        speciation <- log(Nnode(tree))/time

        ## Get the extinction rate
        extinct <- sum(tree_ages[1:Ntip(tree), 1] != min(tree_ages[, 1]))
        if(extinct == 0) {
            extinction <- 0
        } else {
            extinction <- log(extinct)/time
        }
    }

    ## Get the estimations
    return(make.bd.params(speciation = speciation, extinction = extinction))
}
