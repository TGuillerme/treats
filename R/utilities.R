## Utility functions for treats

#' @title Get parent traits
#'
#' @description An internal utility function for \code{modifiers}, \code{traits} or \code{events} to access the value(s) of the parent traits in the \code{treats} algorithm
#'
#' @param trait.values   The internal table of trait values
#' @param lineage        The internal lineage data list
#' @param current        Whether to consider only the current lineage (\code{TRUE} - default) or all the living lineages (\code{FALSE}).
#' 
#' @details
#' This function is designed to be used internally in \code{treats} to help \code{modifiers}, \code{traits} or \code{events} objects to access the parent traits of the lineages simulated through the internal birth death algorithm. 
#' 
#' @examples
#' ## Speciation event is more likely if lineage's ancestor is further away from the mean trait value
#' distance.modify <- function(x, trait.values, lineage) {
#'      ## Distance to the parent's trait
#'      parent_trait_val <- parent.traits(trait.values, lineage)[1]
#'      mean_trait_val <- mean(trait.values[, 1])
#'      distance <- abs(parent_trait_val - mean_trait_val)
#'      ## Scales x with the distance
#'      return(x + x * distance)
#' }
#' 
#' ## Make a distance modifier (speciation more likely with distance)
#' distance.speciation <- make.modifiers(speciation = speciation,
#'                                       modify = distance.modify)
#'
#' @seealso \code{\link{treats}} \code{\link{make.modifiers}}
#' 
#' @author Thomas Guillerme
#' @export

parent.traits <- function(trait.values, lineage, current = TRUE) {
    if(current) {
        ## Find only the current lineage
        find <- lineage$parents[lineage$current]
    } else {
        ## Find all the descendants from living lineages
        find <- unique(cbind(seq_along(lineage$split), lineage$parents)[lineage$livings, , drop = FALSE][, 2])
    }

    return(trait.values[as.numeric(rownames(trait.values)) %in% find, , drop = FALSE])
}
