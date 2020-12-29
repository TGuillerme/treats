## Utility functions for dads

#' @title Get parent traits
#'
#' @description An internal utility function for \code{modifiers}, \code{traits} or \code{events} to access the value(s) of the parent traits in the \code{dads} algorithm
#'
#' @param trait.values   The internal table of trait values
#' @param lineage        The internal lineage data list
#' @param current        Whether to consider only the current lineage (\code{TRUE} - default) or all the living lineages (\code{FALSE}).
#' 
#' @details
#' This function is designed to be used internally in \code{dads} to help \code{modifiers}, \code{traits} or \code{events} objects to access the parent traits of the lineages simulated through the internal birth death algorithm. 
#' 
# @examples
# ## You can use this function in a modifiers object condition:
# 
# ## 1. designing the condition function:
# ## Return FALSE if the trait value is less than the average trait value
# my_condition <- function(trait.values, lineage) {
# }
#'
#' @seealso
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
