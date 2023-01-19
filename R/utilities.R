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
#' @examples
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

#' @title Get a snapshot of the tree
#'
#' @description Creates (and saves) a snapshot of the tree at a particular time point
#'
#' @param time          The time of the snapshot
#' @param lineage       The internal lineage data list
#' @param edge.lengths  The internal edge.lengths tracker
#' @param trait.values  Optional, the internal trait values table
#' @param traits        Optional, the mechanism to generate traits
#' 
#' @details
#' This function is designed to be used internally in \code{dads} to help \code{modifiers}, \code{traits} or \code{events} objects to get a snapshot of the state of the tree at a required time point. 
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export


snapshot <- function(time, lineage, edge.lengths, trait.values, traits) {

    ## First waiting_tim should be implied in the arguments
    time.slice <- first_waiting_time + time
    ## In this specific snapshot case time = time.slice

    ## Save a step by creating singles
    lineage      <- update.single.nodes(lineage)
    edge_lengths <- update.single.edges(time, time.slice, lineage, edge.lengths)
    if(!missing(trait.values)) {
        trait_values <- update.single.traits(trait.values, traits, lineage, edge.lengths)
    }
    return(list(lineage = lineage, edge_lengths = edge_lengths, trait_values = trait_values))
}