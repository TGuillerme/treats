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
#' @return Returns one or more \code{"numeric"} values.
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


#' @title Makes a transition matrix
#'
#' @description Utility function for generating discrete characters evolution transition matrices. 
#'
#' @param type the type of transition matrix, either "equal rates", "stepwise", "symmetric", or  "all rates different". See details.
#' @param states the number of states.
#' @param rates either a fixed value for a rate to attribute to each possible transitions or a \code{function} to generate the rates (default is \code{\link[stats]{runif}}). See details.
#' @param self logical, whether to allow reverting states (i.e. transition rates from state A to the same state A; \code{TRUE}; default) or not (\code{FALSE}).
# @param state.names optional, the names of the states.
#' @param ... if \code{rates} is a function, any optional arguments to be passed to it.
#'
#' @details
#' The following transition rate matrices are currently implemented:
#' \itemize{
#'      \item "equal rates" (or "ER") where all transitions are equal (including no transition if \code{self = TRUE}).
#'      \item "stepwise" (or "Dollo") transitions are allowed only in a step wise way (e.g. state 1 to 2 and 2 to 3 are allowed but not 1 to 3).
#'      \item "symmetric" ("SYM") where transitions between states are all different but not directional (e.g. the change of state 1 to 2 is equal to 2 to 1). If \code{self = TRUE}, the non transitions (e.g. from state 1 to 1) are equal.
#'      \item "all rates different" (or "ARD") where all transitions are different. Note that if rates is a give value (rather than a function), then all rates are actually equal.
#'}
#'
#' If \code{rates} is a function that generates negative values or a negative value, the output transition matrix always returns absolute values.
#'
#' @return Returns a squared \code{"matrix"}.
#'
#' @examples
#' ## A two states equal rates matrix with a rate of 1
#' ## and no stationary rates (no probability of staying in the same state)
#' transition.matrix(type = "equal rates", states = 2, rates = 1, self = FALSE)
#'
#' ## Two different 6 states stepwise matrix with a random absolute normal rate
#' transition.matrix(type = "stepwise", states = 6, rates = rnorm)
#' transition.matrix(type = "stepwise", states = 6, rates = rnorm)
#'
# ## A transition matrix with states names
# transition.matrix(type = "SYM", states = 3, rates = runif,
#                   state.names = c("char1", "char2", "char3"))
#'
#' @seealso \code{\link{make.traits}} \code{\link{discrete.process}}
#' 
#' @author Thomas Guillerme
#' @export
transition.matrix <- function(type, states, rates = runif, self = TRUE, ...){# state.names, ...) {
    ## Sanitizing
    check.class(type, "character")
    if(!(type %in% c("ER", "Equal rates", "Equal Rates", "equal rates", "SYM", "Symmetric", "symmetric", "ARD", "All rates different", "all rates different", "Dollo", "Stepwise", "stepwise"))) {
        stop(paste("type must be one of the followings: ", paste(c('equal rates', 'symmetric', 'all rates different', 'stepwise'), collapse = ", "), ".", sep = ""))
    }
    check.class(states, c("numeric", "integer"))
    check.class(self, "logical")
    # do_state_names <- FALSE
    # if(!missing(state.names)) {
    #     do_state_names <- TRUE
    #     check.length(state.names, length(states))
    #     check.class(state.names, c("character", "numeric", "integer"))
    # }

    ## Rates
    rates_class <- check.class(rates, c("function", "numeric", "integer"))
    if(rates_class != "function") {
        rates_val <- rates
        rates <- function(...) return(rates_val)
    }

    ## Create the empty states matrix
    rate_matrix <- matrix(0, states, states)

    ## Normalise type names
    if(type == "ER" || type == "Equal rates" || type == "Equal Rates") type <- "equal rates"
    if(type == "SYM" || type == "Symmetric") type <- "symmetric"
    if(type == "ARD" || type == "All rates different") type <- "all rates different"
    if(type == "Dollo" || type == "Stepwise") type <- "stepwise"

    ## Equal rates
    if(type == "equal rates") {
        rate_matrix[,] <- rates(1, ...)
    }
    if(type == "stepwise") {
        diag(rate_matrix) <- 1
        ## Make a fat diagonal
        for(diag_i in 1:(length(diag(rate_matrix))-1)) {
         rate_matrix[diag_i + 1, diag_i] <- rate_matrix[diag_i, diag_i + 1] <- 1
        }
        rate_matrix <- rate_matrix * rates(1, ...)
    }
    if(type == "symmetric") {
        diag(rate_matrix) <- rates(1, ...)
        ## Populate the diagonal
        rate_matrix[lower.tri(rate_matrix)] <- rates(n = sum(lower.tri(rate_matrix)), ...)
        rate_matrix[upper.tri(rate_matrix)] <- t(rate_matrix)[upper.tri(rate_matrix)]
    }
    if(type == "all rates different") {
        ## All rates different
        rate_matrix[,] <- rates(states^2, ...)
    }

    ## no self?
    if(!self) {
        diag(rate_matrix) <- 0
    }

    # ## Add state names
    # if(do_state_names) {
    #     rownames(rate_matrix) <- colnames(rate_matrix) <- state.names
    # }

    return(abs(rate_matrix))
}
