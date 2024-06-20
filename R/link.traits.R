#' @title link.traits
#'
#' @description Linking traits objects together to simulate simulate them sequentially.
#'
#' @param base.trait    One or more \code{"treats"} \code{"traits"} object(s) to be considered first.
#' @param next.trait    One or more \code{"treats"} \code{"traits"} object(s) to be considered sequentially.
#' @param link.type     The type of link between the traits. Can be \code{"conditional"}.
#' @param link.args     Optional arguments to interpret the link between the objects (based on the \code{link.type}).
# @param trait.names   Optional, the name(s) of the process(s). 
#'
#' @details
#' This function allows to link several traits together in the simulations. The current link types implemented are:
#' \itemize{
#'      \item{"conditional"}: this allows to link the \code{next.trait} traits conditionally to the \code{base.trait} one. For example if \code{base.trait} is a \code{\link{discrete.process}} with two states \code{0} and \code{1} and \code{next.trait} is a list of two traits with two different processes \code{\link{OU.process}} and \code{\link{BM.process}}. The simulations generates a first trait using \code{base.trait} and then a second one using one of the two processes in \code{next.trait} depending on the results of \code{base.trait}. The link arguments \code{link.args} must be a list of logical functions to interpret \code{x1}, the results of the \code{base.trait}. For example, \code{list(function(x1){x1 == 0}, function(x1){x1 == 1})} will generate a trait using the first \code{next.trait} if \code{x1} is equal to \code{0} or using the second \code{next.trait} if \code{x1} is equal to \code{1}.
# }
#'
#' @return
#' This function outputs a \code{treats} object that is a named list of elements handled internally by the \code{\link{treats}} function.
#'
#' @examples
#'
#' @seealso \code{\link{treats}} \code{\link{trait.process}} \code{\link{make.traits}}
#' 
#' @author Thomas Guillerme
#' @export


link.traits <- function(base.trait, next.trai, link.type, link.args) {
    return(NULL)
}