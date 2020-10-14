#' @title Diversity and disparity simulator
#'
#' @description Simulating diversity and trait disparity
#'
#' @param speciation The speciation parameter (birth/lambda; default = 1)
#' @param extinction The extinction parameter (death/my; default = 0)
#' @param stop.rule  The rules on when to stop the simulation
#' @param traits     The dads traits object (see make.traits)
#' @param modifiers  The dads modifiers object (see make.modifiers)
#' @param events     The dads events object (see make.events)
#' 
#' @examples
#' dads(1)
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

dads <- function(speciation, extinction, stop.rule, traits, modifiers, events) {

    ## Sanitizing
    # speciation and extinction
    # Must be a single numeric value between 0 and 1
    
    # stop.rule
    # Must be a named list

    # traits modifiers events
    # Must be a traits modifiers events object


    output <- 42
    class(output) <- "dads"
    return(output)
}