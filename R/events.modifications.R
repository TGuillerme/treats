#' @name events.modifications
#' @aliases random.extinction trait.extinction bd.params.update traits.update modifiers.update founding.event
#' @title Events modifications
#'
#' @description Inbuilt modifications functions for helping designing events
#'
#' @usage events.modification(x, ...)
#'
#' @param x   a numerical value to update.
#' @param ... any specific argument for the modification (see details).
#' 
#' @return
#' This function outputs a \code{"function"} to be passed to \code{\link{make.events}}.
#'
#' @details
#' The following functions allow to design specific modifications for events:
#' 
#' \itemize{
#'      
#' \item modifications for the target \code{"taxa"}
#'      \itemize{
#'          \item \code{random.extinction}: this function removes (makes extinct) a proportion of living taxa when the event is triggered. The proportion of taxa to remove can be changed with the argument \code{x}.
#'          \item \code{trait.extinction}: this function removes (makes extinct) a number of living taxa based on their trait(s) values when the event is triggered. The trait value is specified with the argument \code{x}.This function has one optional argument:
#'          \itemize{
#'              \item{condition} to specify the condition in relation to that trait value (the default is \code{condition = `<`} meaning taxa with a trait value lower that \code{x} will go extinct).
#'              \item{trait} to specify which trait will be affected (the default is \code{trait = 1}, meaning it will only consider the first trait).
#'          }
#' 
#'      }
#' 
#' \item modifications for the target \code{"bd.params"}
#'      \itemize{
#'          \item \code{bd.params.update}: this function updates a \code{"bd.params"} object within the birth death process. It takes any unambiguous named argument to be passed to \code{\link{make.bd.params}}. For example, to update the speciation from any current rate to a new rate of 42, you can use \code{bd.params.update(speciation = 42)}.
#'      }
#'
#' \item modifications for the target \code{"traits"} 
#'      \itemize{
#'          \item \code{traits.update}: this function updates a \code{"traits"} object within the birth death process. It takes any unambiguous named argument to be passed to \code{\link{make.traits}}. For example, to update the trait process from the current one to an OU process, you can use \code{traits.update(process = OU.process)}.
#'      }
#' 
#' \item modifications for the target \code{"modifiers"}
#'      \itemize{
#'          \item \code{modifiers.update}: this function updates a \code{"modifiers"} object within the birth death process. It takes any unambiguous named argument to be passed to \code{\link{make.modifiers}}. For example, to update the speciation from the current process to be dependent to trait values, you can use \code{modifiers.update(speciation = speciation.trait)}.
#'      }
#' 
#' \item modifications for the target \code{"founding"} 
#'      \itemize{
#'          \item \code{founding.event}: this function runs an independent birth-death process when the condition is met. This function takes any of the arguments normally passed to \code{\link{treats}} (\code{"bd.params"}, \code{"traits"}, \code{"modifiers"} and \code{"events"}). The \code{stop.rule} and other arguments are handled internally: namely the \code{stop.rule} argument is updated to match the time and number of taxa when the founding event is triggered. \emph{Note that this can lead to the simulation stopping just before reaching the \code{max.taxa} or \code{max.living} stop rule}.
#'      }
#' }
#' 
#' More details about the \code{events} functions is explained in the \code{treats} manual: \url{http://tguillerme.github.io/treats}.
#' 
#' @examples
#' ## Generating a mass extinction
#' ## 80% mass extinction at time 4
#' mass_extinction <- make.events(
#'                       target       = "taxa",
#'                       condition    = age.condition(4),
#'                       modification = random.extinction(0.8))
#' 
#' ## Set the simulation parameters
#' stop.rule <- list(max.time = 5)
#' bd.params <- list(extinction = 0, speciation = 1)
#' 
#' ## Run the simulations
#' set.seed(123)
#' results <- treats(bd.params = bd.params,
#'                 stop.rule = stop.rule,
#'                 events    = mass_extinction)
#' ## Plot the results
#' plot(results, show.tip.label = FALSE)
#' axisPhylo()
#' 
#' ## Changing the trait process
#' ## The 95% upper quantile value of a distribution
#' upper.95 <- function(x) {
#'     return(quantile(x, prob = 0.95))
#' } 
#' ## Create an event to change the trait process
#' change_process <- make.events(
#'                   target       = "traits",
#'                   ## condition is triggered if(upper.95(x) > 3)
#'                   condition    = trait.condition(3, condition = `>`, what = upper.95),
#'                   modification = traits.update(process = OU.process))
#' 
#' ## Set the simulation parameters
#' bd.params <- list(extinction = 0, speciation = 1)
#' stop.rule <- list(max.time = 6)
#' traits    <- make.traits()
#' 
#' ## Run the simulations
#' set.seed(1)
#' no_change <- treats(bd.params = bd.params,
#'                   stop.rule = stop.rule,
#'                   traits    = traits)
#' set.seed(1)
#' process_change <- treats(bd.params = bd.params,
#'                        stop.rule = stop.rule,
#'                        traits    = traits,
#'                        events    = change_process)
#' ## Plot the results
#' oldpar <- par(mfrow = c(1,2))
#' plot(no_change, ylim = c(-7, 7))
#' plot(process_change, ylim = c(-7, 7))
#' par(oldpar)
#' 
#' 
#' @seealso \code{\link{treats}} \code{\link{make.events}} \code{\link{events.conditions}}
#' 
#' @author Thomas Guillerme

## The list of conditions
events.modification <- function(x, ...) {
    message("List of inbuilt modification functions in treats:")
    message("For the taxa target:")
    message("   ?random.extinction")
    message("   ?trait.extinction")
    message("For the bd.params target:")
    message("   ?bd.params.update")
    message("For the traits target:")
    message("   ?traits.update")
    message("For the modifiers target:")
    message("   ?modifiers.update")
    message("For the founding target:")
    message("   ?founding.event")
    return(invisible())
} 

## Random mass extinction modification
# random.extinction <- function(x) {
#     ## The function prototype
#     extinction.variable <- function(bd.params, lineage, trait.values) {
#         ## Set the variable
#         extinction_strength <- NULL

#         ## Select a portion of the living species to go extinct
#         extinct <- sample(lineage$n, round(lineage$n * extinction_strength))

#         ## Update the lineage object
#         lineage$livings <- lineage$livings[-extinct]
#         lineage$n       <- lineage$n - length(extinct)
#         return(lineage)
#     }

#     ## Editing the extinction strength
#     body(extinction.variable)[[2]][[3]] <- eval(substitute(x))
#     return(extinction.variable)
# }

# stop("DEBUG")
## 1- Get the current environment
## 2- Generate the function
## 3- Send the function to the previous environment
# stop("DEBUG")
# power1 <- function(exp) {
#     function(x) {
#         x ^ exp
#     }
# }

# square <- power1(2)
# cube <- power1(3)
# square(3)
# cube(3)

## Random mass extinction modification
random.extinction <- function(x){

    ## The function prototype
    extinction.variable <- function(bd.params, lineage, trait.values) {
        
        ## Select a portion of the living species to go extinct
        extinct <- sample(lineage$n, round(lineage$n * x))

        ## Update the lineage object
        lineage$livings <- lineage$livings[-extinct]
        lineage$n       <- lineage$n - length(extinct)
        return(lineage)
    }

    return(extinction.variable)
}

## Mass extinction based on traits modification
trait.extinction <- function(x, condition = `<`, trait = 1) {

    ## Function for extinction trait
    extinction.trait <- function(bd.params, lineage, trait.values) {
        ## Select the nodes be traits
        parent_traits <- parent.traits(trait.values, lineage, current = FALSE)
        selected_nodes <- as.numeric(names(which(condition(parent_traits[, trait], x))))

        ## Select the descendants that'll go extinct
        extinct <- which(lineage$parents %in% selected_nodes)

        ## Update the lineage object
        lineage$livings <- lineage$livings[!lineage$livings %in% extinct]
        lineage$n       <- length(lineage$livings)
        return(lineage)
    }
    return(extinction.trait)
}

## Updating the bd.params
bd.params.update <- function(x, speciation = NULL, extinction = NULL, joint = NULL, absolute = NULL, speciation.args = NULL, extinction.args = NULL) {

    change.bd.params <- function(traits, bd.params, lineage, trait.values) {
        ## Changing the traits
        return(make.bd.params(update          = bd.params,
                              speciation      = speciation,
                              extinction      = extinction,
                              joint           = joint,
                              absolute        = absolute,
                              speciation.args = speciation.args,
                              extinction.args = extinction.args))
    }

    ## Editing the update bd.params function
    return(change.bd.params)
}

## Updating a traits object
traits.update <- function(x, process = NULL, process.args = NULL, trait.names = NULL) {

    change.traits <- function(traits, bd.params, lineage, trait.values) {
        ## Changing the traits
        return(make.traits(update       = traits,
                           process      = process,
                           process.args = process.args,
                           trait.names  = trait.names))
    }

    return(change.traits)
}

## Updating a modifiers object
modifiers.update <- function(x, branch.length = NULL, selection = NULL, speciation = NULL, condition = NULL, modify = NULL) {

    change.modifiers <- function(modifiers, bd.params, lineage, trait.values) {
        ## Setting the variables
        return(make.modifiers(update        = modifiers,
                              branch.length = branch.length,
                              selection     = selection,
                              speciation    = speciation,
                              condition     = condition,
                              modify        = modify))
    }
    return(change.modifiers)
}

## Founding events
founding.event <- function(x, bd.params = NULL, traits = NULL, modifiers = NULL, events = NULL) {

    ## founding events
    founding.fun <- function(stop.rule, time, lineage) {
        ## Update the stop rule
        stop_rule_updated <- stop.rule
        if(stop_rule_updated$max.time != Inf) {
            stop_rule_updated$max.time <- stop_rule_updated$max.time - time
        }
        if(stop_rule_updated$max.living != Inf) {
            stop_rule_updated$max.living <- stop_rule_updated$max.living - lineage$n
        }
        if(stop_rule_updated$max.taxa != Inf) {
            stop_rule_updated$max.living <- stop_rule_updated$max.taxa - sum(!lineage$split)
        }

        ## Run the founding event
        return(birth.death.tree.traits(stop.rule = stop_rule_updated, bd.params, traits, modifiers, events, check.results = FALSE))
    }

    return(founding.fun)
}

