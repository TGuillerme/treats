#' @name events.modifications
#' @aliases modification, random.extinction, trait.extinction, update.bd.params, update.traits, update.modifiers, founding.event
#' @title events.modifications
#'
#' @description Inbuilt modifications functions for helping designing events
#'
#' @usage modification(x, ...)
#' @usage founding.event(x, bd.params = NULL, traits = NULL, modifiers = NULL,
#'                       events = NULL)
#' @usage random.extinction(x)
#' @usage trait.extinction(x, condition = `<`, trait = 1)
#' @usage update.bd.params(x, speciation = NULL, extinction = NULL,
#'                        joint = NULL, absolute = NULL, speciation.args = NULL,
#'                        extinction.args = NULL)
#' @usage update.modifiers(x, branch.length = NULL, selection = NULL,
#'                         speciation = NULL, condition = NULL, modify = NULL)
#' @usage update.traits(x, process = NULL, process.args = NULL,
#'                      trait.names = NULL)
#'
#' @param x   a numerical value to update.
#' @param ... any specific argument for the modification (see details).
#' @param bd.params a \code{"bd.params"} \code{"treats"} object for the founding event (see details for founding.event).
#' @param traits a \code{"traits"} \code{"treats"} object for the founding event (see details for founding.event).
#' @param modifiers a \code{"modifiers"} \code{"treats"} object for the founding event (see details for founding.event).
#' @param events an \code{"events"} \code{"treats"} object for the founding event (see details for founding.event).
#' @param condition a condition function for the trait value (see details for trait.extinction and update.modifiers).
#' @param trait which trait to use (see details for trait.extinction).
#' @param speciation the same argument as in \code{\link{make.bd.params}} (see details for update.bd.params).
#' @param extinction the same argument as in \code{\link{make.bd.params}} (see details for update.bd.params).
#' @param joint the same argument as in \code{\link{make.bd.params}} (see details for update.bd.params).
#' @param absolute the same argument as in \code{\link{make.bd.params}} (see details for update.bd.params).
#' @param speciation.args the same argument as in \code{\link{make.bd.params}} (see details for update.bd.params).
#' @param extinction.args the same argument as in \code{\link{make.bd.params}} (see details for update.bd.params).
#' @param branch.length the same argument as in \code{\link{make.modifiers}} (see details for update.modifiers).
#' @param selection the same argument as in \code{\link{make.modifiers}} (see details for update.modifiers).
#' @param speciation the same argument as in \code{\link{make.modifiers}} (see details for update.modifiers).
#' @param modify the same argument as in \code{\link{make.modifiers}} (see details for update.modifiers).
#' @param process the same argument as in \code{\link{make.modifiers}} (see details for update.modifiers).
#' @param process.args the same argument as in \code{\link{make.modifiers}} (see details for update.modifiers).
#' @param trait.names the same argument as in \code{\link{make.modifiers}} (see details for update.modifiers).
#' 
#' @details
#' The following functions allow to design specific modifications for events:
#' 
#' \itemize{
#'      
#' \item modifications for the target \code{"taxa"}
#'      \itemize{
#'          \item \code{random.extinction}: this function removes (makes extinct) a proportion of living taxa when the event is triggered. The proportion of taxa to remove can be changed with the argument \code{x}. 
#'          \item \code{trait.extinction}: this function removes (makes extinct) a number of living taxa based on their trait(s) values when the event is triggered. The trait value is specified with the argument \code{x}. You can specify the condition in relation to that trait value with \code{condition} (the default is \code{condition = `<`} meaning taxa with a trait value lower that \code{x} will go extinct) and which trait(s) to consider using \code{trait} (the default is \code{trait = 1}, meaning it will only consider the first trait).
#'      }
#' 
#' \item modifications for the target \code{"bd.params"}
#'      \itemize{
#'          \item \code{update.bd.params}: this function updates the birth death parameters within the birth death process. The value of the parameter to change is specified with the argument \code{x} and the argument to change is specified with the argument \code{parameter} (e.g. \code{parameter = "speciation"} will attribute the value \code{x} to \code{bd.params$speciation}).
#'      }
#'
#' \item modifications for the target \code{"traits"} 
#'      \itemize{
#'          \item \code{update.traits}: this function updates a \code{"treats"} \code{"traits"} object. This function takes as arguments any arguments that can be updated in \code{\link{make.traits}}, namely \code{process}, \code{process.args} and \code{trait.names}.
#'      }
#' 
#' \item modifications for the target \code{"modifiers"}
#'      \itemize{
#'          \item \code{update.modifiers}: this function updates a \code{"treats"} \code{"modifiers"} object. This function takes as arguments any arguments that can be updated in \code{\link{make.modifiers}}, namely \code{branch.length}, \code{selection}, \code{speciation}, \code{condition} and \code{modify}.
#'      }
#' 
#' \item modifications for the target \code{"founding"} 
#'      \itemize{
#'          \item \code{founding.event}: this function runs an independent birth-death process when the condition is met. This function takes the arguments \code{"bd.params"}, \code{"traits"}, \code{"modifiers"} and \code{"events"} as they would normally be specified for the \code{\link{treats}} function. The \code{stop.rule} and other arguments are handled internally: namely the \code{stop.rule} argument is updated to match the time and number of taxa when the founding event is triggered. \emph{Note that this can lead to the simulation stopping just before reaching the \code{max.taxa} or \code{max.living} stop rule}.
#'      }
#' }
#' 
#' 
#' More details about the \code{events} functions is explained in the \code{treats} manual: \url{http://tguillerme.github.io/treats}.
#' 
#' @examples

#' ## Generating a mass extinction
#' ## 80% mass extinction at time 4
#' mass_extinction <- make.events(
#'                       target       = "taxa",
#'                       condition    = time.condition(4),
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
#'                   modification = update.traits(process = OU.process))
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
#' par(mfrow = c(1,2))
#' plot(no_change, ylim = c(-7, 7))
#' plot(process_change, ylim = c(-7, 7))
#' 
#' 
#' @seealso \code{\link{treats}} \code{\link{make.events}} \code{\link{events.conditions}}
#' 
#' @author Thomas Guillerme

## The list of conditions
modification <- function(x) {
    cat("List of inbuilt modification functions in treats:\n")
    cat("For the taxa target:\n")
    cat("   ?random.extinction\n")
    cat("   ?trait.extinction\n")
    cat("For the bd.params target:\n")
    cat("   ?update.bd.params\n")
    cat("For the traits target:\n")
    cat("   ?update.traits\n")
    cat("For the modifiers target:\n")
    cat("   ?update.modifiers\n")
    cat("For the founding target:\n")
    cat("   ?founding.event\n")
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
update.bd.params <- function(x, speciation = NULL, extinction = NULL, joint = NULL, absolute = NULL, speciation.args = NULL, extinction.args = NULL) {

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
update.traits <- function(x, process = NULL, process.args = NULL, trait.names = NULL) {

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
update.modifiers <- function(x, branch.length = NULL, selection = NULL, speciation = NULL, condition = NULL, modify = NULL) {

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

