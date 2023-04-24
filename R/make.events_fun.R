## Checking the events structure
check.events <- function(events) {

    ## Check the content at the first level
    required_names <- c("target", "trigger", "condition", "modification", "args", "call")
    if(any(missing <- is.na(match(names(events), required_names)))) {
        stop(paste0("events must have the following elements: ", paste(required_names, collapse = ", "), "."), call. = FALSE)
    }

    ## Dummy values
    bd.params    <- make.bd.params(speciation = 1, extinction = 0)
    trait_values <- rbind(NULL, "1" = c(1))
    lineage <- list("parents" = 1L,     ## The list of parent lineages
                    "livings" = 1L,     ## The list of lineages still not extinct
                    "drawn"   = 1L,     ## The lineage ID drawn (selected)
                    "current" = 1L,     ## The current focal lineage
                    "n"       = 1L,     ## The number of non extinct lineages
                    "split"   = FALSE)
    first_waiting_time <- time <- 0
    traits <- make.traits(process = c(BM.process, BM.process, BM.process))
    modifiers <- make.modifiers(branch.length = branch.length, speciation = speciation, selection = selection)
    stop.rule <- list("max.time" = Inf, max.taxa = 10, max.living = 10)
    time <- 0

    ## Check if the trigger is negative
    if(events$trigger > 0L) {
        stop("The current events object cannot be triggered. Make sure the replications option is set to 0 or any positive integer.")
    }

    ## Check if the condition works
    test_condition <- try(events$condition(bd.params = sample.from(bd.params),
                                         lineage = lineage,
                                         trait.values = trait_values,
                                         time = time - first_waiting_time)
    ,
                        silent = TRUE)

    ## Debrief
    if(is(test_condition, "try-error")) {
        stop(paste0("The condition function from the events object failed with the following error message", ifelse(length(test_condition) > 1, "s:\n", ":\n"), paste(test_condition, collapse = "\n")), call. = FALSE)
    } else {
        if(!is(test_condition, "logical")) {
            stop(paste0("The condition function from the events object did not produce a logical value (it produced a ", paste(class(test_condition), collapse = ","), " instead)."))
        }
    }

    ## Check the target
    allowed_targets <- c("taxa", "bd.params", "traits", "modifiers", "founding")
    check.method(events$target, allowed_targets, "target argument ")
    
    ## Check the modification on the different targets
    switch(events$target,
        taxa      = {
            test_modification <- try(events$modification(
                            bd.params    = sample.from(bd.params),
                            lineage      = lineage,
                            trait.values = trait_values)
            , silent = TRUE)},
        bd.params = {
            test_modification <- try(events$modification(
                            bd.params    = bd.params,
                            lineage      = lineage,
                            trait.values = trait_values)
            , silent = TRUE)},
        traits    = {
            test_modification <- try(events$modification(
                            traits       = traits,
                            bd.params    = sample.from(bd.params),
                            lineage      = lineage,
                            trait.values = trait_values)
            , silent = TRUE)},
        modifiers = {
            test_modification <- try(events$modification(
                            modifiers    = modifiers,
                            bd.params    = sample.from(bd.params),
                            lineage      = lineage,
                            trait.values = trait_values)
            , silent = TRUE)},
        founding = {
            test_modification <- try(events$modification(
                            stop.rule    = stop.rule,
                            time         = time,
                            lineage      = lineage)
            , silent = TRUE)}
        )

    ## Debrief
    if(is(test_modification, "try-error") && events$target != "traits") {

        ## List of exceptions
        error_exceptions <- c("incompatible arguments", "No process", "parent_traits")
        exception <- any(unlist(sapply(error_exceptions, grep, test_modification[[1]])))
        if(!exception) {
            stop(paste0("The modification function from the events object failed with the following error message", ifelse(length(test_modification) > 1, "s:\n", ":\n"), paste(test_modification, collapse = "\n")), call. = FALSE)
        } else {
            return(NULL)
        }
    } else {

        switch(events$target,
            taxa    = {
                lineage_names <- names(lineage)
                if(!is(test_modification, "list")) {
                    stop(paste0("The modification function targeting \"taxa\" must output a list of integers. Currently the modification function output is a ", paste(class(test_modification), collapse = ", "), " object."), call. = FALSE)
                } 
                if(!all(lineage_names %in% names(test_modification))) {
                    stop(paste0("The modification function targeting \"taxa\" must output a list of integers with the following elements: ", paste(lineage_names, collapse = ", "), ". Currently it contains the following: ", paste(names(test_modification), collapse = ", "), "."))
                }
            },

            bd.params = {
                bd.params_names <- names(bd.params)
                if(!is(test_modification, "treats") && !is(test_modification, "bd.params")) {
                    stop(paste0("The modification function targeting \"bd.params\" must output a list of integers. Currently the modification function output is a ", paste(class(test_modification), collapse = ", "), " object."), call. = FALSE)
                } 
                if(!all(bd.params_names %in% names(test_modification))) {
                    stop(paste0("The modification function targeting \"bd.params\" must output a list of integers with the following elements: ", paste(bd.params_names, collapse = ", "), ". Currently it contains the following: ", paste(names(test_modification), collapse = ", "), "."))
                }
            },

            # traits    = {
            #     if(!is(test_modification, "treats") && !is(test_modification, "traits")) {
            #         ## Check if the error comes from additional args (i.e. wrong dummy traits object)
            #         stop(paste0("The modification function targeting \"traits\" must output a treats traits object. Currently the modification function output is a ", paste(class(test_modification), collapse = ", "), "."))
            #     } else {
            #         check.traits(test_modification, events = TRUE)
            #     }
            # },

            modifiers = {
                if(!is(test_modification, "treats") && !is(test_modification, "modifiers")) {
                    stop(paste0("The modification function targeting \"modifiers\" must output a treats modifiers object. Currently the modification function output is a ", paste(class(test_modification), collapse = ", "), "."))
                } else {
                    check.modifiers(test_modification, events = TRUE)
                }
            },

            founding = {
                if(!is(test_modification, "list")) {
                    stop(paste0("The modification function targeting \"founding\" must output a named list containing at least a \"phylo\" object. Currently the modification function output is a ", paste(class(test_modification), collapse = ", "), "."))
                } else {
                    if(!is(test_modification$tree, "phylo")) {
                        stop(paste0("The modification function targeting \"founding\" must output a named list containing at least a \"phylo\" object. Currently the modification function output is a list with the following elements: ", paste(names(test_modification), collapse = ", "), "."))
                    }
                }
            })
      }

    return(NULL)
}