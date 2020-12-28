## Checking and filling arguments for modifiers
check.args.events <- function(fun, fun_name, condition = FALSE) {

    ## The required arguments (internal)
    required_args <- c("bd.params", "lineage", "trait.values")

    if(condition) {
        required_args <- c(required_args, "time")
    }

    ## Must be a function
    check.class(fun, "function", msg = paste0("ction for ", fun_name, " is not a function."))
    ## Check the arguments
    present_args <- names(formals(fun))
    ## Check for incorrect arguments
    if(any(incorrect_args <- is.na(match(present_args, required_args)))) {
        stop(paste0("The ", fun_name, " function cannot recognise the ", paste(present_args[incorrect_args], collapse = ", "), " argument", ifelse(sum(incorrect_args) > 1, "s.", ".")), call. = FALSE)
    }
    ## Add the missing arguments
    used_args <- methods::formalArgs(fun)
    if(any(missing_args <- !(required_args %in% used_args))) {

        ## Add the missing args one by one (there's probably a better way to do this!)

        if(condition) {
            ## Also considering the time argument
            if(missing_args[1]) {
                formals(fun) <- c(formals(fun), alist("time" = NULL))
            }
            missing_args <- missing_args[-1]
        }

        if(missing_args[1]) {
            formals(fun) <- c(formals(fun),  alist("bd.params" = NULL))    
        }
        if(missing_args[2]) {
            formals(fun) <- c(formals(fun),  alist("lineage" = NULL))    
        }
        if(missing_args[3]) {
            formals(fun) <- c(formals(fun),  alist("trait.values" = NULL))
        }              
    }

    return(fun)
} 


## Checking the events structure
check.events <- function(events) {

    ## Check the content at the first level
    required_names <- c("target", "trigger", "condition", "modification")
    if(any(missing <- is.na(match(names(events), required_names)))) {
        stop(paste0("events must have the following elements: ", paste(required_names, collapse = ", "), "."), call. = FALSE)
    }

    ## Dummy values
    bd.params    <- list(speciation = 1, extinction = 0)
    trait.values <- rbind(NULL, "1" = c(1))
    lineage <- list("parents" = 1L,     ## The list of parent lineages
                    "livings" = 1L,     ## The list of lineages still not extinct
                    "drawn"   = 1L,     ## The lineage ID drawn (selected)
                    "current" = 1L,     ## The current focal lineage
                    "n"       = 1L,     ## The number of non extinct lineages
                    "split"   = FALSE)
    first_waiting_time <- time <- 0
    traits <- make.traits()
    modifiers <- make.modifiers(branch.length = branch.length, speciation = speciation, selection = selection)

    ## Check if the trigger is negative
    if(events$trigger > 0L) {
        stop("The current events object cannot be triggered. Make sure the replications option is set to 0 or any positive integer.")
    }

    ## Check if the condition works
    test_condition <- try(events$condition(bd.params = bd.params,
                                         lineage = lineage,
                                         trait.values = trait_values,
                                         time = time - first_waiting_time)
    ,
                        silent = TRUE)

    ## Debrief
    if(class(test_condition) == "try-error") {
        stop(paste0("The condition function from the events object failed with the following error message", ifelse(length(test_condition) > 1, "s:\n", ":\n"), paste(test_condition, collapse = "\n")), call. = FALSE)
    } else {
        if(class(test_condition) != "logical") {
            stop(paste0("The condition function from the events object did not produce a logical value (it produced a ", paste(class(test_condition), collapse = ","), " instead)."))
        }
    }

    ## Check the target
    allowed_targets <- c("taxa", "bd.params", "traits", "modifiers")
    check.method(events$target, allowed_targets, "target argument ")

    ## Check the modification on the different targets
    switch(events$target,
        taxa      = {
            test_modification <- try(events$modification(
                            bd.params    = bd.params,
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
                            bd.params    = bd.params,
                            lineage      = lineage,
                            trait.values = trait_values)
            , silent = TRUE)},
        modifiers = {
            test_modification <- try(events$modification(
                            modifiers    = modifiers,
                            bd.params    = bd.params,
                            lineage      = lineage,
                            trait.values = trait_values)
            , silent = TRUE)})


    ## Debrief
    if(class(test_modification) == "try-error") {
        stop(paste0("The modification function from the events object failed with the following error message", ifelse(length(test_modification) > 1, "s:\n", ":\n"), paste(test_modification, collapse = "\n")), call. = FALSE)
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
                if(!is(test_modification, "list")) {
                    stop(paste0("The modification function targeting \"bd.params\" must output a list of integers. Currently the modification function output is a ", paste(class(test_modification), collapse = ", "), " object."), call. = FALSE)
                } 
                if(!all(bd.params_names %in% names(test_modification))) {
                    stop(paste0("The modification function targeting \"bd.params\" must output a list of integers with the following elements: ", paste(bd.params_names, collapse = ", "), ". Currently it contains the following: ", paste(names(test_modification), collapse = ", "), "."))
                }
            },

            traits    = {
                if(!is(test_modification, "dads") && !is(test_modification, "traits")) {
                    stop(paste0("The modification function targeting \"traits\" must output a dads traits object. Currently the modification function output is a ", paste(class(test_modification), collapse = ", "), "."))
                } else {
                    check.traits(test_modification, events = TRUE)
                }
            },

            modifiers = {
                if(!is(test_modification, "dads") && !is(test_modification, "modifiers")) {
                    stop(paste0("The modification function targeting \"modifiers\" must output a dads modifiers object. Currently the modification function output is a ", paste(class(test_modification), collapse = ", "), "."))
                } else {
                    check.modifiers(test_modification, events = TRUE)
                }
            })
      }

    return(NULL)
}