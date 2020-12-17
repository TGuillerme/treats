## Checking and filling arguments for modifiers
check.args <- function(fun, fun_name, modify = FALSE) {

    ## The required arguments (internal)
    required_args <- c("bd.params", "lineage", "trait.values", "modify.fun")

    if(modify) {
        required_args <- c("x", required_args)
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

        if(modify) {
            ## Also considering the x argument
            if(missing_args[1]) {
                formals(fun) <- c(formals(fun), alist("x" = NULL))
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
        if(missing_args[4]) {
            formals(fun) <- c(formals(fun),  alist("modify.fun" = NULL))
        }                
    }

    return(fun)
} 

## Checking the modifiers structure
check.modifiers <- function(modifiers) {

    ## Check the content at the first level
    required_names <- c("waiting", "selecting", "speciating", "call")
    if(any(missing <- is.na(match(names(modifiers), required_names)))) {
        stop(paste0("modifiers must have the following elements: ", paste(required_names, collapse = ", "), "."), call. = FALSE)
    }

    ## Check the content for each required names
    required_content <- c("fun", "internal")
    for(one_check in required_names[-length(required_names)]) {
        if(any(missing <- is.na(match(names(modifiers[[one_check]]), required_content)))) {
            stop(paste0("The ", one_check, " modifier must contain the following elements: ", paste(required_content, collapse = ", "), "."), call. = FALSE)
        }
    }

    ## Dummy (basic) arguments
    bd.params    <- list(speciation = 1, extinction = 0)
    trait.values <- rbind(NULL, "1" = c(1))
    lineage <- list("parents" = 1L,     ## The list of parent lineages
                    "livings" = 1L,     ## The list of lineages still not extinct
                    "drawn"   = 1L,     ## The lineage ID drawn (selected)
                    "current" = 1L,     ## The current focal lineage
                    "n"       = 1L,     ## The number of non extinct lineages
                    "split"   = FALSE)


    ## Testing the waiting function
    test_waiting <- try(modifiers$waiting$fun(bd.params    = bd.params,
                                              lineage      = lineage,
                                              trait.values = trait.values,
                                              modify.fun   = modifiers$waiting$internal)
    ,
                        silent = TRUE)

    ## Debrief
    if(class(test_waiting) == "try-error") {
        stop(paste0("The branch length element from the modifiers failed with the following error message", ifelse(length(test_waiting) > 1, "s:\n", ":\n"), paste(test_waiting, collapse = "\n")), call. = FALSE)
    } else {
        if(class(test_waiting) != "numeric") {
            stop(paste0("The branch length element from the modifiers did not produce a numeric value (it produced a ", paste(class(test_waiting), collapse = ","), " instead)."))
        }
    }

    ## Testing the selecting function
    test_selecting <- try(modifiers$selecting$fun(bd.params    = bd.params,
                                                  lineage      = lineage,
                                                  trait.values = trait.values,
                                                  modify.fun   = modifiers$selecting$internal),
                        silent = TRUE)

    ## Debrief
    if(class(test_selecting) == "try-error") {
        stop(paste0("The selection element from the modifiers failed with the following error message", ifelse(length(test_selecting) > 1, "s:\n", ":\n"), paste(test_selecting, collapse = "\n")), call. = FALSE)
    } else {
        if(class(test_selecting) != "integer") {
            stop(paste0("The selection element from the modifiers did not produce a integer value (it produced a ", paste(class(test_selecting), collapse = ","), " instead)."))
        }
    }


    ## Testing the speciating function
    test_speciating <- try(modifiers$speciating$fun(bd.params    = bd.params,
                                                    lineage      = lineage,
                                                    trait.values = trait.values,
                                                    modify.fun   = modifiers$speciating$internal),
                           silent = TRUE)

    ## Debrief
    if(class(test_speciating) == "try-error") {
        stop(paste0("The speciation element from the modifiers failed with the following error message", ifelse(length(test_speciating) > 1, "s:\n", ":\n"), paste(test_speciating, collapse = "\n")), call. = FALSE)
    } else {
        if(class(test_speciating) != "logical") {
            stop(paste0("The speciation element from the modifiers did not produce a logical value (it produced a ", paste(class(test_speciating), collapse = ","), " instead)."))
        }
    }
    return(NULL)
}