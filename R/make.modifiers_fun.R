## Checking the modifiers structure
check.modifiers <- function(modifiers) {

    ## Check the content at the first level
    required_names <- c("waiting", "speciating", "call")
    if(any(missing <- is.na(match(names(modifiers), required_names)))) {
        ## TODO: improve message here
        stop(paste0("modifiers must have the following elements: ", paste(required_names, collapse = ", "), "."), call. = FALSE)
    }

    ## Check the content for each required names
    required_content <- c("fun", "internal")
    for(one_check in required_names[-length(required_names)]) {
        if(any(missing <- is.na(match(names(modifiers[[one_check]]), required_content)))) {
            ## TODO: improve message here
            stop(paste0("The ", one_check, " modifier must contain the following elements: ", paste(required_content, collapse = ", "), "."), call. = FALSE)
        }
    }

    ## Dummy (basic) arguments
    bd.params      <- list(speciation = 1, extinction = 0)
    n.taxa         <- 1
    parent.lineage <- 1
    trait_values   <- rbind(NULL, "1" = c(1))

    ## Testing the waiting function
    test_waiting <- try(modifiers$waiting$fun(bd.params,
                                              n.taxa         = n.taxa,
                                              parent.lineage = parent.lineage,
                                              trait.values   = trait_values,
                                              modify.fun     = modifiers$waiting$internal),
                        silent = TRUE)

    ## Debrief
    if(class(test_waiting) == "try-error") {
        stop(paste0("The waiting element from the modifiers failed with the following error message", ifelse(length(test_waiting) > 1, "s:\n", ":\n"), paste(test_waiting, collapse = "\n")), call. = FALSE)
    } else {
        if(class(test_waiting) != "numeric") {
            stop(paste0("The waiting element from the modifiers did not produce a numeric value (it produced a ", paste(class(test_waiting), collapse = ","), " instead)."))
        }
    }


    ## Testing the speciating function
    test_speciating <- try(modifiers$speciating$fun(bd.params,
                                                n.taxa         = n.taxa,
                                                parent.lineage = parent.lineage,
                                                trait.values   = trait_values,
                                                modify.fun     = modifiers$speciating$internal),
                           silent = TRUE)

    ## Debrief
    if(class(test_speciating) == "try-error") {
        stop(paste0("The speciating element from the modifiers failed with the following error message", ifelse(length(test_speciating) > 1, "s:\n", ":\n"), paste(test_speciating, collapse = "\n")), call. = FALSE)
    } else {
        if(class(test_speciating) != "logical") {
            stop(paste0("The waiting element from the modifiers did not produce a logical value (it produced a ", paste(class(test_speciating), collapse = ","), " instead)."))
        }
    }
    return(NULL)
}