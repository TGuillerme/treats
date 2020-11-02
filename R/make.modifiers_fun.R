## Checking the modifiers structure
check.modifiers <- function(modifiers) {

    ## Check the content at the first level
    required_names <- c("waiting", "speciating")
    if(any(missing <- is.na(match(names(modifiers), required_names)))) {
        ## TODO: improve message here
        stop("check.modifiers failure (TODO: improve thi message!).")
    }

    ## Check the content for each required names
    required_content <- c("fun", "internal")
    for(one_check in required_names) {
        if(any(missing <- is.na(match(names(modifiers[[one_check]]), required_content)))) {
            ## TODO: improve message here
            stop("check.modifiers failure (TODO: improve thi message!).")
        }
    }

    ## Dummy (basic) arguments
    bd.params      <- list(speciation = 1, exinction = 0)
    n.taxa         <- 1
    parent.lineage <- 1
    trait_values   <- rbind(NULL, "root" = c("element" = 1, 1))

    ## Testing the waiting function
    test_waiting <- try(
        waiting_time <- modifiers$waiting$fun(bd.params,
                                              n.taxa         = n.taxa,
                                              parent.lineage = parent.lineage,
                                              trait.values   = trait_values,
                                              modify.fun     = modifiers$waiting$internal),
                        silent = TRUE)

    ## Debrief
    if(class(test_waiting) == "try-error") {
        stop("check.modifiers failure (TODO: improve thi message!).")
    } else {
        if(class(waiting_time) != "numeric") {
            stop("check.modifiers failure (TODO: improve thi message!).")
        }
    }


    ## Testing the speciating function
    test_speciating <- try(
        do_speciate <- modifiers$speciating$fun(bd.params,
                                                n.taxa         = n.taxa,
                                                parent.lineage = parent.lineage,
                                                trait.values   = trait_values,
                                                modify.fun     = modifiers$speciating$internal),
                        silent = TRUE)

    ## Debrief
    if(class(test_speciating) == "try-error") {
        stop("check.modifiers failure (TODO: improve thi message!).")
    } else {
        if(class(do_speciate) != "logical") {
            stop("check.modifiers failure (TODO: improve thi message!).")
        }
    }
    return(NULL)
}