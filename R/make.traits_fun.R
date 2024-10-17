## Check if traits work
check.traits <- function(traits, events = FALSE) {

    ## Get the main trait
    if(!is.null(traits$main)) {
        traits <- traits$main
    }

    if(events) {
        events_msg <- "The treats traits object returned by the modification function does not work: "
    } else {
        events_msg <- ""
    }

    ## Make dummy edge.length
    edge.length <- 42
    ## Make dummy parent trait
    parent.trait <- 1

    ## Loop through each trait for detailed explanation on why it failed
    try_success <- list()
    for(one_trait in 1:length(traits)) {
        try_success[[one_trait]] <- try(
                                        sim.element.trait(one.trait    = traits[[one_trait]],
                                                          parent.trait = rep(parent.trait, length(traits[[one_trait]]$start)),
                                                          edge.length  = edge.length)
                                        , silent = TRUE)
    }

    ## catch the errors
    errors <- unlist(lapply(try_success, function(x) return(is(x, "try-error"))))

    ## Error
    if(any(errors)) {
        error_msg <- "Impossible to generate traits with the current traits object."
        trait_failed <- paste0(" The trait", ifelse(sum(errors) > 1, "s ", " "), paste0(which(errors), collapse = ", "), " returned the following error messages:\n")
        failure_messages <- ""
        for(one_trait in 1:length(traits)) {
            if(errors[one_trait]) {
                failure_messages <- c(failure_messages, "trait ", one_trait, ": ", try_success[[one_trait]][[1]], "\n")
            }
        }
        stop(paste0(events_msg, error_msg, trait_failed, paste0(failure_messages, collapse = "")))
    } else {
        return(NULL)
    }
}