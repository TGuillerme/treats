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
        # OLD_LINK_TRAITS_IN
        # if(all(names(traits[[one_trait]]) %in% c("conditional", "conditioned"))) {
        #     ## Separately trying either
        #     test1 <- try(sim.element.trait(traits[[one_trait]]$conditional[[1]], parent.trait, edge.length), silent = TRUE)
        #     test2 <- try(lapply(traits[[one_trait]]$conditioned, function(x, parent.trait,edge.length) return(sim.element.trait(x[[1]], parent.trait, edge.length)), parent.trait, edge.length), silent = TRUE)
        #     if(is(test1, "try-error")) {
        #         try_success[[one_trait]] <- test1
        #     } else {
        #         if(is(test2, "try-error")) {
        #             try_success[[one_trait]] <- test2
        #         } else {
        #             try_success[[one_trait]] <- test1
        #         }
        #     }
        # } else {
        # OLD_LINK_TRAITS_OUT
        try_success[[one_trait]] <- try(sim.element.trait(traits[[one_trait]], parent.trait, edge.length), silent = TRUE)
        # OLD_LINK_TRAITS_IN
        #}
        # OLD_LINK_TRAITS_OUT
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