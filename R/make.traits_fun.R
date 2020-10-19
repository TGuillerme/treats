## Check if traits work
check.traits <- function(traits) {
    ## Make dummy edge.length
    edge.length <- 42
    ## Make dummy parent trait
    parent.trait <- 123

    try_sucess <- try(
        results <- lapply(traits, sim.element.trait, parent.trait, edge.length),
        silent = TRUE)

    ## Error
    if(class(try_sucess) == "try-error") {
        stop("Impossible to generate traits with the current traits object.", call. = FALSE)
    } else {
        return(NULL)
    }
}