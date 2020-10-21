## Function for printing the traits info
print.traits.info <- function(x) {
    ## Get the trait object info
    n_processes   <- length(x)
    trait_process <- lapply(x, function(X) X$trait_id)
    n_traits      <- length(unlist(trait_process))
    start_values  <- unlist(lapply(x, function(X) X$start))
    n_start       <- length(unique(start_values))
    extra_options <- lapply(x, function(X) names(X)[!names(X) %in% c("process", "start", "trait_id")])
    with_extra    <- unlist(lapply(extra_options, function(x) length(x) != 0))

    ## Number of traits
    cat(paste0(n_traits, " trait", ifelse(n_traits > 1, "s ", " ")))
    ## Number of processes
    cat(paste0("for ", n_processes, " process", ifelse(n_processes > 1, "es ", " ")))
    if(n_traits != n_processes) {
        cat(paste0("(", paste0(paste(names(x), unlist(lapply(trait_process, length)), sep = ":"), collapse = ", ") , ") "))
    } else {
        cat(paste0("(", paste0(names(x), collapse = ", ") , ") "))
    }
    ## Starting values
    cat(paste0("with "))
    if(n_start == 1) {
        cat(paste0("one starting value (", unique(start_values), ")"))
    } else {
        cat(paste0("different starting values (", paste0(start_values, collapse = ",") ,")"))
    }
    cat(paste0(".\n"))
    ## Extra options
    if(any(with_extra)) {
        for(one_process in seq_along(with_extra)) {
            if(with_extra[one_process]) {
                cat(paste0("process ", names(x)[one_process], " uses the following extra argument", ifelse(length(extra_options[[one_process]]) > 1, "s: ", ": "), paste0(extra_options[[one_process]], collapse = ","), ";\n"))
            }
        }
        paste0("\n")
    }
    return(invisible())
}
