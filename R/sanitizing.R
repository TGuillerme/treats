## SANITIZING FUNCTIONS
## Checking the class of an object and returning an error message if != class
check.class <- function(object, class, msg, errorif = FALSE) {
    ## Get call
    match_call <- match.call()

    ## class_object variable initialisation
    class_object <- class(object)[1]
    ## class_length variable initialisation
    length_class <- length(class)

    ## Set msg if missing
    if(missing(msg)) {
        if(length_class != 1) {
            msg <- paste(" must be of class ", paste(class, collapse = " or "), ".", sep = "")
        } else {
            msg <- paste(" must be of class ", class, ".", sep = "")
        }
    }

    ## check if object is class.
    if(length_class != 1) {
    ## check if object is class in a cascade (class[1] else class[2] else class[3], etc..)
    ## returns error only if object is not of any class

        error <- NULL
        for(counter in 1:length_class) {
            if(errorif != TRUE) {
                if(class_object != class[counter]) {
                    error <- c(error, TRUE)
                } else {
                    error <- c(error, FALSE)
                }
            } else {
                if(class_object == class[counter]) {
                    error <- c(error, TRUE)
                } else {
                    error <- c(error, FALSE)
                }
            }
        }
        ## If function did not return, class is not matching
        if(!any(!error)) {
            stop(match_call$object, msg, call. = FALSE)
        } else {
            return(class_object)
        }

    } else {
        if(errorif != TRUE) {
            if(class_object != class) {
                stop(match_call$object, msg , call. = FALSE)
            }
        } else {
            if(class_object == class) {
                stop(match_call$object, msg , call. = FALSE)
            }        
        }
    } 
}

## Checking the class of an object and returning an error message if != class
check.length <- function(object, length, msg, errorif = FALSE) {

    match_call <- match.call()

    if(errorif != TRUE) {
        if(length(object) != length) {
            stop(match_call$object, msg , call. = FALSE)
        }
    } else {
        if(length(object) == length) {
            stop(match_call$object, msg , call. = FALSE)
        }        
    }
}

## Checking if a method matches the authorized method
check.method <- function(argument, all_arguments, msg, condition = all) {
    if(condition(is.na(match(argument, all_arguments)))) {
        stop(paste(msg, " must be one of the following: ", paste(all_arguments, collapse = ", "), ".", sep = ""), call. = FALSE)
    }
}

## Stop with call message wrapper function
stop.call <- function(call, msg, msg.pre = "") {
    stop(paste0(msg.pre, as.expression(call), msg), call. = FALSE)
}

## Check through a list
check.list <- function(list, check.fun, condition, ...) {
    ## Run the checks
    check_results <- lapply(list, check.fun, ...)
    ## Apply the condition
    if(!missing(condition)) {
        return(unlist(lapply(check_results, condition)))
    } else {
        return(unlist(check_results))
    }
}

## Checking treats specific class
check.args <- function(fun, fun_name, required_args) {

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

        ## List of missings
        to_add_args <- required_args[missing_args]

        ## Add the missing arguments recursively
        while(length(to_add_args) != 0) {
            ## Create one new argument
            new_arg <- alist(NULL)
            names(new_arg) <- to_add_args[1]
            ## Add the new argument
            formals(fun) <- c(formals(fun), new_arg)
            ## Recursively reduce the list
            to_add_args <- to_add_args[-1]
        }
    }

    return(fun)
}
