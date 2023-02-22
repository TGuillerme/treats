#' @title Prints a \code{dads} object.
#'
#' @description Summarises the content of a \code{dads} object.
#'
#' @param x   A \code{dads} object.
#' @param all \code{logical}; whether to display the entire object (\code{TRUE}) or just summarise its contents (\code{FALSE} - default).
#' @param ... further arguments to be passed to \code{print} or to \code{print.dads}.
#' 
#' @examples
#'
#' @seealso \code{\link{dads}}
#' 
#' @author Thomas Guillerme
#' @export

print.dads <- function(x, all = FALSE, ...) {
    
    ## Get the call name
    match_call <- match.call()
    x_name <- match_call$x

    if(all) {
        ## Print everything
        tmp <- x
        class(tmp) <- "list"
        print(tmp)
    } else {
        ## Dual class dads objects
        if(length(class(x)) > 1) {
            switch(class(x)[[2]],
                traits = {
                    cat(" ---- dads traits object ---- \n")
                    print.traits.info(x$main)
                    if(!is.null(x$background)) {
                        cat("And a background trait (see x$background for info).")
                    }
                },
                events = {
                    cat(" ---- dads events object ---- \n")
                    lapply(x, print.events.info)
                },
                modifiers = {
                    cat(" ---- dads modifiers object ---- \n")
                    print.modifiers.info(x)
                },
                bd.params = {
                    cat(" ---- dads birth-death parameters object ---- \n")
                    print.bd.params.info(x)
                }
                )
            return(invisible())
        }

        ## Normal class dads objects (tree + traits)
        cat(" ---- dads object ---- \n")
        ## Print the modifiers and/or events
        if(!is.null(x$modifiers) || !is.null(x$events)) {
            cat(paste0(
                "Birth death process with",
                ifelse(!is.null(x$modifiers), " modifiers", ""), 
                ifelse(!is.null(x$events), paste0(ifelse(!is.null(x$modifiers), " and", ""), " events"), ""), 
                ":\n"))
            if(!is.null(x$bd.params)) {
                print.bd.params.info(x$bd.params)
            } 
            if(!is.null(x$modifiers)) {
                print.modifiers.info(x$modifiers)
            } 
            if(!is.null(x$events)) {
                lapply(x$events, print.events.info)
            }
            cat("\n")
        }

        ## Print the tree
        cat("Simulated diversity data (x$tree):\n")
        cat(print(x$tree))
        cat("\n")

        ## Print the traits
        if(!is.null(x$traits)) {
            cat("Simulated disparity data (x$data):\n")
            print.traits.info(x$traits$main)
            if(!is.null(x$traits$background)) {
                cat("And a background trait (see x$background for info).")
            }
        }
    }
    return(invisible())
}