#' @title Prints a \code{treats} object.
#'
#' @description Summarises the content of a \code{treats} object.
#'
#' @param x   A \code{treats} object.
#' @param all \code{logical}; whether to display the entire object (\code{TRUE}) or just summarise its contents (\code{FALSE} - default).
#' @param ... further arguments to be passed to \code{print} or to \code{print.treats}.
#' 
#' @examples
#' ## A treats birth-death parameters object
#' make.bd.params()
#' ## A treats traits object
#' make.traits()
#' ## A treats modifiers object
#' make.modifiers()
#' ## A treats object
#' treats(stop.rule = list(max.taxa = 10), traits = make.traits())
#'
#' @seealso \code{\link{treats}}
#' 
#' @author Thomas Guillerme
#' @export

print.treats <- function(x, all = FALSE, ...) {
    
    ## Get the call name
    match_call <- match.call()
    x_name <- match_call$x

    if(all) {
        ## Print everything
        tmp <- x
        class(tmp) <- "list"
        print(tmp)
    } else {
        ## Dual class treats objects
        if(length(class(x)) > 1) {
            switch(class(x)[[2]],
                traits = {
                    cat(" ---- treats traits object ---- \n")
                    print.traits.info(x$main)
                    if(!is.null(x$background)) {
                        cat("And a background trait (see x$background for info).")
                    }
                },
                events = {
                    cat(" ---- treats events object ---- \n")
                    lapply(x, print.events.info)
                },
                modifiers = {
                    cat(" ---- treats modifiers object ---- \n")
                    print.modifiers.info(x)
                },
                bd.params = {
                    cat(" ---- treats birth-death parameters object ---- \n")
                    print.bd.params.info(x)
                }
                )
            return(invisible())
        }

        ## Normal class treats objects (tree + traits)
        cat(" ---- treats object ---- \n")
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
        cat("Simulated phylogenetic tree (x$tree):\n")
        cat(print(x$tree))
        cat("\n")

        ## Print the traits
        if(!is.null(x$data)) {
            cat("Simulated trait data (x$data)")
        }
        if(!is.null(x$traits)) {
            cat(":\n")
            print.traits.info(x$traits$main)
            if(!is.null(x$traits$background)) {
                cat("And a background trait (see x$background for info).")
            }
        }
    }
    return(invisible())
}