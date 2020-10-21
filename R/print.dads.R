#' @title Prints a \code{dads} object.
#'
#' @description Summarises the content of a \code{dads} object.
#'
#' @param x A \code{dads} object.
#' @param all \code{logical}; whether to display the entire object (\code{TRUE}) or just summarise its contents (\code{FALSE} - default).
#' @param ... further arguments to be passed to \code{print} or to \code{print.dads}.
#' 
#' @examples
#'
#' @seealso
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
            ## randtest
            switch(class(x)[[2]],
                traits = {
                    cat("dads traits object:\n")
                    print.traits.info(x)
                },
                events = {
                    cat("dads events object:\n")
                },
                modifiers = {
                    cat("dads modifiers object:\n")
                }
                )
            return(invisible())
        }

        ## Normal class dads objects (tree + traits)
        cat("Simulated diversity data (x$tree):\n")
        cat(print(test$tree))
        cat("Simulated disparity data (x$data):\n")
        cat(paste0(ncol(test$traits), " trait", ifelse(ncol(test$traits) > 1, "s", "") ," for ", nrow(test$traits), " element", ifelse(nrow(test$traits) > 1, "s", "") ," generated using ", length(test$call$process), " process", ifelse(length(test$call$process) > 1, "es.", ".")))
    }
    return(invisible())
}