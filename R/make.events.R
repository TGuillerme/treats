#' @title make.events
#'
#' @description Making events objects for dads
#'
#' @param what  The number of traits (default is 1)
#' @param when  The starting values for each traits (default is 0)
#' @param how   The trait process(es) (default is process.BM)
#' @param add   An optional named list of optional arguments for the trait process
#' @param test  Logical whether to test if the events object will work (default is \code{TRUE})
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

make.events <- function() {

    output <- 42*10
    class(output) <- c("dads", "events")
    return(output)
}

# ## Change in the traits:
# trait.event <- list(
#     when = ...,
#     what = ...,
#     event = ...)
# #TG: for this one, just change traits$process or traits$cor when the event occurs
