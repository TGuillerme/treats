# #' @title plot
# #'
# #' @description plot
# #'
# #' @param x dads data
# #' @param trait
# #' @param plot.tree
# #' @param simulations
# #' @param ...
# #' 
# #' @examples
# #' plot.dads()
# #'
# #' @seealso
# #' 
# #' @author Thomas Guillerme
# #' @export

# plot.dads <- function(x, trait, plot.tree = TRUE, simulations = 100) {

#     ## Check class dads
#     check.class(dads, "dads")

#     if(length(dads_class <- class(dads), 2)) {
#         ## Get the second class of the dads object
#         second_class <- dads_class[2]

#         if(second_class == "traits") {
#             ## Plot the simulated trait
#             stop("TODO: plot.dads traits")
#             #TG: Something like that
#             plot.simulation(main = "Normal",
#                     replicate(simulations,
#                         sim.motion(process = x[[trait]]$process, 
#                                    parameters = list(fun = rnorm, options = list(mean = 0, sd = 1)),
#                                    steps = 100)))
#         }

#         return(invisible())
#     }

#     # if(is(dads, "matrix")) {#TG: And some dads class

#     # }

#     graphics::plot(42)
#     return(invisible())
# }