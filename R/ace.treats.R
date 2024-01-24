# #' @title ace.treats
# #'
# #' @description Ancestral characters estimations on treats
# #'
# #' @param treats a \code{treats} object with traits and a tree (you can use \code{\link{make.treats}} to format your data).
# #' @param traits a \code{traits} object to simulate the trait (see \code{\link{make.traits}}).
# #' @param root.est can be either the name of the algorithm to estimate the root value(s), a predetermined set of root values, or a personalised function to estimate them (see details).
# #' @param param.est can be either the name of the algorithm to estimate traits parameters, a predetermined set of trait parameters, or a personalised function to estimate them (see details).
# #' @param ... any additional arguments to pass to fun.
# #' @param replicates the number of replicates to diagnose the ancestral states estimations
# #'
# #' @return
# #' This function outputs a treats object with the tree (\code{$tree} a list of traits estimates (\code{$data})) as well as a diagnosis of the estimations (\code{$diagnosis}).
# #'
# #' @details
# #' This function 
# #'
# #' @example
# #'
# #' @seealso \code{\link{make.traits}} \code{\link{make.treats}} \code{\link{map.traits}} \code{\link{treats}}
# #' 
# #' @author Thomas Guillerme
# #' @export

# ace.treats <- function(treats, traits, root.est = "debug", param.est = "debug", ..., replicates = 1) {
#     ## Sanitizing
#     check.class(treats, "treats")
#     check.class(traits, c("treats", "traits"))

#     ## Root.est
#     ## Check the root estimation input
#     ## If method or function, run the root estimations
#     if(root.est == "debug") {
#         root.est <- mean(treats$data) # very crude, needs branch length!
#     }

#     ## Param.est
#     ## Check the parameters estimation input
#     ## If method or function, run the root estimations

#     ## Update the trait
#     ace_traits <- traits.update(process.args = list(start = root.est))(traits = traits)

#     ## Map traits
#     ## Map the traits on the tree n times (= replicates)
#     estimates <- map.traits(traits = ace_traits, tree = treats$tree, replicates = replicates)

#     ## Diagnosis
#     ## Run the diagnosis

#     ## Clean the estimate (replace non-estimated values)
#     update.estimation <- function(estimate, input.data) {
#         matching_tips <- match(rownames(estimate$data), rownames(input_data$data))
#         estimate$data[which(!is.na(matching_tips)), ] <- input_data$data[matching_tips[!is.na(matching_tips)],]
#         return(estimate)
#     }
#     estimates_out <- lapply(estimates, update.estimation, input.data = treats)

#     if(replicates == 1) {
#         return(estimates_out[[1]])
#     } else {
#         class(estimates_out) <- "treats"
#         return(estimates_out)
#     }
# }



# ## TODO: allowing plotting multiple estimates
# ## TODO: allow plotting trees with unknown node values