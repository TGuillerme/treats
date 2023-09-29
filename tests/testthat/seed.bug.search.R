## This script helps formalising the seed designation for bug searches/reproducibility in birth.death_fun.R

# 1 - Load the commented out preamble in birth.death.tree.traits (birth.death_fun.R) but commenting out the function declaration and replacing it with the bd.debug function declaration below (and the parameters you want to test).

# 2 - Then loop through some seeds below to find the bug. The script ignores "false" bugs (trees not generated with set params).

## Loop through the seeds
list_tests <- list()
for(i in 1:1000) {
    print(i)
    list_tests[[i]] <- try(bd.debug(i))
}
## Find the errors
fails <- which(unlist(lapply(list_tests, class)) == "try-error")
## Remove the normal errors
detect.fails <- function(x) return(!(x[[1]] == "Error in bd.debug(i) : No tree generated with these parameters.\n"))
real_fails <- unlist(lapply(list_tests[fails], detect.fails)) 
## The real fails:
failing_seeds <- fails[real_fails]
list_tests[failing_seeds]

