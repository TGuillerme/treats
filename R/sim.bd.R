
#' @param speciation the speciation parameter (speciation)
#' @param extinction the extinction parameter (extinction)
#' @param stop.rule a named list: change these to stop rules: max.living, max.living, max.time
#' @param max.time
sim.birth.death <- function(speciation, extinction, stop.rule) {

  ## Initialise the parameters
  start_point <- 0
  start_time  <- 0




}


## This function is for creating a new edge in the birth.death process
#' @param parent the id of the parent edge
#' @param active_edges the number of edges active in the whole tree
#' 
one.bd.event <- function(parent, active_edges, speciation, extinction) {



    event_probability <- sum(n_living_taxa * (speciation + extinction))
    ## Get the waiting time
    waiting_time <- rexp(1, event_probability)
    




    ## Update the global time
    time <- time + waiting_time


    return(c("parent" = parent,
             "descendant" = ,
             "edgelength" = ))





    goes_speciat <- speciation / (extinction + speciation)



}







speciation = 1
extinction = 0
stop.rule = list(max.live = 10)

birth.death.tree.traits <- function(speciation, extinction, stop.rule = list()) {
  
    ## Set up the stop rules
    stop.rule$max.taxa <- ifelse(is.null(stop.rule$max.taxa), Inf, stop.rule$max.taxa)
    stop.rule$max.live <- ifelse(is.null(stop.rule$max.live), Inf, stop.rule$max.live)
    stop.rule$max.time <- ifelse(is.null(stop.rule$max.time), Inf, stop.rule$max.time)

    ## Initialising the values
    parent <- edge_lengths <- time <- 0
    n_living_taxa <- lineages <- 1
    is_extinct <- is_split <- FALSE

    ## Building the tree (while the number of living taxa or the number of max taxa is not reached)
    set.seed(1) ; warning("DEBUG")
    while(n_living_taxa <= stop.rule$max.live || sum(!is_split) <= stop.rule$max.taxa) {
        
        ## Get the probability of something happening
        event_probability <- sum(n_living_taxa * (speciation + extinction))
        ## Get the waiting time
        waiting_time <- rexp(1, event_probability)
        ## Update the global time
        time <- time + waiting_time

        ## Arrived at the max time (cut the branch lengths)
        if(time > stop.rule$max.time) {
            waiting_time <- waiting_time - (time - stop.rule$max.time)
            edge_lengths[lineages] <- edge_lengths[lineages] + waiting_time
            time <- stop.rule$max.time
            break
        }

        ## Updating branch length
        edge_lengths[lineages] <- edge_lengths[lineages] + waiting_time

        ## Pick a lineage for the event to happen to:
        selected_lineage <- sample(n_living_taxa, 1)
        lineage <- lineages[selected_lineage]

        ## Simulating a trait (placeholder)
        trait <- 0
        modifier <- 0

        ## Randomly triggering an event
        if((modifier + runif(1)) < (speciation/(speciation + extinction))) {
            ## Speciating:
            if(n_living_taxa == stop.rule$max.live) {
                ## Don't add this one
                break
            }

            ## Creating the new lineages
            new_lineage <- length(is_split) + 1:2
            is_split[lineage] <- TRUE
            is_extinct[new_lineage] <- is_split[new_lineage] <- FALSE
            parent[new_lineage] <- lineage
            edge_lengths[new_lineage] <- 0
            n_living_taxa <- n_living_taxa + 1

            ## lineages <- which(!split & !extinct)
            lineages <- c(lineages[-selected_lineage], new_lineage)
        } else {

            ## Go extinct
            is_extinct[lineage] <- TRUE
            
            ## lineages <- which(!split & !extinct)
            lineages <- lineages[-selected_lineage]
            n_living_taxa <- n_living_taxa - 1
        }
    }
   
    ## Summarise into a table (minus the initiation)
    table <- data.frame(vertex       = seq_along(is_extinct),
                        parent       = parent,
                        edge_lengths = edge_lengths,
                        is_extinct   = is_extinct,
                        is_split     = is_split)[-1, ]

    ## Number of rows and tips
    Nnode  <- sum(!table$is_split) - 1
    n_tips <- sum(!table$is_split)

    ## Getting the edge table node/tips IDs
    table$vertex2 <- NA
    table$vertex2[!table$is_split] <- 1:n_tips
    table$vertex2[ table$is_split] <- order(table$vertex[table$is_split]) + n_tips + 1

    ## Getting the edge table nodes (left column)
    left_edges <- match(table$parent, table$vertex)
    table$parent2 <- table$vertex2[left_edges]
    table$parent2[is.na(table$parent2)] <- n_tips + 1

    ## Getting the tips and nodes labels
    tree <- list(edge        = cbind(table$parent2, table$vertex2),
                 Nnode       = Nnode,
                 tip.label   = paste0("t", 1:n_tips),
                 node.label  = paste0("n", 1:Nnode),
                 edge.length = table$edge_lengths,
                 root.time   = time)
    class(tree) <- "phylo"

    ## Output
    return(list(tree = tree, traits = matrix(NA)))
}



## This function is for updating one (or more) traits in the process
update.trait <- function(parent_edge, process) {
    ## Use the branch length to update the trait using the process function
}


## Make into a phylo object
convert.to.phylo <- function(bd_table) {
  ## Things in the table that are only in the right column get a tip name
  ## Things in the table that don't have a tip name get a node name
}
