
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







pars <- c(1, 0)

make.tree.bd <- function(pars, max.living=Inf, max.t=Inf) {
  extinct <- FALSE
  split   <- FALSE
  parent  <- 0

  speciation   <- pars[1]
  extinction   <- pars[2]
  edge_lengths <- 0
  time         <- 0
  n_living_taxa  <- 1
  lineages     <- 1

  while ( n_living_taxa <= max.living && n_living_taxa > 0) {
    ## When does an event happen?
    ## Get the probability of something happening
    event_probability <- sum(n_living_taxa * (speciation + extinction))
    ## Get the waiting time
    waiting_time <- rexp(1, event_probability)
    ## Update the global time
    time <- time + waiting_time

    ## Correct for going overtime?
    # if ( time > max.t ) {
    #   waiting_time <- waiting_time - (time - max.t)
    #   edge_lengths[lineages] <- edge_lengths[lineages] + waiting_time
    #   time <- max.t
    #   break
    # }

    ## Updating branch length
    edge_lengths[lineages] <- edge_lengths[lineages] + waiting_time

    ## Pick a lineage for the event to happen to:
    selected_lineage <- sample(n_living_taxa, 1)
    lineage <- lineages[selected_lineage]

    ## Randomly triggering an event
    if (runif(1) < (speciation/(speciation + extinction)) ) {
      
      ## Speciating:
      if(n_living_taxa == max.living) {
        ## Don't add this one
        break
      }

      ## Creating the new lineages
      new_lineage <- length(extinct) + 1:2
      split[lineage] <- TRUE
      extinct[new_lineage] <- split[new_lineage] <- FALSE
      parent[new_lineage] <- lineage
      edge_lengths[new_lineage] <- 0
      n_living_taxa <- n_living_taxa + 1

      ## lineages <- which(!split & !extinct)
      lineages <- c(lineages[-selected_lineage], new_lineage)
    
    } else {
      ## Go extinct
      extinct[lineage] <- TRUE
      
      ## lineages <- which(!split & !extinct)
      lineages <- lineages[-selected_lineage]
      n_living_taxa <- n_living_taxa - 1
    }
  }

  ## Summarise into a table
  table <- data.frame(idx = seq_along(extinct), parent = parent, len = edge_lengths, extinct = extinct, split = split)[-1, ]

  ## Remove the first row (initiation)

  Nnode  <- sum(!table$split) - 1
  n.tips <- sum(!table$split)

  table$idx2 <- NA
  table$idx2[!table$split] <- 1:n.tips
  table$idx2[ table$split] <- order(table$idx[table$split]) + n.tips + 1

  i <- match(table$parent, table$idx)
  table$parent2 <- table$idx2[i]
  table$parent2[is.na(table$parent2)] <- n.tips + 1

  tip.label <- ifelse(subset(table, !split)$extinct,
                      sprintf("ex%d", 1:n.tips),
                      sprintf("sp%d", 1:n.tips))
  node.label <- sprintf("nd%d", 1:Nnode)

  table$name <- NA
  table$name[!table$split] <- tip.label

  phy <- reorder(structure(list(edge=cbind(table$parent2, table$idx2),
                                Nnode=Nnode,
                                tip.label=tip.label,
                                node.label=node.label,
                                edge.length=table$len),
                           class="phylo"))
  phy
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
