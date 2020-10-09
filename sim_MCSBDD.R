sim_MCSBDD <- function (pars, ou = list(NULL,NULL), disp = NULL, root.value = 0, age.max = 50, age.ext = NULL, step.size = 0.01, bounds = c(-Inf, Inf), plot = TRUE, ylims = NULL, full.sim = FALSE) 
{
  lambda1 = pars[1] #speciation initiation rate
  tau0 = pars[2] #basal speciation completion rate
  beta = pars[3] #effect of trait differences on the speciation completion rate
  mu0 = pars[4] #competitive extinction parameter for good species
  mu1 = pars[5] #selective extinction parameter for good species
  mubg = pars[6] #background good species extinction rate
  mui0 = pars[7] #competitive extinction parameter for incipient species
  mui1 = pars[8] #selective extinction parameter for incipient species
  muibg = pars[9] #background incipient species extinction rate
  alpha1 = pars[10] #competition effect on extinction (competition strength)
  alpha2 = pars[11] #competition effect on trait evolution (competition strength)
  alpha3 = pars[12] #selection effect on extinction (selection strength)
  sig2 = pars[13] #variance (rate) of Brownian motion
  m = pars[14] #relative contribution of character displacement (competition) with respect to stochastic (brownian) evolution
  s = sqrt(sig2 * step.size) #variance of BM per step size
  m = m * step.size #relative contribution of competition with respect to BM per step size
  ndisp = 0 #number of dispersal events that have occurred
  if (length(bounds) != 2) {
    stop("Lower and upper bounds should be provided")
  }
  if (root.value < bounds[1] || root.value > bounds[2]) {
    print("Warning: root value outside boundaries; continuing simulation")
  }
  if (is.null(ou[[1]])){
    opt = NULL
    alpha4 = 0
    print("Warning: no OU parameters supplied; continuing simulation")
  }
  if (!is.null(ou[[1]])){
    if (is.null(ou[[2]])){
      stop("Alpha values for OU process should be provided")
    } else if (length(ou[[2]]) > length(ou[[1]])){
      stop("Number of optima and alpha values should match")
    } else if (length(ou[[2]]) == 1) {
      opt = ou[[1]]
      alpha4 = rep(ou[[2]], length(ou[[1]]))
    } else if (length(ou[[2]]) == length(ou[[1]])){
      opt = ou[[1]]
      alpha4 = ou[[2]]
    }
  }
  if(!is.null(disp)){
    if(any(is.na(disp))){
      stop("dispersal matrix contains NAs")
    }
    disp <- disp * step.size
    loc <- 1:dim(disp)[1]
  }
  process_dead = F #parameter signifying whether simulation is still running
  traits <- list(c(1, 1, 2, NA, root.value),
                 c(2, -1, 1, root.value, root.value)) #list of traits of extant lineages: for each lineage (element in list), a vector of four or more values: lineage number, status (incipient=-1/good=1/extinct=-2), parental lineage number (if incipient), current trait value of parental lineage, and all consecutive elements contain trait value at each time step (always root.value at the beginning)
  lineages <- rbind(c(1, 0, 0, -1, 1, 0, 0, 1),
                    c(1, 0, 0, -1, -1, 0, NA, 1))#matrix of extant lineages: for each lineage (element in list), a vector of eight values: parental node (lineage number), descendent node (0 if tip), starting time, ending time (-1 if still active), status (incipient=-1/good=1/extinct=-2), speciation completion or extinction time, speciation completion time (NA if still incipient), locality, dispersal time
  localities <- list(c(0),
                     c(0)) #list of localities of extant lineages: for lineage (element in list), a named vector of one or more variables: time spent at locality (name is locality)
  names(localities[[1]]) <- 1
  names(localities[[2]]) <- 1
  active_lineages = c(1, 2) #vector of numbers of active lineages
  n_lineages = length(active_lineages) #how many active lineages are there?
  n_good = 2L #how many good lineages are there?
  step_count = 1L #which step of the simulation we're on
  
  t = 0 + step.size #add step size to current time to simulate trait values and extinction/speciation for the next time step. The next bit of code will run until maximum age is reached:
  while ((age.max - t) > -step.size/2) { #t must go one step beyond age.max to ensure simulation of last step. /2 is to avoid numerical precision issues
    mat_diff <- matrix(0, n_lineages, n_lineages, dimnames = list(active_lineages, 
                                                                  active_lineages)) #matrix of differences in traits between all lineages
    for (i in 1:n_lineages) {
      sym_lineages <- active_lineages[which(lineages[active_lineages,8] == lineages[active_lineages[[i]],8])] #vector of lineages in same locality as focal lineage
      alo_lineages <- setdiff(active_lineages, sym_lineages) #vector of lineages in different locality from focal lineage
      mat_diff[i, ] <- sapply(traits[active_lineages], 
                              function(x)
                                (traits[[active_lineages[i]]][4 + step_count] - x[4 + step_count])) #fill in matrix by calculating differences in trait values between each pairwise combination of lineages
      if(length(alo_lineages) > 0) {
        mat_diff[i, as.character(alo_lineages)] <- 0
      } #for all lineages that do not co-occur, set the trait differences to 0 so that they do not affect any of the calculations
    } 
    diag(mat_diff) <- NA
    diff_sign <- sign(mat_diff) #extract signs from matrix: which trait in pair is lower and which is higher
    
    #only run the following section if any pair of lineages have identical trait values: this assigns a random sign to each member of a pair of identical lineages, to make sure they are still driven further away from each other
    if (any(diff_sign == 0, na.rm = T)) { 
      diff_sign[lower.tri(diff_sign)] <- NA #turn lower triangle into NA to remove duplicates for rest of loop
      eq_ind <- which(diff_sign == 0, arr.ind = T, useNames = F)#identify which lineages make up identical lineage pairs
      for (i in 1:nrow(eq_ind)) { #run loop for each identical lineage pair
        diff_sign[eq_ind[i, 1], eq_ind[i, 2]] <- sign(rnorm(1)) #assign random sign to lineage pair
      }
      diff_sign[lower.tri(diff_sign)] <- -diff_sign[upper.tri(diff_sign)] #reassign lower triangle
    } 
    
    diff_me = list() #list of differences - each element will store the differences in trait value between each lineage and all other co-occurring lineages
    
    #simulate new trait value for each lineage:
    for (i in 1:n_lineages) {
      signs_me <- diff_sign[i, -i] #vector of signs between focal lineage and all other lineages
      diff_me[[active_lineages[i]]] <- mat_diff[i, -i] #store trait differences
      dist_bounds <- traits[[active_lineages[i]]][4 + step_count] - bounds #calculate distance of current trait value from bounds
      bound_effect <- 3 * sum(sign(dist_bounds) * exp(-2 * (dist_bounds)^2)) #calculate the bound effect - this is added to the simulated trait value to make sure the trait is kept inside the bounds
      traits[[active_lineages[i]]][5 + step_count] <- #simulate new trait value at next time step
        traits[[active_lineages[i]]][4 + step_count] + #start with the current trait value
        alpha2 * m * sum(signs_me * exp(-alpha2 * (diff_me[[active_lineages[i]]])^2)) + #add competitive element where the trait value is pushed away from trait values of other lineages based on the differences - the closer two traits are, the stronger competition will drive them away
        rnorm(1, 0, s) + #add Brownian Motion element
        bound_effect
      
      if(!is.null(opt)){
        for (j in 1:length(opt)){
          traits[[active_lineages[i]]][5 + step_count] <- traits[[active_lineages[i]]][5 + step_count] + 
            alpha4[j] * (opt[j] - traits[[active_lineages[i]]][4 + step_count]) * step.size} #add selection element where the trait value is pulled towards the optima based on how far away it is - the farther from the optimum it is, the stronger the pull
      }
    }
    
    dead_lin = NULL #reset vector of non-active lineages
    born_lin = NULL #reset vector of new lineages
    #
    for (i in active_lineages) {
      localities[[i]][length(localities[[i]])] <- t - lineages[i, 3] - sum(localities[[i]][1:length(localities[[i]])-1]) #update time spent in current locality
      if (!is.null(disp)){
        if (runif(1) <= sum(disp[lineages[i,8],])) { #probability of dispersal event(sum of all probabilities of dispersal from current locality of ith lineage)
          loc_i <- disp[lineages[i,8],] #vector of dispersal probabilities to other localities
          loc_i[is.na(loc_i)] <- 0 #replace NAs (probability of staying in same locality) with 0
          lineages[i, 8] <- sample(loc, size = 1, prob = loc_i) #sample new locality
          ndisp = ndisp + 1 #update number of dispersal events
          localities[[i]][length(localities[[i]])+1] <- 0 #set time spent in new locality to 0
          names(localities[[i]]) <- c(names(localities[[i]][1:length(localities[[i]])-1]), lineages[i, 8]) #update names of localities
          if (lineages[i, 5] == -1) { #if dispersing lineage is incipient, immediately becomes good
            lineages[i, 5:7] <- c(1, t, t) #update lineage status to good in lineage matrix, update speciation completion times
            traits[[i]][2] <- 1 #update lineage status to good in trait list
          }
          if (lineages[i, 5] == 1) { #if dispersing lineage is good and has incipient daughter, daughter immediately becomes good
            i_daughter <- traits[[i]][3] #find daughter lineage of dispersing lineage
            if (lineages[i_daughter, 5] == -1) { #if daughter is incipient & alive, becomes good:
              lineages[i_daughter, c(5, 6, 7)] <- c(1, t, t) #update daughter lineage status to good in lineage matrix, update speciation completion times
              traits[[i_daughter]][2] <- 1 #update daughter lineage status to good in trait list
            }
          }
        }
      }
      
      diff_trait_opt = traits[[i]][4 + step_count] - opt #calculate difference of trait from optima
      
      if (lineages[i, 5] == -1) { #if lineage is incipient
        traits[[i]][5] <- tail(traits[[traits[[i]][3]]], 1) #update current trait of parental lineage
        diff_trait_isp = abs(traits[[i]][4 + step_count] - 
                               traits[[i]][4]) #calculate absolute value of difference from trait value of parental lineage
        lambda2 = tau0 * exp(beta * (diff_trait_isp)^2) #calculate rate of completion of speciation for species - dependent on distance with parent
        mui = alpha1 * mui0 * sum(exp(-alpha1 * (diff_me[[i]])^2)) + #calculate rate of competitive-dependent extinction for species - depends on distance with co-occurring lineages
          alpha3 * mui1 * sum(exp((alpha3 * diff_trait_opt^2))) + #calculate rate of selective-dependent extinction for species - depends on distance from optima
          muibg #add background extinction rate
        probs_i = c(lambda2/(mui + lambda2), #probability of speciation completion
                    mui/(mui + lambda2)) #probability of extinction
        if (runif(1) <= (lambda2 + mui) * step.size) { #probability that lineage does not remain incipient
          event <- sample(1:2, size = 1, prob = probs_i)
          if (event == 1) { #speciation completed
            lineages[i, 5:7] <- c(1, t, t) #update lineage status to good in lineage matrix, update speciation completion times
            traits[[i]][2] <- 1 #update lineage status to good in trait list
          }
          if (event == 2) { #extinction
            lineages[i, c(4, 5, 6)] <- c(t, -2, t) #update lineage status to extinct in lineage matrix, update ending time and extinction time
            traits[[i]][2] <- -2 #update lineage status to extinct in trait list
            dead_lin <- c(dead_lin, i) #update vector of non-active lineages in time step
          }
        }
      }
      if (lineages[i, 5] == 1) { #if lineage is good
        mu = alpha1 * mu0 * sum(exp(-alpha1 * (diff_me[[i]])^2)) + #calculate rate of competitive-dependent extinction for species - depends on distance with co-occurring lineages
          alpha3 * mu1 * sum(exp((alpha3 * diff_trait_opt^2))) + #calculate rate of selective-dependent extinction for species - depends on distance from optima
          mubg #add background extinction rate
        if (mu0 != 0 & mu == 0) { #captures the case when there's only one lineage alive & extinction is activated
          mu = 0.02 * mu0 #when alone, a lineage has basal extinction rate (equal to having infinite distance with neighbors & no selection)
        }
        probs <- c(lambda1/(lambda1 + mu), #speciation initiation probability
                   mu/(lambda1 + mu)) #extinction probability
        if (runif(1) <= (mu + lambda1) * step.size) { #probability that lineage does not remain good
          event <- sample(1:2, size = 1, prob = probs)
          if (event == 1) { #speciation initiated
            #the current lineage is ended, and is saved as the ancestral node - in its stead are created two new lineages, the new parental lineage (same lineage, new number) and the new incipient lineage:
            lineages[i, 2] <- max(lineages[, 1]) + 1 #add descendant node number
            lineages[i, 4] <- t #update ending time
            new_lin1 = c(lineages[i, 2], 0, t, -1, 1, t, t, lineages[i, 8]) #create vector for parent lineage
            new_lin2 = c(lineages[i, 2], 0, t, -1, -1, NA, NA, lineages[i, 8]) #create vector for incipient lineage
            lineages <- rbind(lineages, new_lin1, new_lin2) #add new lineages to lineage matrix
            dead_lin <- c(dead_lin, i) #update vector of non-active lineages in time step
            born_lin <- c(born_lin, dim(lineages)[1] - 1, dim(lineages)[1]) #update vector of new lineages in time step: both the parent and incipient
            #update trait list to include new lineages
            traits[[dim(lineages)[1] - 1]] <- c(dim(lineages)[1] - 1, #number of new good lineage
                                                1, #status (good)
                                                dim(lineages)[1], #number of daughter lineage (previous)
                                                NA, #trait value for parent lineage
                                                rep(NA, times = step_count), #trait values for all previous time steps (NA since it didn't exist)
                                                traits[[i]][5 + step_count]) #trait value for next time step (trait of parent lineage)
            traits[[dim(lineages)[1]]] <- c(dim(lineages)[1], #number of new incipient lineage
                                            -1, #status (incipient)
                                            dim(lineages)[1] - 1, #number of parental lineage
                                            traits[[i]][5 + step_count], #trait value for parent lineage
                                            rep(NA, times = step_count), #trait values for all previous time steps (NA since it didn't exist)
                                            traits[[i]][5 + step_count]) #trait value for next time step (trait of parent lineage)
            localities[[dim(lineages)[1]-1]] <- c(0) #locality vector for new parent lineage - time spent in locality set to 0
            names(localities[[dim(lineages)[1]-1]]) <- lineages[i, 8] #current locality inherited from parent
            localities[[dim(lineages)[1]]] <- c(0) #locality vector for new incipient lineage - time spent in locality set to 0
            names(localities[[dim(lineages)[1]]]) <- lineages[i, 8] #current locality inherited from parent
            #has already descendant incipient lineages?
            daughters <- which(unlist(lapply(traits,
                                             function(x) (x[3]))) == i) #extract lineages who have i as parent
            daughters <- daughters[lineages[daughters, 2] == 0] #keep only living (drop all non-tips)
            if (sum(daughters) > 0) {
              for (j in daughters) {
                traits[[j]][3] <- nrow(lineages) - 1 #update parental lineage number to its new number
              }
            }
          }
          if (event == 2) { #extinction
            lineages[i, c(4, 5, 6)] <- c(t, -2, t) #update lineage status to extinct in lineage matrix, update ending time and extinction time
            traits[[i]][2] <- -2 #update lineage status to extinct in trait list
            dead_lin <- c(dead_lin, i) #update vector of non-active lineages in time step
            i_daughter <- traits[[i]][3] #find daughter lineage of extinct lineage
            if (lineages[i_daughter, 5] == -1) { #if daughter is incipient & alive, becomes good:
              lineages[i_daughter, c(5, 6, 7)] <- c(1, t, t) #update daughter lineage status to good in lineage matrix, update speciation completion times
              traits[[i_daughter]][2] <- 1 #update daughter lineage status to good in trait list
            }
          }
        }
      }
    }
    if (!is.null(dead_lin)) { #if any lineages became non-active during current time step
      active_lineages <- c(active_lineages[!(active_lineages %in% dead_lin)], born_lin) #update active lineages; remove non-active, add newly active
    }
    step_count = step_count + 1L #update step count to next step
    t = t + step.size #update current time to next step
    cat("\r", "time:", t) #print out progres
    n_lineages = length(active_lineages) #update number of lineages
    if (n_lineages == 0) { #kill process if number of lineages has dropped to zero
      print("process died")
      process_dead = T
      (break)()
    }
  }
  t = t - step.size #retract step size to return back to current time
  #update time spent in last locality for all active lineages:
  # for (i in active_lineages) {
  #   localities[[i]][length(localities[[i]])] <- t - lineages[i,3] - sum(localities[[i]])
  # }
  # 
  ##BUILD TREES & GET TIPS TRAIT VECTORS
  #complete process tree
  row.names(lineages) <- NULL
  colnames(lineages) <- NULL
  edges_mat <- lineages[, 1:2] #matrix of parental and descendant nodes for each lineage
  active_lineages <- sort(c(active_lineages, which(lineages[, 5] == -2))) #sort active lineages in increasing order, after dropping extinct lineages
  n_tips = length(active_lineages) #how many active lineages at the end of process
  #update the node numbers to comply with phytools:
  edges_mat[, 1] <- edges_mat[, 1] + n_tips #update parental node numbers to number sequentially from 1 + highest tip number
  edges_mat[, 2][which(edges_mat[, 2] != 0)] <- edges_mat[, 2][which(edges_mat[, 2] != 0)] + n_tips #update ancestral node numbers for all ancestral lineages to number sequentially from 1 + highest tip number
  edges_mat[, 2][which(edges_mat[, 2] == 0)] <- 1:n_tips #change numbers of all tips to number from 1 to number of tips
  edg1 <- as.integer(edges_mat[, 1]) #vector of parental nodes per lineage
  edg2 <- as.integer(edges_mat[, 2]) #vector of node numbers per lineage (tip or internal)
  edges_mat <- cbind(edg1, edg2) #create new matrix of nodes
  dimnames(edges_mat) <- NULL
  lineages[, 4][which(lineages[, 4] == -1)] <- t #update ending time for all incipient lineages at end of process
  lin_length <- round(lineages[, 4] - lineages[, 3], 2) #calculate branch lengths per lineage (ending time - starting time)
  #generate the phylogeny:
  tree <- list(edge = edges_mat,
               edge.length = lin_length, 
               Nnode = (n_tips - 1),
               tip.label = paste("t", as.character(1:n_tips), sep = ""))
  class(tree) <- "phylo" #change class of tree
  if (!is.null(disp)){
    tree$maps <- localities
    tree$mapped.edge <- matrix(data = 0,
                               length(tree$edge.length),
                               length(loc),
                               dimnames = list(apply(tree$edge,
                                                     1,
                                                     function(x)
                                                       paste(x, collapse = ",")),
                                               state = loc))
    for (j in 1:length(tree$maps)) for (k in 1:length(tree$maps[[j]]))
      tree$mapped.edge[j, names(tree$maps[[j]])[k]] <-
      tree$mapped.edge[j, names(tree$maps[[j]])[k]] + tree$maps[[j]][k]
    class(tree) <- c("simmap", setdiff(class(tree), "simmap"))
  }
  tip_values = NULL
  tip_values <- sapply(traits[active_lineages], function(x) (x[length(x)])) #extract tip values as trait values at the last time step
  names(tip_values) <- tree$tip.label #name vector of tip values
  isp_todrop <- c(which(lineages[, 5] == -1),
                  which(lineages[, 5] == -2 & is.na(lineages[, 7]))) #create vector of all lineages that were incipient at the end of the process, or went extinct before completing speciation ('incomplete lineages')
  tip_ids <- edges_mat[isp_todrop, 2] #extract node numbers of incomplete lineages
  if(!is.null(disp)) {
    tip_ids <- paste("t", as.character(tip_ids), sep = "")
    tree_gsp_fossil <- drop.tip.simmap(tree, tip = tip_ids) #prune tree to remove incomplete lineages
  } else {
    tree_gsp_fossil <- drop.tip(tree, tip = tip_ids) #prune tree to remove incomplete lineages
  }
  tip_values_gsp_fossil <- tip_values[names(tip_values) %in% 
                                        tree_gsp_fossil$tip.label] #prune vector of tip values to remove incomplete lineages
  if (process_dead == T) {
    tree_gsp_extant <- "process died"
    tip_values_gsp_extant <- "process died"
  }
  else {
    extinct_todrop <- c(which(lineages[, 5] == -1),
                        which(lineages[, 5] == -2)) #create vector of all lineages that are incipient at end of process or went extinct during the process ('extinct lineages')
    tip_ext_ids <- edges_mat[extinct_todrop, 2] #extract node numbers of extinct lineages
    if(!is.null(disp)) {
      tip_ext_ids <- paste("t", as.character(tip_ext_ids), sep = "")
      tree_gsp_extant <- drop.tip.simmap(tree, tip = tip_ext_ids) #prune tree to remove extinct lineages
    } else {
      tree_gsp_extant <- drop.tip(tree, tip = tip_ext_ids) #prune tree to remove extinct lineages
    }
    tip_values_gsp_extant <- tip_values[names(tip_values) %in% 
                                          tree_gsp_extant$tip.label] #prune vector of tip values to remove extinct lineages
    if (is.null(disp)){
      tree_gsp_fossil <- read.tree(text = write.tree(tree_gsp_fossil)) #save fossil tree (no incomplete lineages)
      tree_gsp_extant <- read.tree(text = write.tree(tree_gsp_extant)) #save extant tree (no extinct lineages)
    }
  }
  if (is.null(disp)) {
    tree <- read.tree(text = write.tree(tree)) #save full tree
  }
  res <- list(all = list(tree = tree, tips = tip_values, disp = ndisp), 
              gsp_fossil = list(tree = tree_gsp_fossil, tips = tip_values_gsp_fossil), 
              gsp_extant = list(tree = tree_gsp_extant, tips = tip_values_gsp_extant)) #save simulation outputs
  if (full.sim == T) {
    res$all$trait_mat <- traits #save trait list to simulation output
    res$all$lin_mat <- lineages #save lineage matrix to simulation output
    res$all$loc_mat <- localities #save locality list to simulation output
  }
  if (plot) {
    plotSimu <- function(traitmat, linmat, step_size, ylims = NULL) {
      #plots a simulation, with incipient lineages in red, good in black
      steps <- max(linmat[, 4])/step_size #calculate number of steps in simulation
      max_trait <- NULL
      min_trait <- NULL
      for (i in 1:nrow(linmat)) {
        max_trait <- c(max_trait,
                       max(traitmat[[i]][-(1:4)], 
                           na.rm = T)) #extract maximal trait values for each lineage
        min_trait <- c(min_trait,
                       min(traitmat[[i]][-(1:4)], 
                           na.rm = T)) #extract minimal trait values for each lineage
      }
      if (is.null(ylims)) {
        plot(1,
             type = "n",
             xlim = c(1, steps),
             ylim = c(min(min_trait), 
                      max(max_trait)),
             ylab = "trait value",
             xlab = "Time") #generate plot area
      }
      else {
        plot(1, type = "n",
             xlim = c(1, steps),
             ylim = ylims, 
             ylab = "trait value",
             xlab = "Time") #generate plot area with pre-determined limits for trait space
      }
      completion <- linmat[, 6] #extract vector of speciation or extinction times for each lineage
      #handle and plot each lineage based on its end point:
      for (i in 1:length(completion)) {
        if (is.na(completion[i])) { #extinct incipient lineages
          lines(x = (5:length(traitmat[[i]])), #branch length
                y = traitmat[[i]][5:length(traitmat[[i]])], #trait values
                col = "red", #incipient lineages in red
                cex = 0.5)
        }
        else if (linmat[i, 5] == -2 &&
                 !is.na(linmat[i, 7]) &&
                 completion[i] != linmat[i, 7]) { #extinct good lineages
          lines(x = 5:((linmat[i, 7]/step_size) + 5), #branch length of incipient stage
                y = traitmat[[i]][5:((linmat[i, 7]/step_size) + 5)], #trait values of incipient stage
                col = "red", #incipient stage in red
                cex = 0.5)
          lines(x = ((linmat[i, 7]/step_size) + 5):length(traitmat[[i]]), #branch length of good stage
                y = traitmat[[i]][((linmat[i, 7]/step_size) + 5):length(traitmat[[i]])], #trait value of good stage
                col = "black", #good stage in black
                cex = 0.5)
        }
        else { #extant good and incipient lineages
          lines(x = 5:((completion[i]/step_size) + 5), #branch length of incipient stage
                y = traitmat[[i]][5:((completion[i]/step_size) + 5)], #trait values of incipient stage
                col = "red", #incipient stage in red
                cex = 0.5)
          if (length(traitmat[[i]]) > (round(completion[i]/step_size) +  5)) { #if completed speciation before end of process
            lines(x = ((completion[i]/step_size) + 5):length(traitmat[[i]]), #branch length of good stage
                  y = traitmat[[i]][((completion[i]/step_size) + 5):length(traitmat[[i]])], #trait values of good stage
                  col = "black", #good stage in black
                  cex = 0.5)
          }
        }
      }
    }
    if (length(ylims) < 2) { #make sure limits on y axis aren't too small
      ylims = NULL
    }
    plot <- plotSimu(traits, lineages, step.size, ylims) #plot the simulation
  }
  return(res) #return simulation output
}


plotMCSBDD <- function(traitmat, linmat, step_size, ylims = NULL) {
  #plots a simulation, with incipient lineages in red, good in black
  steps <- max(linmat[, 4])/step_size #calculate number of steps in simulation
  max_trait <- NULL
  min_trait <- NULL
  for (i in 1:nrow(linmat)) {
    max_trait <- c(max_trait,
                   max(traitmat[[i]][-(1:4)], 
                       na.rm = T)) #extract maximal trait values for each lineage
    min_trait <- c(min_trait,
                   min(traitmat[[i]][-(1:4)], 
                       na.rm = T)) #extract minimal trait values for each lineage
  }
  if (is.null(ylims)) {
    plot(1,
         type = "n",
         xlim = c(1, steps),
         ylim = c(min(min_trait), 
                  max(max_trait)),
         ylab = "trait value",
         xlab = "Time") #generate plot area
  }
  else {
    plot(1, type = "n",
         xlim = c(1, steps),
         ylim = ylims, 
         ylab = "trait value",
         xlab = "Time") #generate plot area with pre-determined limits for trait space
  }
  completion <- linmat[, 6] #extract vector of speciation or extinction times for each lineage
  #handle and plot each lineage based on its end point:
  for (i in 1:length(completion)) {
    if (is.na(completion[i])) { #extinct incipient lineages
      lines(x = (5:length(traitmat[[i]])), #branch length
            y = traitmat[[i]][5:length(traitmat[[i]])], #trait values
            col = "red", #incipient lineages in red
            cex = 0.5)
    }
    else if (linmat[i, 5] == -2 &&
             !is.na(linmat[i, 7]) &&
             completion[i] != linmat[i, 7]) { #extinct good lineages
      lines(x = 5:((linmat[i, 7]/step_size) + 5), #branch length of incipient stage
            y = traitmat[[i]][5:((linmat[i, 7]/step_size) + 5)], #trait values of incipient stage
            col = "red", #incipient stage in red
            cex = 0.5)
      lines(x = ((linmat[i, 7]/step_size) + 5):length(traitmat[[i]]), #branch length of good stage
            y = traitmat[[i]][((linmat[i, 7]/step_size) + 5):length(traitmat[[i]])], #trait value of good stage
            col = "black", #good stage in black
            cex = 0.5)
    }
    else { #extant good and incipient lineages
      lines(x = 5:((completion[i]/step_size) + 5), #branch length of incipient stage
            y = traitmat[[i]][5:((completion[i]/step_size) + 5)], #trait values of incipient stage
            col = "red", #incipient stage in red
            cex = 0.5)
      if (length(traitmat[[i]]) > (round(completion[i]/step_size) +  5)) { #if completed speciation before end of process
        lines(x = ((completion[i]/step_size) + 5):length(traitmat[[i]]), #branch length of good stage
              y = traitmat[[i]][((completion[i]/step_size) + 5):length(traitmat[[i]])], #trait values of good stage
              col = "black", #good stage in black
              cex = 0.5)
      }
    }
  }
}