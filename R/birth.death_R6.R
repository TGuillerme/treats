## R6 business

## The building tree object
build.tree <- R6Class("build.tree",
    ## Bare R6 object (increase performances?)
    #TODO: toggle to bare (all FALSE) after debuging
    portable  = TRUE,
    class     = TRUE,
    cloneable = TRUE,
    ## Data
    public = list(
        ## Components (objects)
        lineage   = NULL, # The lineage tracker
        time      = NULL, # The time tracker
        edge_lengths = NULL, # The edge lengths vector
        was_alive = NULL, # Recording which lineage go extinct
        founding_tree = NULL, #TODO: might need to remove this one
        first_waiting_time = NULL,
        waiting_time = NULL,
        traits    = NULL,
        events    = NULL,
        trait_table = NULL,
        modifiers = list(list("waiting"    = list(fun = branch.length.fast,
                                                  internal = NULL),
                              "selecting"  = list(fun = selection.fast,
                                                  internal = NULL),
                              "speciating" = list(fun = speciation.fast,
                                                  internal = NULL))
                        ), # A list of modifiers functions (these could be accessed using do.modifiers(pointer) {list[[pointer]]fun()})

        ## Functions for initialising the tree
        initialize = function() {
            self$lineage      <-  list(
                 "parents" = 0L,   # The list of parent lineages
                 "livings" = 1L,   # The list of lineages still not extinct
                 "drawn"   = 1L,   # The lineage ID drawn (selected)
                 "current" = 1L,   # The current focal lineage
                 "n"       = 1L,   # The number of non extinct lineages
                 "split"   = FALSE)
            self$time         <- 0
            self$edge_lengths <- 0
            self$was_alive    <- 0L
        },
        set.modifiers = function(fun) {
            self$modifiers[[length(self$modifiers)]] <- fun
        },
        set.traits = function(fun) {
            if(!is.null(self$traits)) {
                self$traits[[length(self$traits)]] <- fun
            } else {
                self$traits <- list(fun)
            }
        },
        set.events = function(fun) {
            if(!is.null(self$events)) {
                self$events[[length(self$events)]] <- fun
            } else {
                self$events <- list(fun)
            }
        },

        ## Initial pass (first) wait + split
        first.wait = function(bd.params) {
            ## Get the waiting time
            self$first_waiting_time <- self$modifiers[[1]]$waiting$fun(
                bd.params      = bd.params,
                lineage        = self$lineage,
                trait.values   = NULL,
                modify.fun     = NULL)
            ## Update time
            self$time <- self$time + self$first_waiting_time
            ## Update branch length
            self$edge_lengths[self$lineage$living] <- self$edge_lengths[self$lineage$living] + self$first_waiting_time
            invisible(self)
        },

        ## Create a new lineage
        new.lineage = function() {
            ## Creating the new lineages
            new_lineage <- length(self$lineage$split) + 1:2
            self$lineage$split[self$lineage$current] <- TRUE
            self$lineage$split[new_lineage] <- FALSE
            self$lineage$parents[new_lineage] <- self$lineage$current
            self$edge_lengths[new_lineage] <- 0
            self$lineage$n <- self$lineage$n + 1L
            self$lineage$livings <- c(self$lineage$livings[-self$lineage$drawn], new_lineage)
            self$was_alive <- 0L
            invisible(self)
        },
        ## Get waiting time
        add.wait.time = function(bd.params) {
            ## Select the waiting time
            self$waiting_time <- self$modifiers[[1]]$waiting$fun(
                bd.params    = bd.params,
                lineage      = self$lineage,
                trait.values = self$trait_values,
                modify.fun   = self$modifiers[[1]]$waiting$internal)
            ## Update the timer
            self$time <- self$time + self$waiting_time
            invisible(self)
        },
        ## Updating branch lengths
        update.branch.length = function() {
            self$edge_lengths[self$lineage$livings] <- self$edge_lengths[self$lineage$livings] + self$waiting_time
            invisible(self)
        },
        ## Go extinct
        go.extinct = function() {
            self$was_alive <- self$lineage$livings[self$lineage$drawn]
            self$lineage$livings <- self$lineage$livings[-self$lineage$drawn]
            self$lineage$n <- self$lineage$n - 1L
            invisible(self)
        },


        ## Draw a lineage
        draw.lineage = function(bd.params) {
            ## Draw a lineage
            self$lineage$drawn <- self$modifiers[[1]]$selecting$fun(
                bd.params    = bd.params,
                lineage      = self$lineage,
                trait.values = self$trait_values,
                modify.fun   = self$modifiers[[1]]$selecting$internal)
            self$lineage$current <- self$lineage$livings[self$lineage$drawn]
            invisible(self)
        },

        ## Functions for modifiers/traits/events
        apply.modifiers = function(pointer) {
            self$lineage <- self$modifiers[[pointer]]()
            invisible(self) #(maybe use this for reducing memory?)
        },
        apply.traits = function(pointer) {
            self$lineage <- self$traits[[pointer]]()
            invisible(self) #(maybe use this for reducing memory?)
        },
        apply.events = function(pointer) {
            self$lineage <- self$events[[pointer]]()
            invisible(self) #(maybe use this for reducing memory?)
        }
    )
)