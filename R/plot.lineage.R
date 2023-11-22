## FOR DEBUG USE ONLY
# @title Convert lineage to topology
#
# @description Converts a lineage object into a tree object (for visualisation)
#
# @param lineage  The lineage object (internal to birth.death_fun.R)
# @param edge_lengths  Optional, the edge_lengths
# @param element.names logical, whether to display the elements as named in lineage (TRUE) or as named in the output tree (FALSE)
# @param plot logical, also plots the tree (TRUE) or not (FALSE)
internal.plot.lineage <- function(lineage, edge_lengths, element.names = TRUE, plot = TRUE) {

    ## Missing edge lenghts
    if(missing(edge_lengths)) {
        edge_lengths <- rep(1, length(lineage$parents))
    }

    ## Summarise into a table (minus the initiation)
    table <- data.frame(parent       = lineage$parents, # These are nodes
                        element      = seq_along(lineage$split), # These are tips or nodes
                        edge_lengths = edge_lengths,
                        is_node      = lineage$split)[-1, ]

    n_nodes <- sum(table$is_node)+1
    n_tips  <- sum(!table$is_node)

    ## Getting the edge table node/tips IDs
    table$element2 <- NA
    table$element2[!table$is_node] <- 1:n_tips
    table$element2[ table$is_node] <- order(table$element[table$is_node]) + n_tips + 1

    ## Getting the edge table nodes (left column)
    left_edges <- match(table$parent, table$element)
    table$parent2 <- table$element2[left_edges]
    ## Add the root (is NA)
    table$parent2[is.na(table$parent2)] <- n_tips + 1

    ## Getting the tips and nodes labels
    tip_labels_IDs <- table$element[match(1:n_tips, table$element2)]
    node_labels_IDs <- c(1, na.omit(table$element[match((n_tips+1):(n_tips+n_nodes), table$element2)]))

    if(!element.names) {
        tree_tips_labels <- paste0("t", 1:n_tips)
        tree_node_labels <- paste0("n", 1:n_nodes)
    } else {
        tree_tips_labels <- tip_labels_IDs
        tree_node_labels <- node_labels_IDs
    }

    tree <- list(edge        = cbind(table$parent2, table$element2),
                 Nnode       = n_nodes,
                 tip.label   = tree_tips_labels,
                 node.label  = tree_node_labels,
                 edge.length = table$edge_lengths)
    class(tree) <- "phylo"

    if(plot) {
        plot(tree)
        axisPhylo()
        nodelabels(tree$node.label)
        ## Highlight the livings
        livings <- rep(NA, n_tips)
        livings[tip_labels_IDs %in% lineage$livings] <- 1
        tiplabels(pch = livings, bg = NULL, col = "black")
        ## Highlight the current
        current <- rep(NA, n_tips)
        current[which(tip_labels_IDs == lineage$livings[lineage$drawn])] <- 19
        tiplabels(pch = current, bg = NULL, col = "red")
        ## Legend
        legend("bottomleft", pch = c(19,1), col = c("red", "black"), legend = c("current", "living"))
    }
    return(tree)
}
