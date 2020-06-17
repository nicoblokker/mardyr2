#' @title project
#' @description projects two-mode discourse networks
#' @param df the dataframe
#' @param mode the desired output (options are `concept` and `actor`, defaults to `actor`)
#' @return returns the one-mode projection with agreement and conflict edge-attributes
#' @examples
#' project(df)
#' @noRd
#' @importFrom stats xtabs

project <- function(df){ # network data frame
          df[,3] <- ifelse(df[,3]<0, -1, 1)   # binarize data frame
          colnames(df) <- c("x","y","z")      # set standard column names
          m <- stats::xtabs(z~x+y, data=df)          # create matrix from data frame
          # positive links
          m_p <- m                            # create matrix containing only the positive links
          m_p[m_p < 0] <- 0
          g1m <- m_p %*% t(m_p)               # bipartite projection for this matrix
          diag(g1m) <- 0                      # remove loops
          g1m[lower.tri(g1m)] <- 0            # remove redundant information
          g1 <- graph.adjacency(g1m, weighted = TRUE) # create network from matrix
          E(g1)$coalition <- "pos"            # set coalition value
          E(g1)$color <- "green"              # set edge color for this coalition
          g1e <- cbind(get.edgelist(g1,names=TRUE), E(g1)$weight, E(g1)$coalition, E(g1)$color); # create edgelist from network
          # negative links
          m_n <- m
          m_n[m_n > 0] <- 0
          g2m <- m_n %*% t(m_n)
          diag(g2m) <- 0
          g2m[lower.tri(g2m)] <- 0
          g2 <- graph.adjacency(g2m, weighted = TRUE)
          E(g2)$coalition <- "neg"
          E(g2)$color <- "red"
          g2e <- cbind(get.edgelist(g2,names=TRUE), E(g2)$weight, E(g2)$coalition, E(g2)$color);
          # conflict links
          g3m <- (m_p %*% t(m_n) + m_n %*% t(m_p))*-1 # sum of the two disagreeement projections
          diag(g3m) <- 0
          g3m[lower.tri(g3m)] <- 0
          g3 <- graph.adjacency(g3m, weighted = TRUE)
          E(g3)$coalition <- "conf"
          E(g3)$color <- "grey"
          g3e <- cbind(get.edgelist(g3,names=TRUE), E(g3)$weight, E(g3)$coalition, E(g3)$color);
          # all graphs combined
          df_all <- data.frame(rbind(g1e,g2e,g3e)) # combine the three dataframes
          colnames(df_all) <- c("actor", "actor2", "weight", "coalition", "color") # set meaningful column names
          g <- graph.data.frame(df_all, directed=FALSE); # create graph from combined data frame
          E(g)$strength <- E(g)$weight          # re-create signed and unsigned weight values
          E(g)$weight <- abs(as.numeric(E(g)$strength))
          E(g)$cpos <- ifelse(E(g)$color == "green", 1,
                              ifelse(E(g)$color == "red", -1, 2))
          return(g)
}
