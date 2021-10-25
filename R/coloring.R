#' @title coloring
#' @description colors nodes of the network according to party affiliation
#' @param g the graph object
#' @param data the dataframe containing party information
#' @return returns a color-vector
#' @examples
#' \dontrun{
#' coloring(g, data = data)
#' }
#' @noRd
#' @import igraph

coloring <- function(g, data = data){
          V(g)$party <- NA
          for(i in 1:igraph::vcount(g)){
                    idx <- which(data$name == V(g)$name[i])
                    if(length(idx) > 0){
                              V(g)$party[i] <- unique(unlist(data$parties[idx]))[1]
                    } else {
                              V(g)$party[i] <- "claim"
                    }
          }
          V(g)$party[is.na(V(g)$party)] <- "without"
          party_colors <- c("AfD" = "darkblue",
                            "CDU" = "black",
                            "CSU" = "deepskyblue",
                            "SPD" = "red",
                            "Green" = "forestgreen",
                            "Left" = "darkorchid",
                            "FDP" = "yellow",
                            "NPD" = "orange",
                            "without" = "slategrey",
                            "claim" = "grey")
          V(g)$color <- unname(party_colors[V(g)$party])
          V(g)$color
}
