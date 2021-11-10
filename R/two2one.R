#' @title two2one
#' @description projects two-mode discourse networks
#' @param df the dataframe
#' @param ret the desired output (options are `graph` and `dataframe`)
#' @param iso logical; whether to include isolates
#' @return returns the one-mode projection with agreement and conflict edge-attributes
#' @examples
#' two2one(df, iso = TRUE, ret = "graph")
#' @details calculations and code for this function are based on https://github.com/shaunss/rdnatools/
#' @noRd
#' @importFrom stats xtabs
#' @import igraph

two2one <- function(df, ret = "graph", iso = TRUE){
        df[, 3] <- ifelse(df[, 3] < 0, -1, 1)
        colnames(df) <- c("from","to","weight")
        mpos <- mneg <- m <- stats::xtabs(weight ~ from + to, data = df)
        mpos[mpos < 0] <- 0
        mneg[mneg > 0] <- 0
        prj <-  function(matrix1, matrix2 = NULL, iso = iso){
                if(is.null(matrix2) & sum(matrix1) > 0){
                        matrix_trans <- tcrossprod(matrix1)
                        comment(matrix_trans) <- "pos"
                }
                if(is.null(matrix2) & sum(matrix1) <= 0){
                        matrix_trans <- tcrossprod(matrix1)
                        comment(matrix_trans) <- "neg"
                }
                if(!is.null(matrix2)){
                        matrix_trans <- (tcrossprod(matrix1, matrix2) + tcrossprod(matrix2, matrix1))*-1
                        comment(matrix_trans) <- "conf"
                }
                graph <- igraph::graph.adjacency(matrix_trans, diag = iso, weighted = T, mode = "upper")
                E(graph)$coalition <- attributes(matrix_trans)$comment
                E(graph)$cpos <- c("pos" = 1, "neg" = -1, "conf" = 2)[E(graph)$coalition]
                E(graph)$color <- c("pos" = "green", "neg" = "red", "conf" = "grey")[E(graph)$coalition]
                projection <- igraph::get.data.frame(graph)
                return(projection)
        }
        elist <- purrr::map2_df(list(mpos, mneg, mpos), list(NULL, NULL, mneg), prj, iso = iso)
        if(ret == "graph"){
                graph_prj <- graph_from_data_frame(elist, directed = F)
                graph_prj <- simplify(graph_prj, remove.multiple = F, remove.loops = T)
                return(graph_prj)
        } else {
                return(elist)
        }
}

