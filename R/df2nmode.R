#' @title df2nmode
#' @description transforms (wide) `dataframe` into (long) edgelists to work with `igraph`
#' @param df the dataframe
#' @param n the number of columns. Last column equals edgeweight
#' @return returns edgelist
#' @examples
#' df <- data.frame(from = 1:4,
#'                  to = c("a", "b", "b", "a"),
#'                  frame = paste0("f", 4:1),
#'                  time = paste0("t", c(1,1,2,2)),
#'                  weight = 1,
#'                  stringsAsFactors = FALSE)
#'
#' # 1st example
#' df2nmode(df, n = 4)
#'
#' # 2nd example
#' for (i in 3:5) {
#'           x <- df2nmode(df, n = i)
#'           g <- igraph::graph_from_data_frame(x, directed = FALSE)
#'           plot(g, main = paste("ncol equal to", i))
#'           Sys.sleep(2)
#' }
#' @export

df2nmode <- function(df, n = 4){
          df <- df[,1:n]
          do.call(rbind, lapply(1:nrow(df), function(x) data.frame(cbind(t(utils::combn(df[x,-n], 2)), df[x, n]))))
}
