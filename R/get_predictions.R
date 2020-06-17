#' @title get_predictions
#' @description send text input to API and receive predictions
#' @param text character input of length 1
#' @param output set to "word" (default) for BIO output or "sentence" for sentence level output
#' @param verbose logical; print flow of information (default FALSE)
#' @param sleep rest time between requests (in seconds; defaults to zero)
#' @return returns dataframe
#' @examples
#' \dontrun{
#' text <- "Seehofer fordert eine Obergrenze. Merkel lehnt eine Obergrenze ab."
#' df <- get_predictions(text = text, output = "sentence")
#' }
#' @export
#' @importFrom magrittr %>%

get_predictions <- function(text, output = "word", verbose = F, sleep = 0){
          stopifnot(length(text) == 1)
          url <- "https://clarin09.ims.uni-stuttgart.de/claimidentifier/2"
          body <- list(text = text)
          if(isTRUE(verbose)){
                    r <- httr::POST(url, body = body, encode = "json", httr::verbose())
          } else {
                    r <- httr::POST(url, body = body, encode = "json")
          }
          stopifnot("Server currently not available." = httr::status_code(r) < 300)
          l <- httr::parsed_content(r)
          run <- function(x){
                    unlist(l[[x]]) %>%
                              tibble::tibble(type = names(.), value = .) %>%
                              dplyr::mutate(sentence = x,
                                            id = rep(1:(nrow(.)/2), each = 2)) %>%
                              tidyr::spread(., type, value)
          }
          df <- purrr::map_df(1:length(l), run)
          if(output != "word"){
                    df <-  df %>%
                              dplyr::group_by(sentence) %>%
                              dplyr::mutate(text = paste(word, collapse = " ")) %>%
                              dplyr::arrange(prediction) %>%
                              dplyr::distinct(sentence, .keep_all = T) %>%
                              dplyr::select(sentence, prediction, text) %>%
                              dplyr::arrange(sentence)
          }
          if(sleep != 0){
                    Sys.sleep(sleep)
          }
          return(df)
}
