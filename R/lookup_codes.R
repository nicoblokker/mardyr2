#' @title lookup_codes
#' @description convert codes into category names
#' @param code the code category as string or integer
#' @param language set to return 'english' (default) or 'german' category names
#' @return returns vector containing category names. Multiple queries are possible.
#' @examples
#'  lookup_codes(code = 102)
#'  lookup_codes(c(303, 101, "801"), language = "german")
#' @export
#' @importFrom magrittr %>%
lookup_codes <- function(code, language = "english"){
          find_c <- function(code){
                    if(language == "english"){
                              label <- migration_codebook_english$description[which(migration_codebook_english$sub == code)]
                    } else {
                              label <- migration_codebook_german$description[which(migration_codebook_german$sub == code)]
                    }
                    if(length(label) == 1){
                              names(label) <- code
                              return(label)
                    } else {
                              label <- NA
                              names(label) <- code
                              warning(paste0("Category '", code, "' does not exist, returning NA"), call. = FALSE)
                              return(label)

                    }
          }
          sapply(code, find_c, USE.NAMES = FALSE)

}
