#' DEbateNet-mig15 data set
#'
#' A dataset containing 1815 annotated text spans including claims from the domestic discourse on migration in Germany in the year 2015.
#' @docType data
#' @format A data frame with 1815 rows and 5 variables:
#' \describe{
#'   \item{actorclusters}{nested dataframe, containing actor information, such as name and party affiliation}
#'   \item{claimvalues}{list containing claim categories}
#'   \item{cpos}{polarity of claims (positive or negative)}
#'   \item{cdate}{date, when the claim was reported in the newspaper}
#'   \item{quote}{quote, the annotated text span}
#' }
#' @usage data(DEbateNet_mig15)
#' @references Lapesa, G., Blessing, A., Blokker, N., Dayanik, E., Haunss, S., Kuhn, J., Pado, S.: DEbateNet-mig15: Tracing the 2015 Immigration Debate in Germany Over Time. In: Proceedings of LREC. , Marseille, France (2020).
"DEbateNet_mig15"
