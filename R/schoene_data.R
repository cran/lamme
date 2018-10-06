#' Data on Interactive Cognitive-Motor Step Training
#'
#' Data from a randomized controlled trial on Interactive cognitive-motor step training.
#' 81 observations are included. The outcome variable included is the hand reaction time.
#' The data come from a randomzied pretest-posttest design with control and treatment groups.
#'
#' @docType data
#'
#' @format A dataframe with 81 rows and 3 variables:
#' \describe{
#'   \item{group}{treatment or control group from experimental manipulation}
#'   \item{pre_HRT}{prettest hand reaction time}
#'   \item{post_HRT}{posttest hand reaction time}
#' }
#'
#' @usage data(schoene)
#'
#' @keywords datasets
#'
#' @references Schoene D, Valenzuela T, Toson B, Delbaere K, Severino C, Garcia J, et al. (2015) Interactive Cognitive-Motor Step Training Improves Cognitive Risk Factors of Falling in Older Adults – A Randomized Controlled Trial. PLoS ONE 10(12): e0145161.
#'
#' @examples
#' data(schoene)
#' head(schoene)
#' table(schoene$group)
"schoene"
