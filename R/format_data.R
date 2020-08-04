#' @title Format dataframe for use with fluoR
#'
#' @description `format_data` automatically formats dataframe for use with fluoR
#'
#' @param dataframe a dataframe with timestamps in the first row or column
#' and trial values in subsequent ones
#' @return a dataframe of timestamps in the first column and trial values in
#' subsequent ones
#' @examples
#' ### Coerce input matrix to fluoR-format dataframe
#' df <- format_data(GCaMP)
#'
#' @export

format_data <- function(dataframe) {

  ### Transpose data if not in long format
  if(ncol(dataframe) > nrow(dataframe)){
    dataframe <- t(dataframe)
    dataframe <- as.data.frame(dataframe)
    row.names(dataframe) <- c()
  }

  ### Label time
  colnames(dataframe)[1] <- "Time"

  ### label iters
  trial.nums <- 1:(ncol(dataframe) - 1)

  ### Label trials
  colnames(dataframe)[2:ncol(dataframe)] <- sprintf("Trial%d", trial.nums)

  return(dataframe)

}
