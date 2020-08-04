#' @title Summarize data across trials
#'
#' @description `summarize_trials` performs the specified summary statistic
#' across trials for all applicable timestamps
#'
#' @importFrom stats median sd mad
#' @param dataframe a fluoR-format dataframe
#' @param trials a vector of trials to summarize across
#' @param summary.type the type of summary statistic to compute
#' @return a dataframe of timestamps and summarized trial values
#' @examples
#' ### Format data frame
#' df <- format_data(GCaMP)
#'
#' ### Average across trials 1, 2, 3, and 4
#' trial.vec <- c(1, 2, 3, 4)
#' type <- 'mean'
#'
#' summarize_trials(dataframe = df, trials = trial.vec, summary.type = type)
#' @export

summarize_trials <- function(dataframe, trials, summary.type = 'mean'){
  trial.inds <- trials + 1

  ret <- apply(dataframe[,trial.inds], 1, function(x){
    switch(
      summary.type,
      'mean' = mean(x),
      'median' = median(x),
      'sd' = sd(x),
      'mad' = mad(x)
      )
    })

  ret <- cbind(dataframe[1], summ.trials = ret)

  return(ret)
}