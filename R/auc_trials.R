#' @title Calculate area under curve
#'
#' @description `auc_trials` calculates area under curve for multiple
#' trials using trapezoidal integration
#'
#' @importFrom pracma trapz
#' @param dataframe a fluoR-format dataframe
#' @param trials trial numbers to find AUC for
#' @param time.range range of time to find AUC for
#' @return a named vector of trial AUCs
#' @examples
#' ### What is the AUC for the first 2 seconds of post-onset
#' ### for the first 5 trials?
#' df <- format_data(GCaMP)
#' trials <- 1:5
#' time.range <- c(0,2)
#'
#' auc_trials(dataframe = df, time.range = time.range, trials = trials)
#' @export

auc_trials <- function(dataframe, trials, time.range){
  trial.inds <- trials + 1

  ### Subset dataframe
  dataframe <- dataframe[dataframe[,1] > time.range[1] &
                           dataframe[,1] <= time.range[2], ]

  ### Find AUCs
  AUCs <- sapply(trial.inds, function(trial){
    ret = trapz(x = dataframe[,1], y = dataframe[,trial])
  })
  names(AUCs) <- paste0('Trial', trial.inds)

  return(AUCs)

}
