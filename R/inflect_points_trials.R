#' @title Find inflection points for multiple trials
#'
#' @description `inflect_points_trials` finds inflection points of waveform
#' data for a range of trials
#'
#' @param dataframe a fluoR-format dataframe
#' @param trials trial numbers to find inflection points for
#' @return a dataframe of timestamps and corresponding inflection points for
#' each specified trial
#' @examples
#' ### Format data frame
#' df <- format_data(GCaMP)
#'
#' ### Find inflection points for first 4 trials
#' inflect_points_trials(dataframe = df, trials = c(1,2,3,4))
#' @export

inflect_points_trials <- function(dataframe, trials){

  trial.inds <- trials + 1

  ### Make data frame of time, inflection points, and raw values
  df <- dataframe[,trial.inds]
  inf.pts <- as.data.frame(sapply(df, inflect_points))

  ### Make full data frame
  df <- cbind(dataframe[1], inf.pts)

  return(df)

}