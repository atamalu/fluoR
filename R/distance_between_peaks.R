#' @title Find distance between peaks
#'
#' @description `distance_between_peaks` finds the distance between wave peaks
#'
#' @param dataframe a fluoR-format dataframe
#' @param trial trial number for input
#' @param time.range range of time to subset
#' @param n.points the number of decreasing data points on left and right of
#' inflection point required to be considered a "peak"
#' @return a dataframe of information on peaks and differences between them
#' @examples
#' ### Format data frame
#' df <- format_data(GCaMP)
#'
#' ### Find distance between peaks for 4s post-stimulus onset for trial 2
#' distance_between_peaks(dataframe = df, trial = 2, time.range = c(0, 4))
#' @export

distance_between_peaks <- function(dataframe, trial, time.range,
                                   n.points = 3){

  trial.ind <- trial + 1

  ### Subset dataframe
  dataframe <- dataframe[dataframe[,1] >= time.range[1] &
                           dataframe[,1] <= time.range[2], ]

  ### Find peaks
  pks <- find_peaks(xvals = dataframe[,trial.ind],
                    n.points = n.points) # find peaks

  ### Start & stop points
  pks.start <- pks[-length(pks)]
  pks.stop <- pks[-1]
  pks.distance.ind <- pks.stop - pks.start
  pks.distance.time <- abs(abs(dataframe[pks.stop, 1]) -
    abs(dataframe[pks.start, 1]))
  pks.distance.intensity <- dataframe[pks.stop, trial.ind] -
    dataframe[pks.start, trial.ind]

  pk1.intensity <- dataframe[pks.start, trial.ind]
  pk2.intensity <- dataframe[pks.stop, trial.ind]

  ### Add variables to data frame
  ret <- data.frame(index.start = pks.start,
                    index.stop = pks.stop,
                    index.distance = pks.distance.ind,
                    peak.diff.time = pks.distance.time,
                    peak1.intensity = pk1.intensity,
                    peak2.intensity = pk2.intensity,
                    peak.diff.intensity = pks.distance.intensity)

  return(ret)

}
