#' @title Get peaks/valleys in waveform data
#'
#' @description `find_peaks` finds peaks or valleys in waveform by using
#' inflection points, with filter of 'n' increasing/decreasing points on both
#' sides of each inflection point.
#'
#' @param xvals vector of numbers
#' @param n.points  the number of decreasing (for peaks) or increasing (valleys)
#' data points on left and right of inflection point required to be considered
#' a "peak". A positive number as an input finds peaks, and a negative number
#' finds valleys.
#' @return a numeric vector of indices
#' @examples
#' ### Format data frame
#' df <- format_data(GCaMP)
#'
#' ### How many peaks are there in trial 1
#' ### with 10 decreasing data points on each side?
#' peak.indices <- find_peaks(df$Trial1, n.points = 10)
#'
#' ### When do they occur?
#' data.frame(times = df$Time[peak.indices],
#' vals = df$Trial1[peak.indices])
#' @export

find_peaks <- function (xvals, n.points = 3){

  if(n.points >= 0){
    xvals = xvals
  } else {
    xvals = -xvals
  }

  # define curves and peaks
  curves <- diff(sign(diff(xvals, na.pad = FALSE)))
  peakz <- sapply(which(curves < 0), FUN = function(i){

    b <- i - n.points + 1
    b <- ifelse(b > 0, b, 1)
    a <- i + n.points + 1
    a <- ifelse(a < length(xvals), a, length(xvals))
    if(all(xvals[c(b : i, (i + 2) : a)] <= xvals[i + 1])){
      return(i + 1)
    } else {
      return(numeric(0))
    }

  })

  peakz <- unlist(peakz)
  return(peakz)

}