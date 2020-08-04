#' @title Find inflection points
#'
#' @description `inflect_points` finds inflection points of waveform data
#'
#' @param x a numeric vector
#' @return vector of inflection points (-2 or 2) with 0's filling the
#' points in-between
#' @examples
#' ### Format data frame
#' df <- format_data(GCaMP)
#'
#' ### Find inflection points for 1st trial
#' inflect_points(df$Trial1)
#' @export

inflect_points <- function(x){

  # Find inflection points
  inf.pts <- diff(sign(diff(x, na.pad = TRUE)))

  # Add NAs since we removed 2 points by doing first difference twice
  inf.pts <- append(inf.pts, 0)
  inf.pts <- append(inf.pts, 0)

  return(inf.pts)

}
