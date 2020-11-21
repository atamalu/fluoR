#' @title Calculate percent change
#'
#' @description `percent_change` transforms a numeric vector into percent
#' change from a base value
#'
#' @param xvals values to be transformed
#' @param base.val a baseline value for percent change to be scaled by
#' @return a numeric vector of percent changes from baseline value
#' @examples
#' ### Format data frame
#' df <- format_data(GCaMP)
#'
#' base.vals <- df$Trial1[df$Time <= 0] # extract pre-event vector (baseline)
#' base.mean <- mean(base.vals)
#'
#' percent_change(xvals = df$Trial1, base.val = base.mean)
#' @export

percent_change <- function(xvals, base.val){
  p.baseline <- ( (xvals - base.val) / abs(base.val)) * 100
  return(p.baseline)
}
