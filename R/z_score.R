#' @title Transform values into z scores
#'
#' @description `z_score` transforms a vector of values into standard z scores
#' or modified z scores
#'
#' @importFrom stats median sd mad
#' @param xvals a vector of values to be transformed into z scores
#' @param mu a specified value for the input vector to be centered at
#' @param sigma a specified value for the input vector to be scaled by
#' @param z.type the type of summary statistic being used for mu
#' @param mad.const the multiplier constant for computing the
#' median absolute deviation (MAD) if using a modified z score
#' @return the input vector transformed into z scores
#' @examples
#' df <- format_data(GCaMP)
#'
#' z_score(xvals = df$Trial1)
#' @export

z_score <- function(xvals, mu = NULL, sigma = NULL,
         z.type = 'standard',
         mad.const = 1.4826){

  ### mean
  if(is.null(mu) & z.type == 'modified'){
    mu <- median(xvals)
  } else {
    mu <- mean(xvals)
  }

  ### sd/mad
  if(is.null(sigma) & z.type == 'modified'){
    sigma <- mad(x = xvals, constant = mad.const)
  } else {
    sigma <- sd(x = xvals)
  }

  ### Formula follows user input
  z.score <- (xvals - mu) / sigma

  return(z.score)

}
