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
#' @return the input vector transformed into z scores
#' @examples
#' df <- format_data(GCaMP)
#'
#' z_score(xvals = df$Trial1)
#' @export

z_score <- function(xvals, mu = NULL, sigma = NULL,
                    z.type = 'standard'){

  ### mean
  if(is.null(mu)){
    if(z.type == 'modified'){
      mu <- median(xvals)
    } else if (z.type == 'standard') {
      mu <- mean(xvals)
    } else {
      print('Invalid z type. Please specify "standard" or "modified"')
    }
  }

  ### sd/mad
  if(is.null(sigma)){
    if(z.type == 'modified'){
      sigma <- median(abs(xvals - median(xvals)))
    } else if (z.type == 'standard') {
      sigma <- sd(x = xvals)
    } else {
      print('Invalid z type. Please specify "standard" or "modified"')
    }
  }

  ### Formula follows user input
  # if modified, mu = median & sigma = mad
  # if standard, mu = mean & sigma = sd
    if(z.type == 'modified'){
      z.score <- 0.6745 * (xvals - mu) / sigma
    } else if (z.type == 'standard') {
      z.score <- (xvals - mu) / sigma
    } else {
      print('Invalid z type. Please specify "standard" or "modified"')
    }

  return(z.score)

}
