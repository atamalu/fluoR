#' @title Transform trial values relative to baseline
#'
#' @description `baseline_transform` transforms a fluoR dataframe into change
#' from a specified baseline period
#'
#' @importFrom stats mad sd
#' @param dataframe a fluoR-format dataframe
#' @param trials trial numbers to transform
#' @param baseline.times a range of values specifying a baseline period
#' @param type type of transformation to use on dataframe input
#' @return a dataframe of transformed values for the specified trials
#' @examples
#' ### Format data frame
#' df <- format_data(GCaMP)
#'
#' ### Standardize variables relative to baseline using z-scores
#' baseline_transform(dataframe = df, trials = 1:5, baseline.times = c(-4,0),
#' type = 'z_standard')
#' @export

baseline_transform <- function(dataframe, trials,
                                 baseline.times, type = 'z_standard'){

  supported.types <- c('z_standard', 'z_modified', 'percent_change')

  trial.inds <- trials + 1
  start.time = baseline.times[[1]]; stop.time = baseline.times[[2]]

  baseline.df <- dataframe[dataframe[,1] >= start.time &
                             dataframe[,1] <= stop.time,]

  ### Error handling
  if(length(setdiff(type, supported.types)) > 0){
    sp.tps <- paste(supported.types, collapse = ' ; ')
    ret <- print(sprintf("Invalid input for 'type'.
                         Please try one of the following options: %s", sp.tps))
  } else {

    ### Filter time
    if(type == 'z_standard'){
      ret <- sapply(trial.inds, function(trial){

        baseline.mean <- mean(baseline.df[,trial])
        baseline.sd <- sd(baseline.df[,trial])

        z_score(xvals = dataframe[,trial],
                mu = baseline.mean,
                sigma = baseline.sd,
                z.type = 'standard')
      })

      ret <- as.data.frame(cbind(dataframe[,1], ret))
      colnames(ret) <- colnames(dataframe[c(1, trial.inds)])

    }

    if(type == 'z_modified'){
      ret <- sapply(trial.inds, function(trial){

        baseline.median <- median(baseline.df[,trial])
        baseline.mad <- mad(baseline.df[,trial])

        z_score(xvals = dataframe[,trial],
                mu = baseline.median,
                sigma = baseline.mad,
                z.type = 'modified')
      })

      ret <- as.data.frame(cbind(dataframe[,1], ret))
      colnames(ret) <- colnames(dataframe[c(1, trial.inds)])

    }

    if(type == 'percent_change'){
      ret <- sapply(trial.inds, function(trial){

        baseline.val <- mean(dataframe[dataframe$Time <= 0, trial])

        percent_change(xvals = dataframe[,trial],
                         base.val = baseline.val)
      })

      ret <- as.data.frame(cbind(dataframe[,1], ret))
      colnames(ret) <- colnames(dataframe[c(1, trial.inds)])

    }
  }

  return(ret)
}
