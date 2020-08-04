#' @title Plot range of trials
#'
#' @description `plot_trials` plots trials specified by the user
#'
#' @importFrom graphics legend lines par plot
#' @param dataframe a fluoR-format dataframe
#' @param trials trial numbers to be plotted
#' @param time.range range of time to be plotted; default keeps full range
#' @return a line graph of user-specified trials
#' @examples
#' ### Format data frame
#' df <- format_data(GCaMP)
#'
#' ### Plot 2 seconds before and 2 seconds after event onset for trials 1-4
#' my.trials <- c(1, 2, 3, 4)
#' start.stop <- c(-2, 2)
#'
#' plot_trials(dataframe = df, trials = my.trials, time.range = start.stop)
#' @export

plot_trials <- function(dataframe, trials, time.range = NULL){

  if(!is.null(time.range)){
    df <- dataframe[dataframe[,1] >= time.range[[1]] &
                    dataframe[,1] <= time.range[[2]],]
  } else {
    df = dataframe
  }

  trial.inds <- trials + 1
  num.trials <- length(trials)

  # legend settings
  num.leg.cols <- ifelse(length(trials) <= 2, 1,
                         ifelse(length(trials) >= 3 &
                                  length(trials) <= 4, 2, 3))

  leg.labels <- c()
  leg.colors <- c()

  # min/max x/y values
  x.min <- min(df[,1])
  x.max <- max(df[,1])

  y.min <- min(apply(df[,trial.inds], 2, min))
  y.min <- y.min - (0.01 * abs(y.min))

  y.max <- max(apply(df[,trial.inds], 2, max)) + 1
  y.max <- y.max + (0.03 * abs(y.max))

  # better palette
  color.vector <- c('chartreuse3', 'cornflowerblue', 'darkgoldenrod1',
                    'peachpuff3','mediumorchid2', 'turquoise3',
                    'wheat4', 'slategray2')

  # default plot to fit all lines
  #plot.new()
  par(xpd = TRUE)
  plot(x = df[,1],
       y = seq(from = round(y.min, 0),
               to = round(y.max, 0),
               length.out = length(df[,1])),
       xlim = c(x.min, x.max), ylim = c(y.min, y.max),
       type = 'n',
       xlab = 'Time', ylab = 'Value')

  # draw lines
  for(i in 1:num.trials){
    trial.ind <- trial.inds[[i]]
    color <- color.vector[[i]]

    lines(x = df[,1],
          y = df[,trial.ind],
          col = color)
    leg.labels <- append(leg.labels, paste0("Trial ", trials[[i]]))
    leg.colors <- append(leg.colors, color)
  }

  # add legends
  legend('topright', legend = leg.labels,
         col = leg.colors, lty=1, cex=0.75,
         bty = 'o', box.lwd = .001,
         ncol = num.leg.cols,
         text.font=2)

}
