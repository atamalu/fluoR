% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/z_score.R
\name{z_score}
\alias{z_score}
\title{Transform values into z scores}
\usage{
z_score(xvals, mu = NULL, sigma = NULL, z.type = "standard")
}
\arguments{
\item{xvals}{a vector of values to be transformed into z scores}

\item{mu}{a specified value for the input vector to be centered at}

\item{sigma}{a specified value for the input vector to be scaled by}

\item{z.type}{the type of summary statistic being used for mu}
}
\value{
the input vector transformed into z scores
}
\description{
`z_score` transforms a vector of values into standard z scores
or modified z scores
}
\examples{
df <- format_data(GCaMP)

z_score(xvals = df$Trial1)
}
