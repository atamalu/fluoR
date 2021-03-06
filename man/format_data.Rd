% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_data.R
\name{format_data}
\alias{format_data}
\title{Format dataframe for use with fluoR}
\usage{
format_data(dataframe)
}
\arguments{
\item{dataframe}{a dataframe with timestamps in the first row or column
and trial values in subsequent ones}
}
\value{
a dataframe of timestamps in the first column and trial values in
subsequent ones
}
\description{
`format_data` automatically formats dataframe for use with fluoR
}
\examples{
### Coerce input matrix to fluoR-format dataframe
df <- format_data(GCaMP)

}
