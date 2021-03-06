% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{get_predictions}
\alias{get_predictions}
\title{Predictions}
\usage{
get_predictions(model)
}
\arguments{
\item{model}{the model for which we want the predictions}
}
\value{
a DF with prediction and 95 confidence interval
}
\description{
Gets the predictions of the models (no newdata needed). It uses the
spaMM::predict method which compute, in addition to the fitted value
the 95 Wald CI.
}
\examples{
data(mod_sex_null_diff_PQL)
get_predictions(mod_sex_null_diff_PQL)

}
