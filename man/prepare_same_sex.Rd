% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_datasets.R
\name{prepare_same_sex}
\alias{prepare_same_sex}
\title{Prepare the data for the same sex interactions}
\usage{
prepare_same_sex(DF1)
}
\arguments{
\item{DF1}{The interactions database (d1)}
}
\value{
a filtered data frames
}
\description{
Keeps the interaction between members of the same sex,
selects the variables of interest, renames the type of
interaction and creates a binomial version of the social support and
delta weight variables.
}
\examples{
data(d1)
data_same_sex <- prepare_same_sex(d1)
}
