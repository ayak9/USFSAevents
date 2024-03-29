% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compsumQual.R
\name{compsumQual}
\alias{compsumQual}
\title{Calculates Frequency & Percentage for the Qualitative variable for one Event}
\usage{
compsumQual(dat)
}
\arguments{
\item{dat}{data frame of summary statistics of the qualitative variable (deductions) for a single event}
}
\value{
data frame of summary statistics of deductions for a single event
}
\description{
Calculates Frequency & Percentage for the Qualitative variable for one Event
}
\examples{
Comp1 = anycomp('https://ijs.usfigureskating.org/leaderboard/results/2018/26192/SEGM036.html')
compsumQual(Comp1)
}
