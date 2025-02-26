% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcALK.R
\name{simModelFisher}
\alias{simModelFisher}
\title{simModelFisher}
\usage{
simModelFisher(species, quarter, rep, fit, sim, i)
}
\arguments{
\item{species}{The species of interest.}

\item{quarter}{The quarter of the year which the ALKs are calculated.}

\item{RFA}{Roundfish area number.}

\item{year}{The year which the ALKs are calculated.}

\item{data}{The CA needed for calculating the ALKs.}

\item{data_hl}{The HL needed for calculating the ALKs (since there can be trawl hauls without age information).}
}
\value{
Returns a list with simulated model based ALK for each trawl haul
}
\description{
simModelFisher
}
