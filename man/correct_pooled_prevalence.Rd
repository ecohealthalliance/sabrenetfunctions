% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{correct_pooled_prevalence}
\alias{correct_pooled_prevalence}
\title{Post-fitting correction for prevalence of pooled samples}
\usage{
correct_pooled_prevalence(num_pos_pools, num_pools, pool_size)
}
\arguments{
\item{num_pos_pools}{Number of positive pools}

\item{num_pools}{Number of pools}

\item{pool_size}{Number of samples per pool}
}
\value{
Adjusted positivity controlling for the pooling
}
\description{
Post-fitting correction for prevalence of pooled samples
}
