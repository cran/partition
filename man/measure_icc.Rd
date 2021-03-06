% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{measure_icc}
\alias{measure_icc}
\title{Measure the information loss of reduction using intraclass correlation
coefficient}
\usage{
measure_icc(.partition_step)
}
\arguments{
\item{.partition_step}{a \code{partition_step} object}
}
\value{
a \code{partition_step} object
}
\description{
Metrics are functions that tell how much information would be
lost for a given reduction in the data. reduce.  \code{\link[=as_measure]{as_measure()}} is a
helper function to create new metrics to be used in \code{partitioner}s.
\code{partitioner}s can be created with \code{\link[=as_partitioner]{as_partitioner()}}.

\code{measure_icc()} assesses information loss by calculating the
intraclass correlation coefficient for the target variables.
}
\seealso{
Other metrics: 
\code{\link{as_measure}()},
\code{\link{measure_min_icc}()},
\code{\link{measure_min_r2}()},
\code{\link{measure_std_mutualinfo}()},
\code{\link{measure_variance_explained}()}
}
\concept{metrics}
