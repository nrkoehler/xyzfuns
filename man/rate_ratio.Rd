% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{rate_ratio}
\alias{rate_ratio}
\title{{Estimate rate ratio}}
\source{
{\url{https://mirror.linux.duke.edu/cran/web/packages/fmsb/index.html}}
}
\usage{
rate_ratio(a, b, time.a, time.b, conf.level = 0.95)
}
\arguments{
\item{a}{The number of events in group a.}

\item{b}{The number of events in group b.}

\item{time.a}{Time under risk of group a.}

\item{time.b}{Time under risk of group b.}

\item{conf.level}{Confidence level (default = 0.95)}
}
\value{
The function returns a data.frame with four variables:
\itemize{
 \item \code{EST} {Point estimate}
 \item \code{CIL} {Confidence interval, lower bound}
 \item \code{CIU} {Confidence interval, upper bound}
 \item \code{PVAL} {p-Value}
}
}
\description{
{Estimate rate ratio along with confidence interval
and p-value.}
}
\details{
{Tidy version of Minato Nakazawa's fmsb::rateratio() function.}
}
