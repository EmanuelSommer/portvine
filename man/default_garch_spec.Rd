% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default_garch_spec.R
\name{default_garch_spec}
\alias{default_garch_spec}
\title{Default specifications for ARMA-GARCH models}
\usage{
default_garch_spec(ar = 1, ma = 1, arch = 1, garch = 1, dist = "sstd")
}
\arguments{
\item{ar}{integer for the autoregressive order}

\item{ma}{integer for the moving average order}

\item{arch}{integer for the ARCH order}

\item{garch}{integer for the GARCH order}

\item{dist}{a single character value of the possible distributions allowed in
\code{\link[rugarch:ugarchspec-methods]{rugarch::ugarchspec}}}
}
\value{
object of class \code{\link[rugarch:ugarchspec-methods]{rugarch::ugarchspec}}
}
\description{
This function is used as the default for the univariate model fitting i.e.
the marginal models and can be used to easily specify a different
individual marginal model specification or default in
\code{\link[=marginal_settings]{marginal_settings()}}.
The ARMA(\code{ar},\code{ma})-GARCH(\code{arch},\code{garch}) is fitted  with the distribution
given by \code{dist} that specifies the conditional density used for the
innovations.
}
\examples{
# the default is then just using
default_garch_spec()
# to specify a ARMA(2,2)-GARCH(1,1) model with normal residual distribution
default_garch_spec(ar = 2, ma = 2, dist = "norm")
}
\seealso{
\code{\link[=marginal_settings]{marginal_settings()}}
}
