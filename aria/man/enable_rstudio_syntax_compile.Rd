% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enable_rstudio_syntax_compile.R
\name{enable_rstudio_syntax_compile}
\alias{enable_rstudio_syntax_compile}
\title{Enable enhanced syntax check (inc. compile) in RStudio}
\usage{
enable_rstudio_syntax_compile()
}
\value{
NULL (invisibly)
}
\description{
This function (called when \code{aria} is loaded) overrides \code{\link{rstan::rstudio_stanc}} with \code{\link{aria::check_syntax_and_maybe_compile}} thus enhancing Rstudio's "source on save" feature. To undo this override, use \code{\link{aria::disable_rstudio_syntax_compile}}
}
\examples{
\dontrun{
enable_rstudio_syntax_compile()
}
}
\seealso{
Other Model checking & compilation functions: 
\code{\link{check_and_compile}()},
\code{\link{disable_rstudio_syntax_compile}()}
}
\concept{Model checking & compilation functions}
