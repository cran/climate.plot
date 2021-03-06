\name{plot.interarrival}
\alias{plot.interarrival}
\title{Plot interarrivals for precipitation}
\description{
  Plots interarrivals for precipitation using boxplots, giving the typical number of continuous wet days and dry days (or spells) throughout the season.
}
\usage{
\method{plot}{interarrival}(x, start, end, width = 11, logy=FALSE, maxy, id, \ldots)
}
\arguments{
  \item{x}{an \code{\link{interarrival}} object with number of dry/wet days}
  \item{start}{start year; if omitted minimum year will be used}
  \item{end}{end year; if omitted will use same as \code{start}, and if \code{start} is omitted, will use maximum year}
  \item{width}{size of bin; see \code{\link{mkfact}}}
  \item{logy}{\code{logical}; y-axis is logarithmic}
  \item{maxy}{maximum number of days for both graphs on the y-axis}
  \item{id}{unique station identifier used to extract a subset of data from \code{x}}
  \item{...}{other arguments passed to \code{\link{.seastitle}} for title style, and other customizations to the appearance}
}
\author{M.W. Toews}
\seealso{\code{\link{interarrival}}}
\examples{
data(mscdata)

dat.int <- interarrival(mscdata,id=1108447)
plot(dat.int)
}
\keyword{hplot}
