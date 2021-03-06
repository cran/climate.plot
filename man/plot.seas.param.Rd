\name{plot.seas.param}
\alias{plot.seas.param}
\title{Plot seasonal normals of a given parameter}
\description{
  Plot seasonal normals using boxplots of a given parameter.
}
\synopsis{
plot.seas.param(x, start, end, width=11, param,
  col = "light grey", id, ylab, ylim, add.alt, ...)
}
\usage{
plot.seas.param(x, param)

plot.seas.param(x, start, end, width=11, param, 
  col = "light grey", id, ylab, ylim, add.alt, ...)
}
\arguments{
  \item{x}{a \code{data.frame}}
  \item{start}{start year; if omitted minimum year will be used}
  \item{end}{end year; if omitted will use same as \code{start}, and if \code{start} is omitted, will use maximum year}
  \item{width}{size of bin; see \code{\link{mkfact}}}
  \item{param}{a parameter; a column name in \code{x}}
  \item{col}{colour for the boxplots; default is \code{"light grey"}}
  \item{id}{unique station identifier used to extract a subset of data from \code{x}}
  \item{ylab}{label for the y-axis; if there is an alternative y-axis, this can have a length of two for the second label}
  \item{ylim}{\code{c(min,max)} range for y-axis}
  \item{add.alt}{this adds an alternative axis, and is specified by \code{c(slope,inter)}; for example, if the primary measure is in degrees C, a secondary scale in K would be \code{c(1, 273.15)}, or in degrees F would be \code{c(5/9, 32)}}
  \item{...}{other arguments passed to \code{\link{.seastitle}} for title style, and other customizations to the appearance}
}
\details{
  Shows normals of a seasonally varying parameter using boxplots.
}
\value{
  Returns values from \code{\link{boxplot}} statistics on the parameter.
}
\author{M.W. Toews}
\seealso{\code{\link{plot.seas.norm}}, \code{\link{plot.year}}}
\examples{
data(mscdata)

plot.seas.param(mscdata, param="max_t", col="tomato", id=1108447,
  add.alt=c(5/9,32), ylab=c("Maximum temperature, C","F"))
abline(h=0)

plot.seas.param(subset(mscdata,precip>0), param="precip",
  col="azure", id=1108447)
}
\keyword{hplot}
