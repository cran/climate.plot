\name{image.seas.sum}
\alias{image.seas.sum}
\title{Show a seasonal sum data object}
\description{
  Graphicly dispay a seasonal sum object.
}
\usage{
image.seas.sum(x, start, end, param, norm = "days", maxz, nlevels = 128, maxa,
  col = .65, dark = 0, gamma = 0.8, lwd = 2, sqrt = FALSE, 
  show.na = TRUE, show.median = TRUE, contour = FALSE, ...)
}
\arguments{
  \item{x}{a \code{\link{seas.sum}} object}
  \item{start}{start year; if omitted minimum year will be used}
  \item{end}{end year; if omitted will use same as \code{start}, and if \code{start} is omitted, will use maximum year}
  \item{param}{the desired parameter show, otherwise will use the prime paremeter, defined in \code{x}}
  \item{norm}{parameter to normilize by, usually \code{"days"}, to produce \emph{unit/day}}
  \item{maxz}{maximum value to be displayed}
  \item{nlevels}{number of colour levels}
  \item{maxa}{maximum for annual axis, on right-hand graphs (if \code{show.median=TRUE})}
  \item{col}{colour or hue, between [0,1]}
  \item{dark}{darkness of colour}
  \item{gamma}{gamma correction for colour}
  \item{lwd}{line-thickness for median/mean lines (if \code{show.median=TRUE})}
  \item{sqrt}{\code{logical}; square root of the values to be taken (legend is transposed back to original unit)}
  \item{show.na}{\code{logical}; put red a \sQuote{x} where data are \code{NA}}
  \item{show.median}{\code{logical}; show how the median calculation is acheived graphically (computationally it is done using a secant method); see \code{\link{seas.norm}} for more information on this method}
  \item{contour}{\code{logical}; show contours in lower left-hand plot}
  \item{...}{other arguments passed to \code{\link{.seastitle}} for title style, and other customizations to the appearance}
}
\details{
  This is a graphical representation of a \code{\link{seas.sum}} object, and is far more infomative than a traditional precipitation \dQuote{normal} (i.e., \code{\link{precip.norm}} or \code{\link{precip.norm}})
}
\author{M.W. Toews}
\seealso{\code{\link{seas.sum}}, \code{\link{seas.norm}}}
\examples{
data(mscdata)

dat.ss <- seas.sum(mscdata, id=1108447, width="mon")
image(dat.ss)
}
\keyword{hplot}
