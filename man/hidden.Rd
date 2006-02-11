\name{hidden}
\alias{.seasxlab}
\alias{.seastitle}
\title{Get labels for seasonal graphs}
\description{
  Get title and x-axis labels for seasonal graphs.
}
\usage{
.seasxlab(width)
.seastitle(main=NULL, id=NULL, name=NULL, orig=NULL, fun=NULL,
  range=NA, show.range=TRUE, show.id=TRUE, style=1)
}
\arguments{
  \item{width}{size of bin; see \code{\link{mkfact}}}
  \item{main}{main title of plot; overrides any other title, but appends year range if \code{show.range=TRUE}}
  \item{id}{station ID, which is used to fetch a station name using \code{\link{getstnname}}}
  \item{name}{a name, which is used in preference to the name retrieved from \code{getstnname}}
  \item{orig}{original object name, which is used if no other name can be found from \code{id} or \code{name}}
  \item{fun}{function, if applicable}
  \item{range}{year range in the format \code{c( start , end )} years}
  \item{show.range}{\code{logical}}
  \item{show.id}{\code{logical}}
  \item{style}{specific styles of presentation, currently only 1 and 2}
}
\details{
  These functions are called by the seasonal plot functions in \pkg{climate.plot} for consistent display of title, and x-axis labels. These functions are ready to have translations for non-English languages built into them.
}
\value{
  .seasxlab returns a \code{character} label.
  
  .seastitle returns a \code{list}.
}
\author{M.W. Toews}
\seealso{\code{\link{getstnname}}}
\examples{
.seasxlab(11)
.seasxlab("mon")

data(mscdata)
plot.seas.temp(mscdata, width="mon",  id=1108447, style=1)
plot.seas.temp(mscdata, width=365/12, id=1108447, style=2)
}
\keyword{utilities}
