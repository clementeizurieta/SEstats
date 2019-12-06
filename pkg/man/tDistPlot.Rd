\name{tDistPlot}
\alias{tDistPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot of t-distribution, C.I., and t-value
}
\description{
Plots the specified Student's t-distribution, shows the confidence band, and where the specified t-value is along the distribution.
}
\usage{
tDistPlot(tvalue, df, alpha = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tvalue}{numerical t-value or vector of multiple t-values}
  \item{df}{degrees of freedom for Student's t distribution}
  \item{alpha}{confidence level}
  \item{...}{other arguement to plot()}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
S. Katie Fogg
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
tDistPlot(tvalue = 0.44, df = 9, alpha = 0.05)
tDistPlot(tvalue = 4.6, df = 20, alpha = 0.01)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
