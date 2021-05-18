\name{EVS}
\alias{EVS}
\docType{data}
\title{
European Values Studies (EVS) data
}
\description{
Random sub-sample of 3584 cases of the survey conducted in 1999 in 32 countries analyzed by Vermunt (2003). 
}
\usage{data("EVS")}
\format{
  The format is:
List of 3

 $ data:'data.frame':	1911 obs. of  11 variables:
 
 country, gender ,yearbird, mstatus (marital status), eduage (age of education completion), employment (Employment status: ordinal scale 1-8), householdinc (Household income: ordinal scale 1-10), A (Maintain order in Nation),Give people more say in Government decisions, (C) Fight rising prices, (D) Protect freedom of speech. 
 
 
 $ predictors:'data.frame' with all the predictors
 
 $ rankings  : matrix with the preferencres for "A" (Maintain order in Nation),  "B" (Give people more say in Government decisions), "C" (Fight rising prices), "D" (Protect freedom of speech).

}
\details{
Rankings were obtained by applying the post-materialism scale developed by Inglehart (1977). The scale is based upon an experiment of the type “pick 2 out of 4” most important political goals for your Governments. For this reason, replace the 'NA's with 3 before using the rankings with codes 'ranktree' or 'cca' (see D'Ambrosio and Heiser, 2016). 
About the predictors, the coding of the Countries are: G1 (Austria, Denmark, Netherlands, Sweden), G2 (Belgium, Croatia, France, Greece, Ireland, Northern Ireland, Spain), G3 (Bulgaria, Czechnia, East, Germany, Finland, Iceland, Luxembourg, Malta, Portugal, Romania, Slovenia, West Germany), G4 (Belarus, Estonia, Hungary, Latvia, Lithuania, Poland, Russia, Slovakia, Ukraine).
Coding of predictor "mstatus" are: mar (married), wid (widowed),  div (divorced), sep (separated), nevm (never married).
}
\source{
http://statisticalinnovations.com/technicalsupport/choice_datasets.html
}
\references{
Vermunt, J. K. (2003). Multilevel latent class models. Sociological Methodology, 33(1), 213–239.

Inglehart, R. (1977). The silent revolution: Changing values and political styles among Western Publics. Princeton, NJ: Princeton University Press.

D'Ambrosio, A., and Heiser W.J. (2016). A recursive partitioning method for the prediction of preference rankings based upon Kemeny distances. Psychometrika, vol. 81 (3), pp.774-94.
}

\examples{
data(EVS)

# EVS$rankings[is.na(EVS$rankings)] <- 3 #place unranked objects in a tie to the third position
# ccares <- cca(EVS$rankings,4) #solution with 4 components

}
\keyword{datasets}

