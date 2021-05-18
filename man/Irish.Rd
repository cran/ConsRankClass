\name{Irish}
\alias{Irish}
\docType{data}
\title{
Irish Election data set
}
\description{
An opinion poll conducted by Irish Marketing Surveys one month prior to the election in 1997. Interviews were conducted on about 1100 respondents, drawn from 100 sampling areas. Interviews took place at randomly located homes, with respondents selected according to a socioeconomic quota. A range of sociological questions was asked of each respondent, as was their voting preference, if any, for each of the candidates.
}
\usage{data("Irish")}
\format{
  The format is:
List of 3

 $ IrishElection: 'data.frame':	1083 obs. of  11 variables:
Gender (male, housewife, nonhousewife),  marital status (single, married, separated), age, socialclass (five unordered categories), Area (rural, city, town), government satisfaction (no opinion,m satisfied, dissatisfied), Bano , Roch, McAl, Nall, Scal

 $ predictors   :'data.frame' with all the predictors
 
 $ rankings     : matrix with the preferencres for "Bano" "Roch" "McAl" "Nall" 
}
\details{
In the original version of the data, the ranking matrix contains NAs. Here, NAs are replaced with the number 7, to indicate that all the non-stated preferences are in a tie at the last position (see D'Ambrosio and Heiser, 2016). For details about the data set see Gormley and Murphy, 2008.
}
\source{
https://projecteuclid.org/journals/annals-of-applied-statistics/volume-2/issue-4/A-mixture-of-experts-model-for-rank-data-with/10.1214/08-AOAS178.full?tab=ArticleLinkSupplemental
}
\references{
Gormley, I.C., and Murphy, T.B. (2008). A mixture of experts model for rank data withapplications in election studies. Annals of Applied Statistics 2(4): 1452-1477. DOI: 10.1214/08-AOAS178

D'Ambrosio, A., and Heiser W.J. (2016). A recursive partitioning method for the prediction of preference rankings based upon Kemeny distances. Psychometrika, vol. 81 (3), pp.774-94. DOI: 10.1007/s11336-016-9505-1.
}
\examples{
data(Irish)
}
\keyword{datasets}
