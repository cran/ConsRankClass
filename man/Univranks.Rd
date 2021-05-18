\name{Univranks}
\alias{Univranks}
\docType{data}
\title{
University rankings dataset.
}
\description{
University rankings dataset was analysed by Dittrich, Hatzinger and Katzenbeisser (1998) to investigate paired comparison data concerning European universities and student's characteristics with the goal to show that university rankings are different for different groups of students. Here both raw data (with paired comparisons) and the version with rankings are preesented (see details). A survey of 303 students studying at the Vienna University of Economics was carried out to examine the student's preference of six universities, namely London, Paris, Milan, St. Gallen, Barcelona and Stockholm. The data set contains 23 variables. The first 15 digits in each row indicate the preferences of a student. For a given comparison, responses were coded by 1 if the first preference was preferred, by 2 if the second university was preferred, and by 3 if universities are tied. All rows containing missing ranked Universities were skipped.
}
\usage{data("Univranks")}
\format{
  The format is:
List of 3

 $ rawdata: 'data.frame':	212 obs. of  23 variables: the first 15 are the paired comparisons coded as follows: (1: the first is preferred to the second; 2: the second is preferred to the fisrt; 3 tied)
 
   $ LP  : comparison of London to Paris 
  
   $ LM  : comparison of London to Milan
  
   $ PM  : comparison of London to Milan
  
   $ LSg : comparison of London to St. Gallen
   
   $ PSg : comparison of Paris to St. Gallen
   
   $ MSg : comparison of  Milan to St. Gallen
   
   $ LB  : comparison of London to Barcelona
   
   $ PB  : comparison of Paris to Barcelona
   
   $ MB  : comparison of Milan to Barcelona
   
   $ SgB : comparison of St. Gallen to Barcelona
   
   $ LSt : comparison of London to Stockholm
   
   $ PSt : comparison of Paris to Stockholm
   
   $ MSt : comparison of Milan to Stockholm
   
   $ SgSt: comparison of St. Gallen to Stockholm
   
   $ BSt : comparison of Barcelona to Stockholm
   
   $ Stud: Factor w/ 2 levels "commerce","other" 
   
   $ Eng : Factor w/ 2 levels "good","poor""
   
   $ Fra : Factor w/ 2 levels "good","poor"
   
   $ Spa : Factor w/ 2 levels "good","poor"
   
   $ Ita : Factor w/ 2 levels "good","poor"
   
   $ Wor : Factor w/ 2 levels "no","yes"
   
   $ Deg : Factor w/ 2 levels "no","yes"
   
   $ Sex : Factor w/ 2 levels "female","male"
  
 $ predictors:'data.frame':	212 obs. of  8 variables( the last 8 variables of the "rawdata" dataframe

 $ rankings  : matrix of preference rankings. The columns are: "L" (London), "P" (Paris), "M" (Milan), "Sg" (St. Gallen), "B" (Barcerlona), "St" (Stockholm)
 
}
\details{
To obtain the preference rankings from the paired comparisons the procedure has been the following: the first row of the raw data is [1  3  2   1   2   1  1  2  1   1   1   2   1    1   2]. London is preferred to Paris, St. Gallen, Barcelona Stockholm (LP, LM, LSg, LB and LSt are always equal to 1), and there is no preference between London and Milan (they are tied); Milan is preferred to Paris (PM = 2), St. Gallen, Barcelona and Stockholm; and so on. The first ordering is then <{L M} Sg St B P> corresponding to a ranking [1,5,1,2,4,3], where the columns indicate L  P  M Sg  B St.  
}
\source{
http://www.blackwellpublishers.co.uk/rss
}
\references{
Dittrich, R., Hatzinger, R., and Katzenbeisser, W. (1998). Modelling the effect of subject-specific covariates in paired comparison studies with an application to university rankings. Journal of the Royal Statistical Society: Series C (Applied Statistics), 47(4), 511-525. DOI: 10.1111/1467-9876.00125

D'Ambrosio, A. (2008). Tree based methods for data editing and preference rankings. Ph.D. thesis, University of Naples Federico II. \url{https://www.doi.org/10.6092/UNINA/FEDOA/2746}
}
\examples{
data(Univranks)
}
\keyword{datasets}
