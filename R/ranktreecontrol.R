#' Utility function
#'
#' Utility function to use to set the control arguments of \code{ranktree}
#'
#' @param num The maximum number of observations in a node to be split: default, 10\% of the sample size
#' @param decrmin Minimum decrease in impurity
#' @param algorithm The algorithm used to compute the median ranking. One among"BB", "quick" (default), "fast" and "decor"
#' @param full Specifies if the median ranking must be searched in the universe of rankings including all the possible ties. Default: FALSE
#' @param itermax Number of iterations for "fast" and "decor" algorithms. itermax=10 is the default option.  
#' @param np (for "decor" only) the number of population individuals. np=15 is the default option.
#' @param gl (for"decor" only) generations limit, maximum number of consecutive generations without improvement. gl=100 is the default option.
#' @param ff (for"decor" only) the scaling rate for mutation. Must be in [0,1]. ff=0.4 is the default option.
#' @param cr (for"decor" only) the crossover range. Must be in [0,1]. cr=0.9 is the default option.
#' @param proc (for "BB" only) proc=TRUE allows the branch and bound algorithm to work in difficult cases, i.e. when the number of objects is larger than 15 or 25. proc=FALSE is the default option
#' @param ps If PS=TRUE, on the screen some information about how many branches are processed are displayed. Default value: FALSE
#' 
#' 
#' @return A list containing all the control parameters
#' 
#' 
#' @author Antonio D'Ambrosio \email{antdambr@unina.it}
#' 
#' @seealso  \code{\link{ranktree}} 
#' 
#' @export


ranktreecontrol <- function(num=NULL, decrmin=0.01, algorithm="quick",full=FALSE,
                             itermax=10,np=15,gl=100,ff=0.4,cr=0.9,proc=FALSE,
                             ps=FALSE)
  
{
  
  return(list(num=num,decrmin=decrmin, algorithm=algorithm, full=full, itermax=itermax, np=np, gl=gl, ff=ff,
              cr=cr, proc=proc, ps=ps))
}
