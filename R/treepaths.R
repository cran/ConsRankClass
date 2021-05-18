#'Path of a terminal node
#'
#'Given an object of the class "ranktree", it extracts the paths of all terminal nodes
#'
#' @param Tree An object of the class "ranktree" 
#' 
#' @return A list containing:
#' \tabular{lll}{
#' leaves\tab \tab the number of the terminal nodes\cr
#' size\tab  \tab the sample size within each terminal nodes\cr
#' paths\tab \tab a list containing all the paths}
#' 
#' @examples
#' \donttest{
#' data(Irish)
#' #build the tree with default options
#' tree <- ranktree(Irish$rankings,Irish$predictors)
#' #get information about all the paths leading to terminal nodes
#' paths <- treepaths(tree)
#' #
#' #the terminal nodes
#' paths$leaves
#' #
#' #sample size within each terminal node
#' paths$size
#' #
#' #visualize the path of the second leave (terminal node number 8)
#' paths$paths[[2]]
#' }
#' 
#' 
#' @author Antonio D'Ambrosio \email{antdambr@unina.it}
#' 
#' 
#' @seealso \code{\link{ranktree}}, \code{\link{nodepath}}, \code{\link{getsubtree}}
#' 
#' 
#' @export
#' 


treepaths <-function(Tree){
  
  isleaf <- unlist(Tree$node$terminal)
  idleaf <- which(isleaf==TRUE)
  idnumleaf <- unlist(Tree$node$number[idleaf])
  numidnode <- paste('node',paste(idleaf),sep='')  #id of node
  numnode <- paste('node',paste(idnumleaf),sep='')  #number of node
  sizeleaf <- unlist(Tree$node$size[isleaf])
  summp <- lapply(idnumleaf, nodepath, Tree)
  names(summp) <- numnode
  
  return(list(leaves=idnumleaf,size=sizeleaf,paths=summp))
  
  
}

