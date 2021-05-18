#'S3 methods for ranktree
#'
#'Print methods for objects of class \code{ranktree}
#'
#'@param x An object of the class "ranktree"
#'@param \dots not used
#'
#'@return print a brief summary of the prediction tree
#'
#'@examples
#'
#'data("Univranks")
#'tree <- ranktree(Univranks$rankings,Univranks$predictors,num=50)
#'tree
#'
#'
#'
#'@export


print.ranktree <- function(x,...){
  
  Tree <- x
  
  isleaf <- unlist(Tree$node$terminal)
  obj <- "\nRecursive partitioning method for preference rankings: \n"
  cat(obj)
  cat("Number of items in the rank matrix: ",ncol(Tree$Y), "\n")
  cat("Number of predictors: ", ncol(Tree$X), "\n")
  cat("Algorithm used to compute median rankings: ", Tree$control$algorithm, "\n")
  cat("\n")
  
  cat("Number of terminal nodes: ",(Tree$numnodes+1)/2,"\n")
  cat("Averaged tau_X rank correlation coefficient: ", Tree$goodness$tau, "\n")
  cat("Error (normalized Kemeny distance): ", Tree$goodness$error, "\n")
  cat("Variables generating splits: ", paste(unique(unlist(Tree$node$varsplit[!isleaf])),collapse=', '), "\n")
  cat("Leaves number:",paste(unlist(Tree$node$number[isleaf]), collapse=', '))
  cat("\n")
  
}