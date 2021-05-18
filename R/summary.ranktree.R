#'S3 methods for ranktree
#'
#'Summary methods for objects of class \code{ranktree}
#'
#'@param object An object of the class "ranktree"
#'@param \dots not used
#'
#'@return it shows the summary of the prediction tree
#'
#'@examples
#'
#' data("Univranks")
#'tree <- ranktree(Univranks$rankings,Univranks$predictors,num=50)
#'summary(tree)
#'
#'@export
#'

summary.ranktree <- function(object,...){
  #object is of the class "ranktree"
  Tree <- object
  isleaf <- unlist(Tree$node$terminal)
  forsum <- treepaths(Tree)
  ll <- length(forsum$paths)

  obj <- "\nRecursive partitioning method for preference rankings: \n"
  cat(obj)
  cat("Number of terminal nodes: ",length(forsum$leaves),"\n")
  cat("Averaged tau_X rank correlation coefficient: ", Tree$goodness$tau, "\n")
  cat("Error (normalized Kemeny distance): ", Tree$goodness$error, "\n")
  cat("Variables generating splits: ", paste(unique(unlist(Tree$node$varsplit[!isleaf])),collapse=', '), "\n\n")

  cat(forsum$paths[[1]], "\n\n")
  for (j in 2:ll){
    cat(forsum$paths[[j]], "\n\n")
  }

}
