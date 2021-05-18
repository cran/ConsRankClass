#'S3 methods for cca
#'
#'Print methods for objects of class \code{cca}
#'
#'@param x An object of the class "cca"
#'@param \dots not used
#'
#'@return print a brief summary of the CCA
#'
#'
#'@export


print.cca <- function(x,...){
  
  rescca<-x
  
  obj <- "\nK-Median Cluster Component Analysis: \n"
  cat(obj)
  cat("Components extracted: ",ncol(rescca$pk), ".\n\n", sep='')
  cat("Cluster centers: ", "\n")
  cat(rescca$oclc[1,], "\n")
  for (j in 2:nrow(rescca$clc)){
     cat(rescca$oclc[j,], "\n")
  }
  cat("\n")
  cat("Global homogeneity: ", format(rescca$Hcca,digits=3), ".\n", sep='')
  cat("Estimated proportions: ", paste(format(c(rescca$props), digits=3), collapse=';'), ".\n", sep='')
  cat("Internal homogeneity: ", paste(format(c(rescca$hk), digits=3), collapse=';'), ".\n\n", sep='')
  cat("Overall classifiability (normalized geometric mean): ", format(rescca$Ucca, digits=3), ".\n", sep='')
  cat("Overall classifiability (normalized probabilities prod): ", format(rescca$Uprodscca, digits=3), ".\n\n", sep='')
  cat("Algorithm used to compute median rankings: ", rescca$settings$algorithm, ".\n")
  cat("\n")
  
}