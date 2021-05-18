#'Validation of the tree for preference rankings
#'
#'Validation of the tree either with a test set procedure or with v-fold cross validation
#'
#' @param Tree An object of the class "ranktree" coming form te function \code{ranktree}
#' @param testX The data frame containing the test set (predictors)
#' @param testY The matrix ontaining the test set (response)
#' @param method One between "test" (default) or "cv"
#' @param V The cross-validation parameter. Default V=5
#' @param plotting With the defaul option plotting=TRUE, the pruning sequence plot is visualized
#' 
#' 
#' @return A list containing:
#' \tabular{lll}{
#' tau \tab \tab the Tau_x rank correlation coefficient of the sequence of the trees\cr
#' error \tab  \tab the error of the sequence of the trees\cr
#' termnodes\tab \tab the number of terminal nodes of the sequence of the trees\cr
#' best_tau \tab \tab the best tree in terms of Tau_x rank correlation coefficient\cr
#' best_error \tab \tab the best tree in terms of error (it is the same)\cr
#' validation \tab \tab information about the validation procedure}#' 
#' 
#' 
#' @examples
#' \donttest{
#' data(EVS)
#' EVS$rankings[is.na(EVS$rankings)] <- 3
#' set.seed(654)
#' training=sample(1911,1434)
#' tree <- ranktree(EVS$rankings[training,],EVS$predictors[training,],decrmin=0.001,num=50)
#' #test set validation
#' vtreetest <- validatetree(tree,testX=EVS$predictors[-training,],EVS$rankings[-training,]) 
#' #cross-validation
#' vtreecv <- validatetree(tree,method="cv",V=10) 
#' }
#' 
#' @author Antonio D'Ambrosio \email{antdambr@unina.it}
#' 
#' @importFrom janitor compare_df_cols
#' 
#' @export



validatetree <- function(Tree,testX=NULL,testY=NULL,method="test",V=5,plotting=TRUE){
  
  #Tree: the tree as returned by ranktree
  #testX: test sample 
  #method: one among "testset" or "cv"
  
  nitems <- ncol(Tree$Y)
  if (method=="test"){#test set 
    
    if (is(testX,"NULL")){stop("The test set must be included")}
    if (is(testY,"NULL")){stop("The test set must be included")}
    checktest <- compare_df_cols(Tree$X,testX)
    if (isFALSE(identical(checktest[[2]],checktest[[3]]))){stop("The test set in not conform to the training set")}
    if (isFALSE(identical(colnames(testY),colnames(Tree$Y)))){stop("The items in the ranking matrices must be equal and in the same order")}
    
    treeseq <- length(Tree$subtrees)
    taos <- errors <- rep(0,treeseq)  
    
    for (j in 1:length(taos)){
      yfit <- predict.ranktree(Tree$subtrees[[j]],testX)$rankings
      taos[j] <- mean(diag(tau_x(testY,yfit)))

      
      
      errors[j] <- mean(diag(kemenyd(testY,yfit)))

      
    }
    

    
    best <- which.max(taos)
    beste <- which.min(errors)
    validation="test set"
    
    validate=list()
    validate$tau <- taos
    validate$error <- errors/(nitems*(nitems-1)) #normalized error
    validate$termnodes <- Tree$pruneinfo$termnodes
    validate$best_tau <- Tree$pruneinfo$termnodes[best]
    validate$best_error <- Tree$pruneinfo$termnodes[beste]
    validate$validation <- validation

    
    
  }else if (method=="cv"){ #end test set, begin CV
    
    treeseq <- length(Tree$subtrees)
    taos <- errors <- rep(0,treeseq) 
    
    #get alpha and geometric means of the alpha boundary points
    alpha <- Tree$alpha
    avalpha <- c(sqrt(alpha[1:(length(alpha)-1)]*alpha[2:length(alpha)]),Inf)
    
    N <- nrow(Tree$X)
    
    cvid <- 1+(seq(1:N)%%V)
    
    cvid <- cvid[sample(N)]
    
    fitcv <- list( matrix(0,nrow=nrow(Tree$X),ncol=ncol(Tree$Y)) ) 
    
    fitcv <- rep(fitcv,length(alpha))
    
    for (j in 1:V){
      
      idtest <- which(cvid==j)
      idtrain <- which(cvid!=j)
      controlcv <- Tree$control
      #controlcv$printinfo <- FALSE
      cvtree <- ranktree(Tree$Y[idtrain,],Tree$X[idtrain,],control=controlcv)
      
      nsubtree <- findcvtree(cvtree,avalpha)
      
      level <- sort(cvtree$pruneinfo$termnodes[nsubtree])
      
      #sapply(cvtree$subtrees,function(x) unlist(x$numnodes))
      
      
      for (p in 1:length(nsubtree)){
        
        yfit <- predict.ranktree( cvtree$subtrees[[ nsubtree[p] ]], Tree$X[idtest,])
        
        fitcv[[p]][idtest,] <- yfit$rankings
        
      }
      
      
    }
    
    
    taos <- sapply(fitcv,function(x) mean(diag(tau_x(x,Tree$Y))))
    
    errors <- sapply(fitcv,function(x) mean(diag(kemenyd(x,Tree$Y))))
    
    best <- which.max(taos)
    beste <- which.min(errors)

    
    validation="cross validation"
    
    validate=list()
    validate$tau <- taos
    validate$error <- errors/(nitems*(nitems-1)) #normalized error
    validate$termnodes <- Tree$pruneinfo$termnodes
    validate$best_tau <- Tree$pruneinfo$termnodes[best]
    validate$best_error <- Tree$pruneinfo$termnodes[beste]

    validate$validation <- validation

    
    
    
    
  }#end cv
  
  if( isTRUE(plotting)) {
    
    plot.ranktree(Tree, plot.type="pruningseq",valtree=validate)
    
  }
  
  
  return(validate)
  
}#end function

#-----------------

findcvtree <-function(Tree,alpha0){
  
  adj <- 1+100*( 2^(-52) )
  alphas <- Tree$alpha
  k <- rep(0,length(alpha0))
  
  for (j in 1:length(alpha0)){
    
    k[j] <- sum(alphas <= alpha0[j]*adj)
    
  }
  
  return(k)
  
}

