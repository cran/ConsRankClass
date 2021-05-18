#' Predict the median rankings for new observations
#'
#' Predict the median rankings in a tree-based structure built with \code{ranktree} for new observations
#'
#' @param object An object of the class "ranktree"
#' @param newx A dataframe of the same nature of the predictor dataframe with which the tree has been built
#' @param \dots System reserved (No specific usage)
#'
#'
#' @return A list containing:
#' \tabular{lll}{
#' rankings \tab \tab the fit in terms of rankings\cr
#' orderings \tab  \tab the fit in terms of orderings\cr
#' info\tab \tab dataframe containing the terminal nodes in which the new x fall down, then the new x and the fit (in terms of rankings)
#' } 
#'
#'
#' @author Antonio D'Ambrosio \email{antdambr@unina.it}
#' 
#' @seealso \code{\link{ranktree}}  \code{\link{validatetree}}
#' 
#' @examples 
#' \donttest{
#' data(EVS)
#' EVS$rankings[is.na(EVS$rankings)] <- 3
#' set.seed(654)
#' training=sample(1911,1434)
#' tree <- ranktree(EVS$rankings[training,],EVS$predictors[training,],decrmin=0.001,num=50)
#' #use the function predict ro predict rankings for new predictors
#' rankfit <- predict(tree,newx=EVS$predictors[-training,])
#' #fit in terms of rankings
#' rankfit$rankings
#' #fit in terms of orderings
#' rankfit$orderings
#' # information about the fit (terminal node, predictor and fit (in terms of rankings))
#' rankfit$info
#' }
#' 
#' 
#' 
#' @importFrom janitor compare_df_cols_same
#' @importFrom pracma repmat
#' 
#' @export


predict.ranktree <- function(object,newx,...){
  
  #object is an object of class "ranktree"
  #x is a new X
  
  Tree <- object
  
  checktest <- compare_df_cols_same(Tree$X,newx,bind_method = "rbind",verbose=FALSE)
  if (isFALSE(checktest)){stop("New x in not conform to the predictor data frame")}
  
  xt <- downtree(Tree,newx)
  idleaf <- sort(unique(xt$leafid))
  idx <- xt$xid
  class <- Tree$tsynth$children$class
  classid <- Tree$tsynth$children$classid
  numclass <- Tree$tsynth$children$nclass
  
  numfit <- classfit <- matrix(0,nrow(newx),ncol(numclass))
  
  for (j in 1:length(idleaf)){
    
    leaves <- which(xt[,2]==idleaf[j])
    
    if (length(which(classid==idleaf[j]))>0){
      
      numfit[leaves,] <- repmat(numclass[which(classid==idleaf[j]),],length(leaves),1)
      
      
      #matrix(rep(numclass[which(classid==idleaf[j]),],length(leaves)),ncol=Co,byrow=TRUE)
      
      
      classfit[leaves,] <- matrix(rep(class[which(classid==idleaf[j]),],length(leaves)),ncol=ncol(numclass),byrow=TRUE)
      
      
    }
    
    
  }
  
  orderings <- classfit
  rankings <- numfit
  colnames(rankings) <- c(colnames(numclass))
  infofit <- data.frame(cbind(xt[,-1]),rankings)
  
  return(list(rankings=rankings,orderings=orderings,info=infofit))
  
}#end function

#-----------------

#' @importFrom methods is

downtree <- function(Tree,x){
  
  #path of the (test) sample x into the tree
  
  xid <- seq(1:nrow(x))
  nomin <- Tree$nomin
  
  if (is(Tree$tsynth$parents,"logical")){  #a tree with only the root node
    index <- rep(1,length(xid))
  } else { #any other tree
    
    
    nodint <- Tree$tsynth$parents$measures[,1:2]  #id and node number of parents
    index <- rep(1,nrow(x))
    
    
    for (j in 1:nrow(nodint)){
      
      idnode <- nodint$idp[j]
      numnode <- nodint$nn[j]
      col <- Tree$node$varsplitid[[idnode]] #column generating split
      split <- Tree$node$cutspli[[idnode]] #splitting rule
      nomspl <- nomin[col]  #nature of the splitting variable (1 nominal, 0 ordinal)
      
      if (nomspl==0){  #ordinal and numerical case
        
        index[which(index==idnode & x[,col]<=split)] <- Tree$node$idchildren[[idnode]][1] #left
        index[which(index==idnode)] <- Tree$node$idchildren[[idnode]][2] #right
        
        
        
      } else { #nominal case
        
        
        index[which(index==idnode & x[,col] %in% split)] <- Tree$node$idchildren[[idnode]][1] #left
        index[which(index==idnode)] <- Tree$node$idchildren[[idnode]][2] #right
        
        
        
      } #end condition
      
      
      
      
      
    }#end for
    
    
    
  }
  
  
  
  x <- cbind(xid,index,x)
  colnames(x)[2] <- "leafid"
  return(x)
  
  
}

