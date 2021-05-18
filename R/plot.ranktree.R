#' Plot tree-based structure or pruning sequence of ranktree
#'
#' Plot the tree coming from the \code{ranktree} or the pruning sequence of the \code{ranktree}
#'
#' @param x An object of the class "ranktree"
#' @param plot.type One among "tree" or "pruningseq"
#' @param dispclass Display the median ranking above terminal nodes. Default option: FALSE
#' @param valtree If plot.type="pruningseq", it shows the Tau_x rank correlation coefficient or the error along the pruning sequence on the training set. If valtree is the output of the function \code{validatetree}, it shows either the Tau_x rank correlation coefficient or the error along the pruning sequence of also the decision tree (validated by wither test set or cross-validation)
#' @param taos If plot.type="pruningseq", it plots the Tau_x rank correlation coefficient along the pruning sequence. If taos=FALSE, it plots the error.
#' @param \dots System reserved (No specific usage)
#'
#' @return the plot of either the tree or the pruning sequence
#'
#' @author Antonio D'Ambrosio \email{antdambr@unina.it}
#' 
#' @seealso \code{\link{ranktree}},  \code{\link{validatetree}}
#' 
#' @examples
#' data("Univranks")
#' tree <- ranktree(Univranks$rankings,Univranks$predictors,num=50)
#' plot(tree,dispclass=TRUE)
#'   
#' \donttest{
#' data(EVS)
#' EVS$rankings[is.na(EVS$rankings)] <- 3
#' set.seed(654)
#' training=sample(1911,1434)
#' tree <- ranktree(EVS$rankings[training,],EVS$predictors[training,],decrmin=0.001,num=50)
#' plot(tree,dispclass=TRUE)
#' #test set validation
#' vtreetest <- validatetree(tree,testX=EVS$predictors[-training,],EVS$rankings[-training,]) 
#' dtree <- getsubtree(tree,vtreetest$best_tau)
#' plot(dtree,dispclass=TRUE)
#' #see the global weigthted tau_X rank correlation coefficients
#' plot(tree,plot.type="pruningseq",valtree=vtreetest)
#' #see the error rates
#' plot(tree,plot.type="pruningseq",valtree=vtreetest, taos=FALSE)
#' }
#' 
#' @keywords Tree-based structure
#' @keywords pruning sequence 
#' 
#' @importFrom graphics plot
#' 
#' @export


plot.ranktree <- function(x,plot.type="tree",dispclass=FALSE,valtree=NULL,
                          taos=TRUE,...){
  
  
  #x is an object of class "ranktree"
  
  Tree <- x
  plot.type <- match.arg(plot.type, c("tree", "pruningseq"), several.ok = FALSE)
  
  if (plot.type == "tree") {
    
    tsynth <- Tree$tsynth
    
    x <- unlist(Tree$node$xcoord)
    y <- unlist(Tree$node$ycoord)
    rx <- range(x)
    ry <- range(y)
    
    #prepare the plot
    #trellis.device()
    plot(x, 
         y, 
         type="n", 
         axes=FALSE, 
         ann=FALSE, 
         xlim=c(rx[1]-0.1,rx[2]+0.1 ), 
         ylim = c(ry[1]-0.1,ry[2]+0.1)
    )
    
    #get internal nodes
    idparent <- tsynth$parents$measures$idp
    numparent <- tsynth$parents$measures$nn
    splits <- tsynth$parents$measures$varp
    
    #get terminal nodes
    
    idterminal <- tsynth$children$measures$idt
    numterminal <- tsynth$children$measures$nn  
    classes <- tsynth$children$class
    
    points(x[idparent],y[idparent],col="black",cex=1.5)
    text(x[idparent],y[idparent]-0.05,splits,cex=0.7)
    text( x[idparent], y[idparent]+0.05, as.character(numparent), cex=0.7, col="black")
    
    points(x[idterminal],y[idterminal],col="blue",cex=1.5, pch=0)
    text( x[idterminal], y[idterminal]+0.05, as.character(numterminal), cex=0.7, col="red")
    
    if (isTRUE(dispclass)){
      
      for (i in 1:length(idterminal)){
        
        text( x[idterminal[i]], y[idterminal[i]]+0.1, paste(classes[i,], collapse= ' '), cex=0.7)
        
      }
      
    }
    
    terminals <- unlist(Tree$node$terminal)
    
    allnodes <- cbind(c(idparent,idterminal),c(numparent,numterminal))
    allnodes <- allnodes[order(allnodes[,1]),]
    
    for (j in 1:length(x)){
      
      if (j==1){
        
        if (isFALSE(terminals[j])){ #if internal
          
          if (is(Tree$node$cutspli[[j]],"numeric")){ #if numeric
            
            text(x[j],y[j]-0.075,substitute('' <= pr, list(pr=   round(Tree$node$cutspli[[j]],digits=3))    ),cex=0.7)
            
            
          } else {
            
            text(x[j],y[j]-0.075,substitute('' %in% group("{",pr,"}"), list(pr=paste(Tree$node$cutspli[[j]], collapse= ', '))),cex=0.7)
            
          } #end if numeric
          
          
          
        } #end if internal
        
        
      } else { #if j > 1
        
        nfather <- allnodes[which(allnodes[,2]==Tree$node$father[[allnodes[j]]]),1]
        xcoordf <- x[nfather]
        ycoordf <- y[nfather]
        segments(x[j],y[j],x1=xcoordf,y1=ycoordf,lty=5,col="grey")
        
        if (isFALSE(terminals[j])){#if internal
          
          if (is(Tree$node$cutspli[[j]],"numeric")){#if numeric
            
            text(x[j],y[j]-0.075,substitute('' <= pr, list(pr=      round(Tree$node$cutspli[[j]],digits=3))           ),cex=0.7)
            
            
          } else {
            
            text(x[j],y[j]-0.075,substitute('' %in% group("{",pr,"}"), list(pr=paste(Tree$node$cutspli[[j]], collapse= ', '))),cex=0.7)
            
          } #end if numeric
          
          
        } #end if internal
        
        
      }#end else j>1
      
    }#end for
  } 
  
  if (plot.type == "pruningseq"){
    
    #vt <- valtree
    #ta <- taos
    plotpruningseq(Tree,valtree,taos)
    
  }
  
}#end function

#-------------

#' @importFrom graphics points
#' @importFrom graphics text 
#' @importFrom graphics segments

plotpruningseq <- function(Tree,valtree=NULL,taos=TRUE){
  
  nterm <- Tree$pruneinfo$termnodes
  
  ifelse(isTRUE(taos),Rt <- Tree$pruneinfo$tau, Rt <- Tree$pruneinfo$error)
  ifelse(isTRUE(taos),ylabel <- "Tau_x rcc", ylabel <- "Error")
  if(!is(valtree,"NULL")){
    ifelse(isTRUE(taos),Rt_test <- valtree$tau, Rt_test <- valtree$error)
    plot(nterm, Rt, type="b", col="black", cex=0.8, 
         xlab="Number of terminal nodes", ylab = ylabel, 
         ylim=c(min(c(Rt,Rt_test)),(max(c(Rt,Rt_test))+0.006)),
         main=paste(valtree$validation, "procedure"))
    text(nterm, Rt+0.002, nterm, cex=0.6)
    points(nterm, Rt_test, col="red", cex=0.8, type="b")
    
  } else{
    
    plot(nterm, Rt, type="l", col="gray", xlab="Number of terminal nodes", ylab = ylabel, 
         ylim=c(min(Rt),(max(Rt)+0.006)),main="pruning sequence")
    points(nterm, Rt, col="black", cex=0.8)
    text(nterm, Rt+0.002, nterm, cex=0.6)
    
  }
  
}
