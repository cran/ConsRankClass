#'Determine a tree from the main tree-based structure
#'
#'Given a tree belonging to the class "ranktree", determine a subtree with a given number of terminal nodes
#'
#' @param Tree An object of the class "ranktree" coming form te function \code{ranktree}
#' @param cut The maximum number of terminal nodes that the Tree must have
#' @param tokeep parameter invoked by other internal functions
#' 
#' @details If the pruning sequence returns a series of subtrees with, say, 1,2,4,7,9 terminal nodes and the user set cut=8, 
#' the function extract the subtree with 7 terminal nodes.
#' 
#' @return An object of the class "ranktree", containing the same information of the output of the function \code{ranktree}
#' 
#' 
#' @examples
#' 
#' data("Univranks")
#' tree <- ranktree(Univranks$rankings,Univranks$predictors,num=50)
#' #see how many terminal nodes have the trees compomimg the nested sequence of subtrees
#' infoprun <- tree$pruneinfo$termnodes
#' #select the tree with, say, 6 terminal nodes
#' tree6 <- getsubtree(tree,6)
#' 
#' 
#' @author Antonio D'Ambrosio \email{antdambr@unina.it}
#' 
#' 
#' @export



getsubtree <- function(Tree,cut,tokeep=NULL){
  
  if (is(tokeep,"NULL")){
    
    cut2 <- which(Tree$pruneinfo$termnodes==(cut+1))
    
    if(length(cut2)==0){
      cnt <- 2
      repeat{
        cut2 <- which(Tree$pruneinfo$termnodes==(cut+cnt))
        if(length(cut2)==0){
          cnt <- cnt+1
        } else {
          
          break  
          
          }
      }
      
      
    }
    
    ids <- seq(1:max(Tree$pruneinfo$prunelist))
    
    cut <- which(Tree$pruneinfo$prunelist>0 & Tree$pruneinfo$prunelist <= ids[cut2])
    
    N <- Tree$numnodes
    children <- Tree$idgenealogy[,2:3]
    parents <- cut
    tokeep <- rep(1,N)
    
    okids <- vector(mode="numeric",length=0L)
    
    while(sum(tokeep)>0){
      
      
      
      newkids <- c(as.matrix(children[parents,]))
      
      newkids <- newkids[newkids>0 & !is.element(newkids,okids)]
      
      if (length(newkids)==0){break}
      
      okids <- c(okids,newkids)
      tokeep[newkids] <- 0
      parents <- newkids
      
    }
    
  } #end tokeep=NULL
  
  TRE <- Tree
  TRE$node$varsplit[cut] <- NA
  TRE$node$cutspli[cut] <- NA
  TRE$node$terminal[cut] <- TRUE
  TRE$node$children[cut] <- NA
  TRE$node$idchildren[cut] <- NA
  
  fields <- names(Tree$node)
  subtree <- list()
  subtree$node <- vector(mode = "list", length = length(fields))
  names(subtree$node) <- fields
  
  for (j in 1:length(fields)){
    subtree$node[[j]] <- TRE$node[[j]][which(tokeep==1)]
  }
  
  subtree$tsynth <- synthtree(subtree)
  
  if( nrow(subtree$tsynt$children$measures)>1 ){
    
    subtree <- layouttree(subtree)
    #update children id and father id
    idn <- which(rowSums(subtree$idgenealogy)>0)
    
    for (j in 1:length(idn)){
      
      subtree$node$idchildren[[idn[j]]] <- c(as.matrix((subtree$idgenealogy[idn[j],2:3])))
      subtree$node$children[[idn[j]]] <- c(as.matrix((subtree$genealogy[idn[j],2:3])))
      
      
    }
    
    subtree$numnodes <- length(subtree$node$number)
    
    
    allnodes <- cbind(c(repmat(subtree$idgenealogy$father,1,2)),  
                      cbind(c(subtree$idgenealogy$leftchild,subtree$idgenealogy$rightchild)))
    
    allnodes <- allnodes[rowSums(allnodes)>0,]
    
    for (j in 1:(subtree$numnodes-1)){
      
      subtree$node$idfather[[allnodes[j,2]]] <- allnodes[j,1] 
      
    }
    
    
  } else {
    
    subtree$numnodes <- 1
    subtree$node$idchildren[[1]] <- subtree$node$children[[1]] <- NA
    
    
  }
  
  
  subtree$nomin <- Tree$nomin
  subtree$goodness$error <- sum(unlist(subtree$node$werror[subtree$tsynth$children$measures$idt]))
  subtree$goodness$tau <- sum(unlist(subtree$node$wtau[subtree$tsynth$children$measures$idt]))
  subtree$goodness$impurity <- sum(unlist(subtree$node$wimpur[subtree$tsynth$children$measures$idt]))
  subtree$X <- Tree$X
  subtree$Y <- Tree$Y
  subtree$control <- Tree$control
  
  class(subtree) <- c("ranktree","ConsRankClass")
  
  return(subtree)
  
  
  
}
