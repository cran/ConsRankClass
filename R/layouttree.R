#'Utility function
#'
#'A utility function completing the output of the function \code{ranktree}.
#'
#' @param Tree an object of the class "ranktree"
#'
#' @return an object of the class "ranktree" completing the output of the function \code{ranktree}
#' 
#' @author Antonio D'Ambrosio \email{antdambr@unina.it}
#' 
#' 
#' @export


layouttree <- function(Tree){
  
  tsynth <- Tree$tsynth
  
  #tree, output of ranktree
  
  idisleaf <- tsynth$children$measures$idt  #id of terminal nodes
  numisleaf <- tsynth$children$measures$nn  #node number of terminal nodes
  idtnode <- tsynth$parents$measures$idp  #id of parent nodes
  numtnode <- tsynth$parents$measures$nn #node number of parent nodes
  nodes <- seq(1:length(idtnode))
  
  alln <- sort(c(numisleaf,numtnode))  #all nodes in terms of node number
  allid <- sort(c(idisleaf,idtnode))  #all nodes in terms of id node
  allp <- parid <- sapply(1:length(alln), function(row)
    ifelse(alln[row]%%2==0,alln[row]/2,(alln[row]-1)/2)
  )
  
  
  
  allc <- data.frame(matrix(0,length(alln),2))
  colnames(allc) <- c("left","right")
  isleaf <- unlist(Tree$node$terminal)
  
  for (j in 1:nrow(allc)){
    
    if (isFALSE(isleaf[j])){
      allc$left[j]=alln[j]*2
      allc$right[j]=(alln[j]*2)+1
    }
    
  }
  forf <- rep(0,length(alln))
  forf[which(allc[,1]!=0)] <- numtnode
  Tree$genealogy <- data.frame(cbind(forf,allc))
  colnames(Tree$genealogy) <- c("father","leftchild","rightchild")
  
  
  for (j in 1:length(allp)){
    
    ind <- which(allp==alln[j])
    parid[ind]=allid[j]
    ind <- which(alln[j]==allc$left)
    allc$left[ind] <- allid[j]
    ind <- which(alln[j]==allc$right)
    allc$right[ind] <- allid[j]
    
  }
  
  forf <- rep(0,length(alln))
  forf[which(allc[,1]!=0)] <- idtnode
  Tree$idgenealogy <- data.frame(cbind(forf,allc))
  colnames(Tree$idgenealogy) <- c("father","leftchild","rightchild")
  Tree$idparents <- parid
  
  
  terminals=unlist(Tree$node$terminal[allid])
  
  cx <- cy <- rep(0,length(terminals)) 
  
  for (j in 1:length(cy)){
    
    p <- parid[j]
    
    if (p > 0) {cy[j] <- cy[p]+1}
  }
  
  
  for (j in 1:length(cx)){
    
    p <- parid[j]
    
    if (p==0){
      
      cx[j] <- 0.5
      
    } else {
      
      dx <- 2^-(cy[j]+1)
      
      if (j==allc$left[p]){
        
        cx[j] <- cx[p] - dx
        
      } else {
        
        cx[j] <- cx[p] + dx
        
      }
      
    }
    
  }
  
  #make leaf nodes equally spaced, preserving their order
  
  nleaves <- length(idisleaf)
  b <- order(cx[idisleaf])
  cx[idisleaf[b]] <- seq(1:nleaves) / (nleaves+1)
  
  #Position parent nodes above and between their children
  
  for (j in max(cy):0){
    
    a <- which(cy==j & terminals==0)
    c <- allc[a,]
    cx[a] <- (cx[c$left] + cx[c$right])/2
    
  }
  
  
  k <- max(cy)
  cy <- 1- (cy+0.5)/(k+1)
  cy <- cy*-1 #parents above children
  
  for (j in 1:length(allid)){
    
    Tree$node$xcoord[[j]] <- cx[j]
    Tree$node$ycoord[[j]] <- cy[j]
    
  }
  
  return(Tree)
}