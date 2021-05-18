#'Path of a terminal node
#'
#'Given an object of the class "ranktree", it visualize the path leading to the terminal node
#'
#' @param termnode The terminal node of which the path has to be extracted
#' @param Tree An object of the class "ranktree" 
#'
#' @return The path leading to the terminal node
#' 
#' @examples 
#' \donttest{
#' data(Irish)
#' #build the tree with default options
#' tree <- ranktree(Irish$rankings,Irish$predictors)
#' #get information about all the paths leading to terminal nodes
#' paths <- treepaths(tree)
#' #see the path for terminal node number 8
#' nodepath(termnode=8,tree)
#' }
#'  
#' @seealso \code{\link{ranktree}}, \code{\link{treepaths}}, \code{\link{getsubtree}}
#'  
#' @author Antonio D'Ambrosio \email{antdambr@unina.it}
#'  
#' @export
#'    






nodepath <- function(termnode,Tree){
  
  #function that highlights the path of a terminal node
  
  idtn <- which(Tree$node$number==termnode) #id of termnodes
  
  nodeclass <- Tree$tsynth$children$class[which(Tree$tsynth$children$classid==idtn),]
  nodetau <- Tree$node$tau[[idtn]]
  nodekemeny <- Tree$node$error[[idtn]]
  nodesize <- Tree$node$size[[idtn]]
  
  #isleaf = unlist(Tree$node$terminal)
  #idleaf=which(isleaf==TRUE)
  #idnumleaf=unlist(Tree$node$number[idleaf])
  
  
  
  node <- nodes <- idtn
  repeat{
    fnode=Tree$node$idfather[[node]]
    nodes=c(fnode,nodes)  #id nodes of the path
    node=fnode
    if(node==1){break}
    
  }
  
  path <- unlist(Tree$node$number[nodes]) #path leading to node termnode
  pathvar <- unlist(Tree$node$varsplit[nodes])
  
  Pa1 <- NULL
  
  for (j in 1:(length(path)-1)){
    
    varn <- which(names(Tree$nomin)==pathvar[j])
    
    if(Tree$nomin[names(Tree$nomin)==pathvar[j]]==0){#ordinal or numerical predictor
      
      
      if( which(Tree$node$idchildren[[nodes[j]]]==nodes[(j+1)])==1   ) {#left child
        
        P1 <- paste(pathvar[j], paste("<=",collapse=''), paste(format(Tree$node$cutspli[[nodes[j]]],digits=3))  )
        
      } else {
        
        
        P1 <- paste(pathvar[j], paste(">",collapse=''), paste(format(Tree$node$cutspli[[nodes[j]]],digits=3))  )
        
      }
      
      
    } else {#nominal case
      
      
      if( which(Tree$node$idchildren[[nodes[j]]]==nodes[(j+1)])==1   ) {#left child
        
        P1 <- paste(pathvar[j], paste("in {",collapse=''), paste(Tree$node$cutspli[[nodes[j]]],collapse=', '), paste("}")  )
        
      } else {
        
        catgs <- unique(Tree$X[Tree$node$idatnode[[nodes[(j+1)]]],varn])
        
        P1 <- paste(pathvar[j], paste("in {",collapse=''), paste(catgs,collapse=', '), paste("}")  )
        
      }
      
      
      
      
      
    }
    
    if(j < (length(path)-1) ){
      
      Pa1 <- paste(Pa1,P1,paste("AND"))
      
    } else {
      
      Pa1 <- paste(Pa1,P1)
    }
    
  }#end for
  
  Pa1 <- paste("Terminal node number ", Tree$node$number[[idtn]], ":", sep="", Pa1)
  
  Pa1 <- paste(Pa1,paste(". CLASS: <",paste(nodeclass, collapse=" "),paste(">")))
  
  Pa1 <- paste(Pa1,paste(". Averaged Tau_X within node:",paste(format(nodetau, digits = 3) ,".")))
  
  Pa1 <- paste(Pa1,paste("Error-Kemeny distance within node:",paste(format(nodekemeny, digits = 3) ,".")))
  
  Pa1 <- paste(Pa1,paste("Sample size within node:",paste(format(nodesize, digits = 3) ,".")))
  
  return(Path=Pa1)
  
}
