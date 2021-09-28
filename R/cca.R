#' K-Median Cluster Component Analysis
#' 
#' K-Median Cluster Component Analysis, a distribution-free soft-clustering method
#' for preference rankings.
#' 
#' 
#' @param X A n by m data matrix containing preference rankings, in which there are n judges and m objects to be judged. Each row is a ranking of the objects which are represented by the columns. 
#' @param k The number of cluster components
#' @param control a list of options that control details of the \code{cca} algorithm governed by the function
#' \code{ccacontrol}. The options govern maximum number of iterations of \code{cca} (itercca=1 is the default), the algorithm chosen to 
#' compute the median ranking (default, "quick"), and other options related
#' to the consrank algorithm, which is called by \code{cca}
#' @param \dots arguments passed bypassing \code{ccacontrol}
#' 
#' @details The user can use any algorithm implemented in the \code{consrank} function from the \pkg{ConsRank} package. All algorithms allow the user to set the option 'full=TRUE' 
#' if the median ranking(s) must be searched in the restricted space of permutations instead of in the unconstrained universe of rankings of n items including all possible ties. 
#' There are two classification uncertainty measures: Us and Uprods. "Us" is the geometric
#' mean of the membership probabilities of each individual, normalized in such a way that
#' in the case of maximum uncertainty Us=1. "Ucca" is the average of all the "Us".
#' "Uprods" is the product of the membership probabilities of each individual, normalized in such a way that
#' in the case of maximum uncertainty Uprods=1. "Uprodscca" is the average of all the "Uprods".
#' 
#' @return An object of the class "cca". It contains:
#' \tabular{lll}{
#' pk\tab \tab the membership probability matrix\cr
#' clc\tab \tab  cluster centers\cr
#' oclc \tab \tab cluster centers in terms of orderings\cr
#' idc\tab  \tab crisp partition: id of the cluster component associated with the highest membership probability\cr
#' Hcca  \tab \tab Global homogeneity measure (tau_X rank correlation coefficient) \cr
#' hk \tab \tab Homogeneity within cluster\cr
#' props \tab \tab estimated proportion of cases within cluster\cr
#' Us \tab \tab Uncertainty measure per-individual (see details)\cr
#' Ucca \tab \tab Global uncertainty measure\cr
#' Uprods \tab \tab Uncertainty measure per-individual (see details)\cr
#' Uprodscca \tab \tab Global uncertainty measure\cr
#' consrankout \tab \tab complete output of rank aggregation algorithm, containing eventually multiple median rankings
#'} 
#'
#' @examples
#' \donttest{
#' data(Irish)
#' set.seed(135) #for reproducibility
#' # CCA with four components
#' ccares <- cca(Irish$rankings, 4, itercca=10)
#' summary(ccares)
#' }
#' 
#' 
#' @author Antonio D'Ambrosio \email{antdambr@unina.it}
#' 
#' @references D'Ambrosio, A. and Heiser, W.J. (2019). A Distribution-free Soft Clustering Method for Preference Rankings. Behaviormetrika , vol. 46(2), pp. 333–351, DOI: 10.1007/s41237-018-0069-5
#' @references Heiser W.J., and D'Ambrosio A. (2013). Clustering and Prediction of Rankings within a Kemeny Distance Framework. 
#' In Berthold, L., Van den Poel, D, Ultsch, A. (eds). Algorithms from and for Nature and Life.pp-19-31. Springer international. DOI: 10.1007/978-3-319-00035-0_2.
#' @references Ben-Israel, A., and Iyigun, C. (2008). Probabilistic d-clustering. Journal of Classification, 25(1), pp.5-26. DOI: 10.1007/s00357-008-9002-z
#' 
#' @keywords Adjusted Concordance Index
#' @keywords Normalized Degree of Concordance
#' @keywords Soft clustering
#' @keywords Preference rankings
#' 
#' @import ConsRank
#' @importFrom pracma repmat
#' @importFrom stats runif
#' @importFrom utils combn
#' 
#' @seealso \code{ccacontrol}
#' @seealso \code{ranktree}
#' 
#' 
#' @export 



cca <- function(X,k,control=ccacontrol(...), ...){
  
  
  
  out <- list(pk=NULL,clc=NULL,oclc=NULL,idc=NULL,Hcca=NULL,
              hk=NULL, props=NULL, Us=NULL, Ucca=NULL,
              Uprods=NULL,Uprodscca=NULL, consrankout=NULL,
              replications=NULL, settings=control)
  
  if (control$itercca==1){
    
    res <- ccacore(X,k,control)
    out$pk <- res$pk
    out$clc <- res$clc
    out$oclc <- rank2order(res$clc,items=colnames(X))
    out$idc <- res$idc
    out$Hcca <- res$Hcca
    out$hk <- res$hk
    out$props <- res$props
    out$Us <- res$Us
    out$Ucca <- res$Ucca
    out$Uprods <- res$Us2
    out$Uprodscca <- res$Ucca2
    out$consrankout <- res$Clusters
    out$replications <- 1
    
  }else{
    
    iterout <- out
    
    for (j in 1:control$itercca){
    
      res <- ccacore(X,k,control)
      iterout$pk[[j]] <- res$pk
      iterout$clc[[j]] <- res$clc
      iterout$oclc[[j]] <- NA
      iterout$idc[[j]] <- res$idc
      iterout$Hcca[[j]] <- res$Hcca
      iterout$hk[[j]] <- res$hk
      iterout$props[[j]] <- res$props
      iterout$Us[[j]] <- res$Us
      iterout$Ucca[[j]] <- res$Ucca
      iterout$Uprods[[j]] <- res$Us2
      iterout$Uprodscca[[j]] <- res$Ucca2
      iterout$consrankout[[j]] <- res$Clusters
      
    }#end for
    
    Ucca <- unlist(iterout$Ucca)
    idbest <- which.min(Ucca)
    
    out$pk <- iterout$pk[[idbest]]
    out$clc <- iterout$clc[[idbest]]
    out$oclc <- rank2order(iterout$clc[[idbest]],items=colnames(X))
    out$idc <- iterout$idc[[idbest]]
    out$Hcca <- iterout$Hcca[[idbest]]
    out$hk <- iterout$hk[[idbest]]
    out$props <- iterout$props[[idbest]]
    out$Us <- iterout$Us[[idbest]]
    out$Ucca <- iterout$Ucca[[idbest]]
    out$Uprods <- iterout$Uprods[[idbest]]
    out$Uprodscca <- iterout$Uprodscca[[idbest]]
    out$consrankout <- iterout$Clusters[[idbest]]
    out$replications <- control$itercca

    
 
  }#end if
  
  class(out) <- c("cca","ConsRankClass")
  
  print(out)
  
  return(out)

  
}



#-------------------------------------------------------------------------
ccacore <- function(X,
                    k,
                    control) {
  #K-Median cluster component analysis by D'Ambrosio & Heiser
  
  
  r <- nrow(X)
  co <- ncol(X)
  R <- 10
  L <- 0
  Prold <- matrix(0, 1, r)
  PXold <- matrix(0, k, r)
  maxcuf <- (1 / k) ^ k
  
  Cs <- t(replicate(k, order(runif(co))))
  
  
  if (nrow(unique(Cs))!=k){ #cannot have same cluster center
    repeat{
      Cs <- t(replicate(k, order(runif(co))))
      if((nrow(unique(Cs))==k)){break}
    }
  }
  
  
  # #-----------------
  # Cs <- t(apply(matrix(0, k, co), 1, function(x)
  #   sample(c))) #random cluster centers
  # 
  # if (nrow(unique(Cs))!=k){
  #   
  #   repeat{
  #     
  #     Cs <- t(apply(matrix(0, k, co), 1, function(x)
  #       sample(co))) #random cluster centers
  #     
  #     if((nrow(unique(Cs))==k)){break}
  #     
  #   }
  #   
  # }
  # #-----------------
  
  Accs1 <- toler <- 1e-10
  TTTT <- 10
  
  
  while (TTTT > toler) {
    L <- L + 1
    d <- kemenyd(X, Cs)
    dd <- d
    PK <- combprobs(dd, k) #equation 5, behaviormetrika paper
    
    Uk <- PK ^ 2
    
    #ddd <- d
    
    #ddd[ddd == 0] <- 1
    
    #    forint <- sqrt(PK ^ 2 * ddd)
    
    #    intensity <- matrix(colSums(forint) / sum(forint), ncol = 1) #cluster intensity
    
    intensity <- matrix(colMeans(PK), ncol=1)
    
    Uk <- Uk / repmat(matrix(colSums(Uk), nrow = 1), dim(d)[1], 1) #normalize intensity weight 
    
    Clusters <- vector(mode = "list")
    for (c in 1:k) {
      Clusters[[c]] <- consrank(
        X, #all data
        wk = Uk[, c], #weigths
        algorithm = control$algorithm,
        ps = control$ps,
        proc=control$proc,
        full=control$full,
        itermax=control$consrankitermax,
        np=control$np,
        gl=control$gl,
        ff=control$ff,
        cr=control$cr
      )
    }
    
    # centers <- list.rbind(sapply(Clusters, rbind)[1, ])
    # itaux <- list.rbind(sapply(Clusters, rbind)[2, ]) #h_k, eq. 6 behaviormetrika
    centers <- matrix(0,k,co)
    itaux <- matrix(0,k,ncol=1)
    for (h in 1:k){
      centers[h,] <- Clusters[[h]]$Consensus[1,]
      itaux[h,1] <- Clusters[[h]]$Tau[1]
    }
    Globtau <- crossprod(itaux, intensity) #H_cca, eq. 7 behaviormetrica
    # Accsiter <- colMeans(d * PK)[1] 
    Accsiter <- colMeans(d * (PK^2))[1] #loss function, eq. 2
    TTTT <- abs(Accsiter - Accs1)
    Accs1 <- Accsiter
    # print(Accs1)
    # flush.console()
    indexC <- apply(PK, 1, which.max)
    Cs <- centers #messo oggi, 19/4/2021
    
  } #end while
  
  unc1 <- (apply(PK, 1, prod)) / maxcuf #first uncertainty measure: 
  
  
  # jdf <- apply(dd, 1, prod) / PrK$den
  # 
  # unc2 <- k*jdf / (apply(dd, 1, prod)^(1/k))
  # 
  # unc2[which(is.na(unc2))] <- 0 #eq. 8 long format
  
  unc2 <- k*( (apply(PK, 1, prod)) )^(1/k) #eq. 8 behaviormetrika
  #geometric mean for each individual, normalized to be equal to one 
  #in case of maximum uncertailnty
  
  return(
    list(
      pk = PK, #membership
      clc = centers, #cluster centers
      idc = indexC, #id of observations in the clusters
      Hcca = Globtau, #Global homogeneity
      hk = itaux, #Homogeneity within clusters
      props = intensity, #estimated proportions within cluster
      Us = unc2, #normalized classifiability (CUF) for each id
      Ucca = mean(unc2), #Global normalized classifiability (CUF)
      Us2 = unc1, #normalized classifiability (only prod of probs)
      Ucca2 = mean(unc1), #Global normalized classifiability
      loss = Accsiter, #loss function
      Iters = L, #iterations
      Clusters = Clusters #info about the k output of consrank
    )
  )
}

#--------------------------------------
combprobs <- function(D,k){
  
  if (k==2){
    
    denom <- rowSums(D)
    Numeratore <- cbind(D[,2],D[,1])
    
    
  } else {
    
    combinazioni <- t(combn(k,(k-1)))
    productories <- matrix(0,dim(D)[1],dim(combinazioni)[1])
    for (j in 1:k){productories[,j] <- apply(D[,combinazioni[j,]],1,prod)}
    
    denom<-rowSums(productories)
    indk<-apply(combinazioni,1,function(x) setdiff(seq(1:k),x))
    
    Numeratore<-matrix(0,dim(D)[1],k)
    for (j in 1:k){
      Numeratore[,indk[j]]<-productories[,j]
    }
    
    #Numeratore<-productories[,indk]
    
  }
  
  PK<-Numeratore/repmat(matrix(denom,ncol=1),1,k)
  

}
