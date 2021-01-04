integral_definida <- function(funcion,a,b,n,aprox='rect'){
  
  interv <- (b-a)/n
  
  seg <- seq(from=a,to=b,by=interv)
  
  if (aprox=='trap'){
    
    strt_end <- c(funcion(seg[1]),funcion(seg[n+1]))
    mid <- 2*funcion(seg[2:n])
    
    return(( (b-a)/(2*n) )*sum(strt_end,mid))
  }
  else if (aprox=='simp'){
    
    strt_end <- c(funcion(seg[1]),funcion(seg[n+1]))
    seg_cut <- seg[2:n]
    
    indices <- 1:length(seg_cut)
    mid <- c(4*funcion(seg_cut[indices%%2 != 0]),
             2*funcion(seg_cut[indices%%2 == 0]))
    
    return(( (b-a)/(3*n) )*sum(strt_end,mid))
  }
  else{
    
    In <- double(1)
    Sn <- double(1)
    
    for (i in 1:n){
      fmi <- min(funcion(seg[i]:seg[i+1]))
      In <- In+fmi*interv
      fMi <- max(funcion(seg[i]:seg[i+1]))
      Sn <- Sn+fMi*interv
    }
    return(list(In=In,Sn=Sn))
  }
}