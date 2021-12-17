def_cdf_inv=function(cdf,...){
  f=function(x,p,...){
    cdf(x,...)-p
  }
  
  f_root=function(p,...){
    uniroot(f,interval=c(-1,1)*1e3,p=p...,extendInt="upX",tol-Machine$double.ep5^0.5,maxiter=1e4)$root
  }
  return(f_root)
}