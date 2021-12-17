def_env_const=function(targ_pdf,prop_pdf){
  h=function(x){
    if(targ_pdf(x)< .Machine$double.eps){
      return(0)
    }
    else{
     return( targ_pdf(x)/prop_pdf(x))
    }
  opt = optimize(h,interval=c(-1,1)*10,maximum=TRUE,tol=.Machine$double.eps0^0.5)
  } 
  return(def_env_const(opt$objective))
}


  
  
  
  