adaptive_rejection_sampling=function(n,h,interval,x,...){
  
  initialize_envelope_info(envelope_env,h,interval,x,...)
  
  out=rep(NA,n)
  i=0
  while (True) {
    x_val=upper_envelope_sample(envelope_env)
    u=runif(1)
    
    upper_y_val=upper_envelope_evaluate(x_val,envelope_env,log=TRUE)
    
    lowerr_y_val=lower_envelope_evaluate(x_val,envelope_env,log=TRUE)
    
    if(log(u)<lowerr_y_val- upper_y_val){
      i=i+1
      out[i]=x_val
    }
  else{
    h_y_val=h(x_val,...)
    if(log(u)<h_y_val)
      i=i+1
    out[i]=x_val
  }
    update_environment_info(envelope_env,h,x_new,y_new,mu=mu,sigma=sigma)
    }
    
  }
}