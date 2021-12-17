library(numDeriv)

x_intercept_fun = function(x,y,s,ind_low,ind_up){
  (s[ind_low]*x[ind_low]-s[ind_up]*x[ind_up]+y[ind_up]-y[ind_low])/(s[ind_low]-s[ind_up])
}

y_intercept_fun = function(x,y,s,ind_low,ind_up){
  (s[ind_low]*s[ind_up]*(x[ind_low]-x[ind_up])+s[ind_low]*y[ind_up]-s[ind_up]*y[ind_low])/(s[ind_low]-s[ind_up])
}

areas_fun = function(x,y,s,x_intervals){
  a = (x_intervals[,2]-x_intervals[,1])*exp(y)
  b = exp(s*(x_intervals[,2]-x)+y)/s - 
    exp(s*(x_intervals[,1]-x)+y)/s
  ifelse(abs(s)<.Machine$double.eps^0.75,a,b)
}

chord_slope_fun = function(x,y,ind_low,ind_up){
  (y[ind_up]-y[ind_low])/(x[ind_up]-x[ind_low])
}

initialize_envelope_info = function(env,h,interval,x,...){
  # ... are additional arguments to h and get fed to h 
  # and to h through grad
  if(length(interval)!=2) stop("interval is not lenghth 2")
  if(!is.numeric(interval)) stop("interval is not numeric")
  if(length(x)<2) stop("x has less than 2 points")
  if(!is.numeric(x)) stop("x is not numeric")
  interval = sort(interval)
  if(any(x<interval[1]) || any(x>interval[2])) stop("x values outside interval")
  dh_dx_1 = function(x){grad(h,x,...)}
  dh_dx = Vectorize(dh_dx_1)
  s = dh_dx(x)
  k = length(x)
  if(interval[1]== -Inf && s[1]<0){
    stop("left derivative condition not met for infinite interval")
  }
  if(interval[k]== Inf && s[k]>0){
    stop("right derivative condition not met for infinite interval")
  }
  env$x = x
  env$y = y = h(x,...)
  env$s = s
  ind_low = 1:(k-1)
  ind_up = 2:k
  env$x_intercept = x_intercept_fun(x,y,s,ind_low,ind_up)
  env$y_intercept = y_intercept_fun(x,y,s,ind_low,ind_up)
  env$x_intervals = cbind(c(interval[1],env$x_intercept),c(env$x_intercept,interval[2]))
  
  env$areas = areas_fun(env$x,env$y,env$s,env$x_intervals)
  env$c = chord_slope_fun(x,y,ind_low,ind_up)
}

update_environment_info = function(env,h,x_new,y_new,...){
  # ... are additional arguments to h and get fed to h through grad
  s_new = grad(h,x_new,...)
  index = (env$x < x_new)
  index
  if(all(!index)){
    env$x =  c(x_new,env$x)
    env$y = c(y_new,env$y)
    env$s = c(s_new,env$s)
    env$x_intercept = c(x_intercept_fun(env$x,env$y,env$s,1,2),
                        env$x_intercept)
    env$y_intercept = c(y_intercept_fun(env$x,env$y,env$s,1,2),
                        env$y_intercept)
    env$x_intervals = rbind(c(NA),env$x_intervals)
    
    env$x_intervals[c(1,2),1] = c(env$x_intervals[2,1],env$x_intercept[1])
    env$x_intervals[1,2] = env$x_intercept[1]
    
    env$areas = c(NA,env$areas)
    print(areas_fun(env$x[c(1,2)],env$y[c(1,2)],env$s[c(1,2)],env$x_intervals[c(1,2),c(1,2)]))
    env$areas[c(1,2)] = areas_fun(env$x[c(1,2)],env$y[c(1,2)],env$s[c(1,2)],env$x_intervals[c(1,2),c(1,2)])
    env$c = c(chord_slope_fun(env$x,env$y,1,2),env$c)
  }else if(all(index)){
    k = length(env$x)
    env$x = c(env$x,x_new)
    env$y = c(env$y,y_new)
    env$s = c(env$s,s_new)
    env$x_intercept = c(env$x_intercept,x_intercept_fun(env$x,env$y,env$s,k,k+1))
    env$y_intercept = c(env$y_intercept,y_intercept_fun(env$x,env$y,env$s,k,k+1))
    env$x_intervals = rbind(env$x_intervals,c(NA))
    env$x_intervals[c(k,k+1),2] = c(env$x_intercept[k],env$x_intervals[k,2])
    env$x_intervals[k+1,1] = env$x_intercept[k]
    env$areas = c(env$areas,NA)
    env$areas[c(k,k+1)] = areas_fun(env$x[c(k,k+1)],env$y[c(k,k+1)],env$s[c(k,k+1)],env$x_intervals[c(k,k+1),])
    env$c = c(env$c,chord_slope_fun(env$x,env$y,k,k+1))
  }else{
    k = length(env$x)
    ind = max(which(index))
    ind_1 = 1:ind
    ind_2 = (ind+1):k
    
    env$x = c(env$x[ind_1],x_new,env$x[ind_2])
    env$y = c(env$y[ind_1],y_new,env$y[ind_2])
    env$s = c(env$s[ind_1],s_new,env$s[ind_2])
    
    if(k==2){
      env$x_intercept = x_intercept_fun(env$x,env$y,env$s,c(1,2),c(1,2)+1)
      env$y_intercept = y_intercept_fun(env$x,env$y,env$s,c(1,2),c(1,2)+1)
      env$x_intervals = cbind(c(env$x_intervals[1,1],env$x_intercept),
                              c(env$x_intercept,env$x_intervals[2,2]))
      env$areas = areas_fun(env$x,env$y,env$s,env$x_intervals)
      env$c = chord_slope_fun(env$x,env$y,c(1,2),c(1,2)+1)
    }else{
      if(ind==1){
        env$x_intercept = c(x_intercept_fun(env$x,env$y,env$s,c(1,2),c(1,2)+1),
                            env$x_intercept[2:(k-1)])
        env$y_intercept = c(y_intercept_fun(env$x,env$y,env$s,c(1,2),c(1,2)+1),
                            env$y_intercept[2:(k-1)])
        env$x_intervals = rbind(cbind(c(env$x_intervals[1,1],env$x_intercept[c(1:2)]),
                                      c(env$x_intercept[1:3])),
                                env$x_intervals[3:k,])
        env$areas = c(areas_fun(env$x[1:3],env$y[1:3],env$s[1:3],env$x_intervals[1:3,]),env$areas[3:k])
        
        env$c = c(chord_slope_fun(env$x,env$y,c(1,2),c(1,2)+1),
                  env$c[2:(k-1)])
      }else if(ind==(k-1)){
        env$x_intercept = c(env$x_intercept[1:(k-2)],
                            x_intercept_fun(env$x,env$y,env$s,c(k-1,k),c(k,k+1)))
        env$y_intercept = c(env$y_intercept[1:(k-2)],
                            y_intercept_fun(env$x,env$y,env$s,c(k-1,k),c(k,k+1)))
        env$x_intervals = rbind(env$x_intervals[1:(k-2),],
                                cbind(c(env$x_intercept[(k-2):k]),
                                      c(env$x_intercept[c(k-1,k)],env$x_intervals[k,2])))
        env$areas = c(env$areas[1:(k-2)],areas_fun(env$x[(k-1):(k+1)],env$y[(k-1):(k+1)],env$s[(k-1):(k+1)],env$x_intervals[(k-1):(k+1),]))
        env$c = c(env$c[1:(k-2)],chord_slope_fun(env$x,env$y,c(k-1,k),c(k,k+1)))
      }else{
        env$x_intercept = c(env$x_intercept[1:(ind-1)],
                            x_intercept_fun(env$x,env$y,env$s,ind+c(0,1),ind+c(1,2)),
                            env$x_intercept[(ind+1):(k-1)])
        env$y_intercept = c(env$y_intercept[1:(ind-1)],
                            y_intercept_fun(env$x,env$y,env$s,ind+c(0,1),ind+c(1,2)),
                            env$y_intercept[(ind+1):(k-1)])
        env$x_intervals = rbind(env$x_intervals[1:(ind-1),],
                                cbind(c(env$x_intercept[(ind-1):(ind+1)]),
                                      c(env$x_intercept[ind:(ind+2)])),
                                env$x_intervals[(ind+2):k,])
        env$areas = c(env$areas[1:(ind-1)],
                      areas_fun(env$x[ind:(ind+2)],env$y[ind:(ind+2)],env$s[ind:(ind+2)],env$x_intervals[ind:(ind+2),]),
                      env$areas[(ind+2):k])
        env$c = c(env$c[1:(ind-1)],
                  chord_slope_fun(env$x,env$y,c(ind,ind+1),c(ind,ind+1)+1),
                  env$c[(ind+1):(k-1)])
      }
    }
  }
}

upper_envelope_sample = function(env){
  ind = sample(1:length(env$x),1,TRUE,env$areas)
  x = env$x[ind]
  y = env$y[ind]
  s = env$s[ind]
  interval = env$x_intervals[ind,]
  if(abs(s)>.Machine$double.eps^0.75){
    u = runif(1)
    #u = (exp(s*(t-x)+y)-exp(s*(interval[1]-x)+y))/(exp(s*(interval[2]-x)+y)-exp(s*(interval[1]-x)+y))
    return((log(u*(exp(s*(interval[2]-x)+y)-exp(s*(interval[1]-x)+y))+
                  exp(s*(interval[1]-x)+y))-y)/s+x)
  }else{
    return(runif(1,interval[1],interval[2]))
  }
}

lower_envelope_evaluate = function(x_val,env,log=TRUE){
  # for a single x_val value
  index = (env$x < x_val)
  if((!any(index)) || all(index)){
    out = -Inf
  }else{
    ind = max(which(index))
    out = env$c[ind]*(x_val-env$x[ind])+env$y[ind]
  }
  if(!log) out = exp(out)
  return(out)
}

upper_envelope_evaluate = function(x_val,env,log=TRUE){
  # for a single x_val value
  # evaluated as a function before normalization 
  # into a probability density
  # so it already has whatever that multiplicative constant would be
  # for getting a multiple of the associated density to envelope f
  ind = max(which(env$x_intervals[,1] < x_val))
  out = env$s[ind]*(x_val-env$x[ind])+env$y[ind]
  if(!log) out = exp(out)
  return(out)
}



