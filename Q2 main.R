prop_ran=rcauchy
prop_pdf=dcauchy
targ_pdf=dnorm
M=10

N=1e4

rejection_sampling=ffunction(n,targ_pdf,prop_pdf,prop_ran,envelope_const){
  out=rep(NA,n)
  i=0
  while(TRUE){
    prop_value=prop_ran(1)
    u=runif(1)
    u<(targ_pdf(prop_value)/(envelope_const*prop_pdf(prop_value)))
    if((u*envelope_const*prop_pdf(prop_value))<(targ_pdf(prop_value))){
      i=i+1
      out[i]=prop_value
    }
    if(i==N) break
  }
  print(counter)
  return(out)
}

num_wanted=100
num_accepted=0

out=rep(NA,num_wanted)
while(TRUE){
  prop_val=prop_ran(1)
  u=runif(1)
  if(u<targ_pdf(x)/(envelope_const*prop_pdf(x))){
    index=num_accepted+1
    out[index]=prop_val
    num_accepted=num_accepted+1
  }
  condition=(num_accepted==num_wanted)
  if(condition)break
}
