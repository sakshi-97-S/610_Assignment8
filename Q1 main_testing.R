inverse_sampling(100,pnorm,qnorm,mean=102,sd=25)
ks_test_out=ks.test(x,pnorm,mean=100,sd=25)
class(ks_test_out)
structure(ks_test_out)
names(ks_test_out)


f=function(a,b,c){
  return(a+b+c)
}
names(formals(f))