

test_that("ks_test",{
 z= def_env_const( dnorm, function(x) 1/pi/(1+x^2))<1.521
 ks_test_out =def_env_const( dexp, function(x) 1/(1+x)^2)<1.472
})
 
 
