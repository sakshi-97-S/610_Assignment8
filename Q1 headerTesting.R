test_that("ks_test",{
  x=inverse_sampling(100,pnorm,mean=100,sd=25)
  ks_test_out=ks.test(x,pnorm,mean=100,sd=25)
  expect_gt(ks_test_out$p.value,0.01)
})