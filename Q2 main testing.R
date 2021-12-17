ks.test(out,pnorm)
draws=rejection_sampling(12,targ_pdf,prop_pdf,prop_ran,envelope_const=10)
draws
length(draws)
