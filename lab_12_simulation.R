generate_data = function(n,p) {
  num = n
  per = p
  m=matrix(rnorm(num*per, mean=0, sd=1), nrow=num, ncol=per)
  v=rnorm(num, mean=0, sd=1)
  list(covariates = m, responses = v)
}



model_select = function(covariates, responses, cutoff) {
  reg = lm(responses ~ covariates)
  t = t.test(responses, covariates)[3]
  if (t>cutoff) {
    
  } 
  if (t>=cutoff) {
    
  }
}
