

generate_data = function(n,p) {
  num = n
  per = p
  m=matrix(rnorm(num*per, mean=0, sd=1), nrow=num, ncol=per)
  v=rnorm(num, mean=0, sd=1)
  list(covariates = m, responses = v)
}


model_select = function(covariates, responses, cutoff) {
  num = ncol(covariates)
  pvalues = summary(lm(responses ~ covariates))$coefficients[,4]
  if (length(pvalues) > 0) {
    pvalues = summary(lm(responses ~ covariates))$coefficients[,4][-1]
    covariates = covariates[, c(is.na(pvalues)==FALSE)]
    covariates = covariates[, c(cutoff<pvalues)]
    pvalues = summary(lm(responses ~ covariates))$coefficients[,4][-1]
    return(pvalues)
  }
  if (length(pvalues) == 0) {
    return(c())
  }
}

