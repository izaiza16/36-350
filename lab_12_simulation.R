

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


run_simulation = function(n_trials, n, p, cutoff) {
  pvalues = c()
  for (i in 1:n_trials) {
    d = generate_data(n, p) 
    pval = model_select(d$covariates, d$responses, cutoff)
    pvalues = c(pvalues, pval)
  }
  return(pvalues)
}

par(mfrow=c(3,3))
hist(run_simulation(100, 100, 10, 0.05))
hist(run_simulation(100, 1000, 10, 0.05))
hist(run_simulation(100, 10000, 10, 0.05))
hist(run_simulation(100, 100, 20, 0.05))
hist(run_simulation(100, 1000, 20, 0.05))
hist(run_simulation(100, 10000, 20, 0.05))
hist(run_simulation(100, 100, 50, 0.05))
hist(run_simulation(100, 1000, 50, 0.05))
hist(run_simulation(100, 10000, 50, 0.05))

run_simulation = function(n_trials, n, p, cutoff) {
  pvalues = c()
  for (i in 1:n_trials) {
    d = generate_data(n, p) 
    pval = model_select(d$covariates, d$responses, cutoff)
    pvalues = c(pvalues, pval)
  }
  write.csv(pvalues, file="my.pvalues.csv")
}

make_plot = function (datapath) {
  pvalues = read.csv(datapath)
  hist(pvalues)
}

