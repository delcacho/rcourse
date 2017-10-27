library(stats4)

guessFunction <- function(x) { 
    
  llnorm <- function(mu, sigma) {
    R = suppressWarnings(dnorm(x, mu, sigma))
    -sum(log(R))
  }
  fit = mle(llnorm, start = list(mu = 1, sigma=1))
  vec = do.call(rnorm,list(10000,fit@details$par))
  pvalnorm = ks.test(vec,x)$p.value
  print("You generated from a Normal distribution with the following params")
  print(fit@details$par)
  print(paste("P-value",pvalnorm))
}
