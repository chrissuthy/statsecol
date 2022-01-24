norm.ci = function(x,se,alpha=0.05) {
  x + qnorm(1-alpha/2)*c(-1,1)*se
}

lognorm.ci = function(x,se,alpha=0.05) {
  x*exp(qnorm(1-alpha/2)*c(-1,1)*sqrt(log(1+(se/x)^2)))
}
