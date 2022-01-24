plotsample_est = function(n,p,alpha=0.05,method="lognormal",dbn="binomial",B=999,Nmult=3) {
  ntot = sum(n)
  Nhat = ntot/p
  if(dbn=="bootstrap") {
    b.Nhat = rep(NA,B)
    for(i in 1:B) b.Nhat[i] = sum(sample(n,size=length(n),replace=TRUE))/p
    se.Nhat = sqrt(var(b.Nhat)*(B-1)/B)
    K = length(n)
    se.Nhat = se.Nhat*sqrt(K/(K-1))
    Nhat = sum(n)/p
    cv.Nhat = se.Nhat/mean(b.Nhat)
    if(method=="normal") {
      ci.Nhat = round(norm.ci(Nhat,se.Nhat,alpha))
    }else if(method=="lognormal") {
      ci.Nhat = round(lognorm.ci(Nhat,se.Nhat,alpha))
    }else if(method=="percentile") {
      ci.Nhat = round(quantile(b.Nhat,probs=c(alpha/2,1-alpha/2)))
    }else {
      stop("Invalid method. For boostrap, method must be 'normal', 'lognormal' or 'percentile'.")
    }
  }else {
    if(dbn=="binomial") {
      var.Nhat = Nhat*(1-p)/p
    }else if (dbn=="poisson") {
      var.Nhat = Nhat/p
    }else {
      stop("Invalid dbn. It must be 'binomial', 'poisson' or 'bootstrap.")
    }
    se.Nhat = sqrt(var.Nhat)
    cv.Nhat = se.Nhat/Nhat
    if(method=="normal") {
      ci.Nhat = round(norm.ci(Nhat,se.Nhat,alpha))
    }else if(method=="lognormal") {
      ci.Nhat = round(lognorm.ci(Nhat,se.Nhat,alpha))
    }else if(method=="exact") {
      Ns = c(ntot:(Nmult*Nhat))
      cdf = rep(NA,length(Ns))
      if(dbn=="binomial") for(i in 1:length(Ns)) cdf[i] = pbinom(ntot,Ns[i],p)
      else if(dbn=="poisson") for(i in 1:length(Ns)) cdf[i] = ppois(ntot,Ns[i]*p)
      else stop("Invalid dbn. With method 'exact', dbn must be 'binomial' or 'poisson'.")
      lo = Ns[max(which(cdf>=(1-alpha/2)))]
      up = Ns[min(which(cdf<=(alpha/2)))]
      ci.Nhat = round(c(lo,up))
    }
  }
  return(list(Nhat=Nhat,se.Nhat=se.Nhat,cv.Nhat=cv.Nhat,ci.Nhat=ci.Nhat,method=method))
}