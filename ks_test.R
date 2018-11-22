#KS determines if a data set is normal with mean mu and standard deviation sd.
# KS outputs pvalue. If pvalue >= 0.05, our data is normal

d = function(data){
  data = sort(data)
  mu = mean(data)
  sd = sd(data)
  F = pnorm(data,mu,sd)
  Fu = c(1:length(data))/length(data)
  Fl = Fu - 1/(length(data))
  d = max(abs(F - Fl),abs(F-Fu))
  return(d)
}

KS = function(d,mu,sd,n)
{ # d is the KS distance for our data set compared to the Norm(mu,sd)
  # m is the number of iterations used to calculate the pvalue
  # n is the data set size
  # mu and sd are our parameters
  KSout = NULL #our output vector
  Fu = c(1:n)/n
  Fl = Fu - 1/n
  for (i in 1:n)
  {
    newdata = sort(rnorm(n,mu,sd))
    newmu = mean(newdata)
    newsd = sd(newdata)
    F = pnorm(newdata,newmu,newsd) #CDF values
    d_dot = max(abs(F-Fl),abs(F-Fu))
    KSout = c(d_dot,KSout)
  } #End for loop
  pvalue = length(KSout[KSout>d])/500
  return (pvalue)
} #End function