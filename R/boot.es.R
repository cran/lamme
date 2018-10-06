#' Boostrapped CI for Effect Size measures
#'
#' Compute the bias-corrected and expanded percentile boostrapped confidence intervals
#' for effect size estimates zetas and the overall signal-to-noise ratio. Additionally,
#' if pretest scores are provided, boostrapped CI on beta is also given.
#'
#' @param y the raw posttest scores of a continuous outcome variable.
#' @param g the categorical variable that denotes the group membership.
#' @param x (optional) the raw pretest scores of a continuous outcome variable.
#' @param nrep the number of boostrapped samples. (default=1000)
#' @param alpha the significance level (default=.05)
#' @return a table of lower and upper limit from bias-corrected and accelerated and expanded percentile boostrapped
#' confidence interval. The first row is on the geometric mean of the control group
#' (default group of comparison). After that, zeta estimates are given of the each respective group versus
#' the control group (default group of comparison). Then, if pretest scores are given, CI on the beta
#' estimate is given. Lastly, CI on the signal-to-noise ratio, an overall effect size measure, is provided.
#'         \item{BCa LL}{the lower limit of the Bias-Corrected and accelerated boostrapped Confidence Interval}
#'         \item{BCa UL}{the upper limit of the Bias-Corrected and accelerated boostrapped Confidence Interval}
#'         \item{exp LL}{the lower limit of the expanded percentile boostrapped Confidence Interval}
#'         \item{exp UL}{the upper limit of the expanded percentile boostrapped Confidence Interval}
#' @examples
#' data("schoene")
#' attach(schoene)
#' boot.es(post_HRT,group,pre_HRT,1000,.05)
#' @export
#' @importFrom stats lm qnorm pnorm quantile qt var
#' @references Efron, B. (1987). "Better Bootstrap Confidence Intervals". Journal of the American Statistical Association. Journal of the American Statistical Association, Vol. 82, No. 397. 82 (397): 171–185. doi:10.2307/2289144. JSTOR 2289144.

boot.es<-function(y,g,x=0,nrep=1000,alpha=.05){
  n=length(y)
  iboot=function(){
    ind=sample(1:n,n,replace=T);
    if (sum(x)!=0) {
      m=lm(log(y[ind])~factor(g[ind])+log(x[ind]))} else {
        m=lm(log(y[ind])~factor(g[ind]))}
    return(c(m$coefficients,var(m$fitted.values)/var(log(y[ind]))/(1-var(m$fitted.values)/var(log(y[ind])))))
  }
  bes=replicate(nrep,iboot())
  alphaD2=pnorm(sqrt(n/(n-1))*qt(alpha/2,df=n-1))
  ep=apply(bes,1,function(x) quantile(x,probs=c(alphaD2,1-alphaD2)))
  ijack=function(j){
    if (sum(x)!=0) {
      m=lm(log(y[-j])~factor(g[-j])+log(x[-j]))} else {
        m=lm(log(y[-j])~factor(g[-j]))}
    return(c(m$coefficients,var(m$fitted.values)/var(log(y[-j]))/(1-var(m$fitted.values)/var(log(y[-j])))))
  }
  jes=sapply(1:n,ijack)
  if (sum(x)!=0) {
    m=lm(log(y)~factor(g)+log(x))} else {
      m=lm(log(y)~factor(g))}
  theta_h=c(m$coefficients,var(m$fitted.values)/var(log(y))/(1-var(m$fitted.values)/var(log(y))))
  z0h=qnorm(rowSums(bes<theta_h)/1000)
  ah=-rowSums((jes-rowMeans(jes))^3)/(6*rowSums((jes-rowMeans(jes))^2)^1.5)
  alpha_1=pnorm(z0h+(z0h+qnorm(alpha/2))/(1-ah*(z0h+qnorm(alpha/2))))
  alpha_2=pnorm(z0h+(z0h+qnorm(1-alpha/2))/(1-ah*(z0h+qnorm(1-alpha/2))))
  sorted=apply(bes,1,sort)
  bca.ll=sapply(1:4,function(i) sorted[alpha_1[i]*1000,i])
  bca.ul=sapply(1:4,function(i) sorted[alpha_2[i]*1000,i])
  tbl=cbind(bca.ll,bca.ul,t(ep))
  rownames(tbl)=c(levels(factor(g)),'x','sig2nois ratio')
  colnames(tbl)=c('BCa LL','BCa UL',"exp perc LL","exp perc UL")
  tbl[1:length(levels(factor(g))),]=exp(tbl[1:length(levels(factor(g))),])
  tbl
}
