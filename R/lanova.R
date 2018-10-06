#' Logged ANOVA
#'
#' Mathematically, LANOVA is the ANOVA form of
#' a log-log model where the dependent variable is log-transformed.
#' LANOVA can test and estimate multiplicative effects.
#'
#' @param y the raw scores of a continuous outcome variable.
#' @param g a categorical variable that denotes the group membership.
#' @param plot a TRUE/FALSE variable that denotes if diagnostic plots are desired. (default=F)
#' @return An summary object of the LANOVA results.
#' In residuals, the summary statistics are of sample multiplicative errors.
#' In the coefficients table, the estimate of the intercept is the default group (control group) geometric mean estimate.
#' Other coefficient estimates are effect size measure zeta's estimates. The standard error is on the logged scale.
#' The confidence intervals are of significance level = .05 for the control group geometric mean and for the zeta estimates,
#' respectively, of the intercept and other coefficients
#' The residual standard error is that of the logged scale residuals. Both R-squared and Adjusted R-squared are computed
#' on the logged model. If `plot=TRUE`, diagnostic plots are provided.
#' @examples
#' # generate data
#' y1=rnorm(1000,5,1)+rnorm(1000)
#' y2=rnorm(1000,5.5,1)+rnorm(1000)
#' y3=rnorm(1000,6,1)+rnorm(1000)
#' y1=exp(y1)
#' y2=exp(y2)
#' y3=exp(y3)
#' dep=c(y1,y2,y3)
#' tc=rep(c(0,1,2),each=1000)
#' # applying lanova with the generated data
#' lanova(dep,tc)
#' @export
#' @importFrom stats lm confint.lm


lanova<-function(y,g,plot=F){
  if (sum(y<=0)>1) warning("all values of the dependent variable needs to be positive-valued")
  mod=lm((log(y)~factor(g)))
  if (plot==T) plot(mod)
  s=summary(mod)
  s$coefficients[,1]=exp(s$coefficients[,1])
  colnames(s$coefficients)[2]="(log-scale) s.e."
  s$coefficients=cbind(s$coefficients,exp(confint.lm(mod)))
  s$residuals=exp(s$residuals)
  s
}


