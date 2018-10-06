#' Logged ANCOVA
#'
#' Mathematically, LANCOVA is the ANCOVA form of
#' a log-log model where both the dependent variable and the covariate
#' is log-transformed.
#' LANCOVA can test and estimate multiplicative effects.
#'
#' @param y the raw posttest scores of a continuous outcome variable.
#' @param x the raw pretest scores of a continuous outcome variable.
#' @param g the categorical variable that denotes the group membership
#' @param plot a TRUE/FALSE variable that denotes if diagnostic plots are desired. (default=F)
#' @return An summary object of the LANCOVA results.
#' In residuals, the summary statistics are of sample multiplicative errors.
#' In the coefficients table, the estimate of the intercept is the (control group) geometric mean estimate.
#' The estimate for the pretest scores is the power parameter beta's estimate.
#' Other coefficient estimates are effect size measure zeta's estimates. The standard error is on the logged scale.
#' The confidence intervals are of significance level = .05 for the control group geometric mean and for the zeta estimates,
#' respectively, of the intercept and other coefficients
#' The residual standard error is that of the logged scale residuals. Both R-squared and Adjusted R-squared are computed
#' on the logged model. If `plot=TRUE`, diagnostic plots are provided.
#' @examples
#' data("schoene")
#' attach(schoene)
#' lancova(post_HRT,group,pre_HRT)
#' @export
#' @importFrom stats lm confint.lm

lancova<-function(y,g,x,plot=F){
  if (sum(y<=0|x<=0)>1) warning("all values of the dependent variable needs to be positive-valued")
  mod=lm((log(y)~log(x)+factor(g)))
  if (plot==T) plot(mod)
  s=summary(mod)
  s$coefficients[-2,1]=exp(s$coefficients[-2,1])
  colnames(s$coefficients)[2]="(log-scale) s.e."
  rownames(s$coefficients)[2]="x"
  s$coefficients=cbind(s$coefficients,exp(confint.lm(mod)))
  s$coefficients[2,c(5,6)]=log(s$coefficients[2,c(5,6)])
  s$residuals=exp(s$residuals)
  s
}


