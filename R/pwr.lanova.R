#' Power Calculation for LANOVA
#'
#' Compute the statistical power of the LANOVA test.
#'
#' @param k the number of groups.
#' @param n the number of observations per group.
#' @param r_sqrd the expected explained variance (on the logged scale)
#' @param alpha the significance level (default=.05)
#' @return \item{power}{the statistical power of test}
#' @examples
#' pwr.lanova(3,40,.4,.05)
#' @export
#' @importFrom stats pf qf
#' @references Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.





pwr.lanova<-function(k,n,r_sqrd,alpha=.05){
  lambda <- k * n * r_sqrd*(1-r_sqrd)
  power <- pf(qf(alpha, k - 1, (n - 1) * k, lower.tail = FALSE),
       k - 1, (n - 1) * k, lambda, lower.tail = FALSE)
  list('the number of group' = k, 'the per-group sample size' = n, 'r squared' = r_sqrd, 'significance level' = alpha, 'power' = power)
}
