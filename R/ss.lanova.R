#' Sample Size Planning for LANOVA
#'
#' Compute the required per-group sample size for the LANOVA test.
#'
#' @param k the number of groups.
#' @param r_sqrd the expected explained variance (on the logged scale)
#' @param power the desired statistical power (default=.8)
#' @param alpha the significance level (default=.05)
#' @return \item{n}{the per-group sample size requirement}
#' @examples
#' ss.lanova(3,.01,.14,.05)
#' @export
#' @importFrom stats pf qf uniroot
#' @references Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.

ss.lanova<-function(k,r_sqrd,power=.8,alpha=.05){
  p.body <- quote({
    lambda <- k * n * r_sqrd*(1-r_sqrd)
    pf(qf(alpha, k - 1, (n - 1) * k, lower.tail = FALSE),
       k - 1, (n - 1) * k, lambda, lower.tail = FALSE)
  })
  n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
  list('the number of group' = k, 'proportion of variance to be explained' = r_sqrd, 'significance level' = alpha, 'desired power' = power, 'the per-group sample size requirement' = n)
}
