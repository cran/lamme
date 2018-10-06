#' Sample Size Planning for LANCOVA
#'
#' Compute the required per-group sample size for the LANCOVA test.
#'
#' @param k the number of groups.
#' @param r_sqrd the expected explained variance by the model (on the logged scale)
#' @param rho_sqrd the pretest-posttest correlation
#' @param power the desired statistical power (default=.8)
#' @param alpha the significance level (default=.05)
#' @return \item{n}{the per-group sample size requirement}
#' @examples
#' ss.lancova(3,.5,.01,.14,.05)
#' @export
#' @importFrom stats pf qf uniroot
#' @references Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.



ss.lancova<-function(k,rho_sqrd,r_sqrd,power=.8,alpha=.05){
  p.body <- quote({
    lambda <- k * n * r_sqrd*(1-r_sqrd)/(1-rho_sqrd)
    pf(qf(alpha, k - 1, (n - 1) * k-1, lower.tail = FALSE),
       k - 1, (n - 1) * k-1, lambda, lower.tail = FALSE)
  })
  n <- uniroot(function(n) eval(p.body) - power, c(2 + 1e-10, 1e+09))$root
  list('the number of group' = k,
       'pretest-posttest correlation'= rho_sqrd,
       'proportion of variance to be explained' = r_sqrd,
       'significance level' = alpha,
       'desired power' = power,
       'the per-group sample size requirement' = n)
}
