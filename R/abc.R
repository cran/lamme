#' the ABC procedure for model selection
#'
#' the AIC comparison with Modified Box-Cox Transformation (ABC)
#' is a diagnostic procedure to help select among various additive
#' and multiplicative models
#'
#' When only `y` and `g` are specified, the ABC procedure compares
#' LANOVA and ANOVA models. When `x` is also specified, the ABC procedure
#' compares LANCOVA, ANCOVA, ANCOHET, and ANCOVA with log-transformed y.
#'
#' @param y the raw posttest scores of a continuous outcome variable.
#' @param x (optional) the raw pretest scores of a continuous outcome variable.
#' @param g the categorical variable that denotes the group membership.
#' @return AIC results of different models. The model with smallest AIC is preferred.
#' @examples
#' data("schoene")
#' attach(schoene)
#' abc(post_HRT,group,pre_HRT)
#' abc(post_HRT,group)
#' @export
#' @importFrom stats lm AIC


abc <- function(y,g,x=0){
    y.m=prod(y)^(1/length(y))*log(y)
    if (sum(x)!=0) {
      m=list('ancova'=lm(y~factor(g)+x), #ancova
             'ancohet'=lm(y~factor(g)+x+x*factor(g)),#ancohet
             'ancova-l'=lm(y.m~factor(g)+x), #ancova-l
             'lancova'=lm(y.m~factor(g)+log(x)) #lancova
      )
    } else {
    m=list('anova'=lm(y~factor(g)),'lanova'=lm(y.m~factor(g)))
    }
    lapply(m,AIC)
}


