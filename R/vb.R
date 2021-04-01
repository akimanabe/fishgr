#' von Bertalanffy growth function
#'
#' @param age numerical age
#' @param ps vectorized parameters in order of c(Linf, K, t0)
#'
#' @return numerical or vector
#' @export
#'
#' @examples
#' \dontrun{
#' vb(1, c(300, 0.6, 0))
#' }
vb <- function(age, ps) {
  ps[1] * (1 - exp(- ps[2] * (age - ps[3])))
}
