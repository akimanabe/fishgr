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

#' extended von Bertalanffy growth function
#'
#' @param age numerical age
#' @param ps vectorized parameters in order of c(Linf, K1, K2, tm, t0)
#'
#' @return numerical or vector
#' @export
#'
#' @examples
#' \dontrun{
#' ext_vb(seq(0,5, 0.5), ps = c(300, 0.6, 0.4, 2, 0))
#' }
ext_vb <- function(age, ps) {
  x <-
    ifelse(age < ps[4],
           - ps[2] * (age - ps[5]),
           - ps[2] * (ps[4] - ps[5]) - ps[3] * (age - ps[4]))

  ps[1] * (1 - exp(x))
}
