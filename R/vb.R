#' von Bertalanffy growth function
#'
#' @param age numerical age
#' @param ps vectorized parameters in order of c(Linf, K, t0)
#' @param setorigin constrain t_0 to 0 when TRUE
#'
#' @return numerical or vector
#' @export
#'
#' @examples
#' \dontrun{
#' vb(1, c(300, 0.6, 0), setorigin = FALSE)
#' }
vb <- function(age, ps, setorigin = FALSE) {
  if (setorigin == FALSE) {
    estim_length <-
      ps[1] * (1 - exp(- ps[2] * (age - ps[3])))
  }

  if (setorigin == TRUE) {
    estim_length <-
      ps[1] * (1 - exp(- ps[2] * (age - 0)))
  }

  return(estim_length)

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
