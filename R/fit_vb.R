#' Fit vb via maximum likelihood method
#'
#' @param dat tibble with column `c(Age, Length)`
#' @param ps parameters in order of `c(L_inf, K, t_0)`
#'
#' @return tibble of estimated parameters
#' @export
#'
#' @examples
#' \dontrun{
#' foo <- tibble::tibble(
#' Age = seq(0, 5, 0.1),
#' Length = vb(Age, c(300, 0.6, 0)))
#' fit_vb(dat = foo, ps = c(200, 0.8, 0))
#' }
fit_vb <-
  function(dat, ps) {

    log_likeli <- function(ps) {
      dat %>%
        dplyr::mutate(loglikeli = dnorm(Length, vb(Age, ps = ps), log = TRUE),
                      loglikeli = - loglikeli) %>%
        dplyr::pull(loglikeli) %>%
        sum()
    }

    optim(ps, log_likeli) %>%
      broom::tidy() %>%
      dplyr::mutate(parameter = dplyr::recode(parameter,
                                               parameter1 = "L_inf",
                                               parameter2 = "K",
                                               parameter3 = "t_0"))

}
