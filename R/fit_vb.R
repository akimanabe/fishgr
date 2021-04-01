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

#' Fit vb via maximum likelihood method
#'
#' @param dat tibble with column `c(Age, Length)`
#' @param ps parameters in order of `c(L_inf, K_1, K_2, t_m, t_0)`
#'
#' @return tibble of estimated parameters
#' @export
#'
#' @examples
#' \dontrun{
#' foo <- tibble::tibble(Age = seq(0, 5, 0.1),
#' Length = ext_vb(age = seq(0, 5, 0.1),
#' ps = c(300, 0.6, 0.4, 3, 0)))
#' fit_ext_vb(foo, ps = c(250, 0.5, 1, 2, 0))
#' }
fit_ext_vb <-
  function(dat, ps) {

    log_likeli <- function(ps) {
      dat %>%
        dplyr::mutate(loglikeli = dnorm(Length, ext_vb(Age, ps = ps), log = TRUE),
                      loglikeli = - loglikeli) %>%
        dplyr::pull(loglikeli) %>%
        sum()
    }

    optim(ps, log_likeli, method = "L-BFGS-B", lower = 0, upper = Inf) %>%
      broom::tidy() %>%
      dplyr::mutate(parameter = dplyr::recode(parameter,
                                              parameter1 = "L_inf",
                                              parameter2 = "K_1",
                                              parameter3 = "K_2",
                                              parameter4 = "t_m",
                                              parameter5 = "t_0"
                                              ))

  }
