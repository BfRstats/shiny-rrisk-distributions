# Mit rrisk-distributions-definitions zusammenführen

#---BEGIN: dist fit info--------------------------------------------------------
dist_info_list <- list(
  "cauchy" = list(
    display_name = "cauchy",
    type         = "continuous",
    range        = c(min = -Inf, max = Inf),
    pdf_func     = "dcauchy",
    cdf_func     = "pcauchy",
    start_values = function(x) {
      c(location = median(x),
        scale = mean(abs(x - median(x))))
    },
    lower        = c(location = -Inf, scale = 1e-6),
    upper        = c(location = Inf,  scale = Inf)
  ),
  "exponential" = list(
    display_name = "exponential",
    type         = "continuous",
    range        = c(min = 0, max = Inf),
    pdf_func     = "dexp",
    cdf_func     = "pexp",
    start_values = function(x) {
      c(rate = 1/mean(x))
    },
    lower        = c(rate = 1e-6),
    upper        = c(rate = Inf)
  ),
  "lognormal" = list(
    display_name = "log-normal",
    type         = "continuous",
    range        = c(min = 0, max = Inf),
    pdf_func     = "dlnorm",
    cdf_func     = "plnorm",
    start_values = function(x) {
      c(meanlog = mean(log(x)),
        sdlog   = sd(log(x)))
    },
    lower        = c(meanlog = -Inf, sdlog = 0),
    upper        = c(meanlog = Inf,  sdlog = Inf)
  ),
  "gaussian" = list(
    display_name = "normal (gaussian)",
    type         = "continuous",
    range        = c(min = -Inf, max = Inf),
    pdf_func     = "dnorm",
    cdf_func     = "pnorm",
    start_values = function(x) {
      c(mean = mean(x),
        sd   = sd(x))
    },
    lower        = c(mean = -Inf, sd = 0),
    upper        = c(mean = Inf,  sd = Inf)
  ),
  "gamma" = list(
    display_name = "gamma",
    type         = "continuous",
    range        = c(min = 0, max = Inf),
    pdf_func     = "dgamma",
    cdf_func     = "pgamma",
    start_values = function(x) {
      c(shape = mean(x),
        rate = 1)
    },
    lower        = c(shape = 1e-6, rate = 1e-6),
    upper        = c(shape = Inf,  rate = Inf)
  ),
  "invgamma" = list(
    display_name = "inverse-gamma",
    type         = "continuous",
    range        = c(min = 0, max = Inf),
    pdf_func     = "rrisk_dinvgamma",
    cdf_func     = "rrisk_pinvgamma",
    start_values = function(x) {
      c(shape = 2,
        rate  = mean(x))
    },
    lower        = c(shape = 1e-6, scale = 1e-6),
    upper        = c(shape = Inf,  scale = Inf)
  ),
  "weibull" = list(
    display_name = "weibull",
    type         = "continuous",
    range        = c(min = 0, max = Inf),
    pdf_func     = "dweibull",
    cdf_func     = "pweibull",
    start_values = function(x) {
      c(shape = 1,
        scale = mean(x))
    },
    lower        = c(shape = 1e-6, scale = 0),
    upper        = c(shape = Inf,  scale = Inf)
  ),
  "pert" = list(
    display_name = "modified PERT",
    type         = "continuous",
    range        = c(min = -Inf, max = Inf),
    pdf_func     = "rrisk_dmodpert",
    cdf_func     = "rrisk_pmodpert",
    start_values = function(x) {
      c(min   = min(x) - 0.1,
        mode  = median(x),
        max   = max(x) + 0.1,
        shape = 4)
    },
    lower       = c(min = -Inf, mode = -Inf, max = -Inf, shape = 1e-6),
    upper       = c(min = Inf,  mode = Inf,  max = Inf,  shape = Inf)
  ),
  "triang" = list(
    display_name = "triangular",
    type         = "continuous",
    range        = c(min = -Inf, max = Inf),
    pdf_func     = "rrisk_dtriang",
    cdf_func     = "rrisk_ptriang",
    start_values = function(x) {
      c(min   = min(x) - 0.1,
        mode  = median(x),
        max   = max(x) + 0.1)
    },
    lower        = c(min = -Inf, mode = -Inf, max = -Inf),
    upper        = c(min = Inf,  mode = Inf,  max = Inf)
  )
)
#---END: dist fit info----------------------------------------------------------