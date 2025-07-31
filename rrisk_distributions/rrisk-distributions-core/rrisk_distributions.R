# extra distribution for the model
rrisk_runif <- function(n, min = 0, max = 1, type = "MC")
{
  if (n == 1 || type == "MC")
    stats::runif(n, min, max)
  else if (type == "LHS") {
    half_step_size <- (max-min)/(2*n)
    sample(seq(min + half_step_size, 
               max - half_step_size, 
               length.out = n))
  }
}

rrisk_rbinom <- function(n, size, prob, type = "MC")
{
  stats::qbinom(p    = rrisk_runif(n, type = type),
                size = size,
                prob = prob)
}

rrisk_rnbinom <- function(n, size, prob, mu = NULL, type = "MC")
{
  if (is.null(mu))
    stats::qnbinom(p    = rrisk_runif(n, type = type),
                   size = size,
                   prob = prob)
  else
    stats::qnbinom(p    = rrisk_runif(n, type = type),
                   size = size,
                   mu   = mu)
}

rrisk_rpois <- function(n, lambda, type = "MC")
{
  stats::qpois(p      = rrisk_runif(n, type = type),
               lambda = lambda)
}

# # optimized version for rdiscrete
# rrisk_rdiscrete <- function(n, x, prob, type = "MC")
# {
#   p <- cumsum(prob)
#   r <- rrisk_runif(n, type = type)
#   result <- rep(0, length(n))
#   for (i in seq_along(r)) {
#     for (j in seq_along(p)) {
#       if (r[i] < p[j]) {
#         result[i] <- x[j]
#         break
#       }
#     }
#   }
#   result
# }
rrisk_rdiscrete <- function(n, x, prob, ...)
{
  sample(x = x, size = n, replace = TRUE, prob = prob)
}

rrisk_rmultinom <- function(n, probs, type = "MC")
{
  cumprob <- cumsum(c(0,probs))
  z <- rep(0, n)
  i <- 0
  for (p in rrisk_runif(n, type = type)) {
    tmp <- which(cumprob < p)
    i <- i + 1
    z[i] <- tmp[length(tmp)]
  }
  z
}

rrisk_rbeta <- function(n, shape1, shape2, type = "MC")
{
  stats::qbeta(p      = rrisk_runif(n, type = type),
               shape1 = shape1,
               shape2 = shape2)
}

rrisk_rweibull <- function(n, shape, scale = 1, lower = 0, upper = Inf,
                           type = "MC") 
{
  min_p <- stats::pweibull(lower, shape = shape, scale = scale)
  max_p <- stats::pweibull(upper, shape = shape, scale = scale)
  stats::qweibull(p     = rrisk_runif(n, min_p, max_p, type = type),
                  shape = shape,
                  scale = scale)
}

rrisk_rnorm <- function(n, mean, sd, lower = -Inf, upper = Inf,
                        type = "MC") 
{
  min_p <- stats::pnorm(lower, mean = mean, sd = sd)
  max_p <- stats::pnorm(upper, mean = mean, sd = sd)
  stats::qnorm(p    = rrisk_runif(n, min_p, max_p, type = type),
               mean = mean,
               sd   = sd)
}

rrisk_rlnorm <- function(n, meanlog = 0, sdlog = 1, lower = 0, upper = Inf,
                         type = "MC") 
{
  min_p <- stats::plnorm(lower, meanlog = meanlog, sdlog = sdlog)
  max_p <- stats::plnorm(upper, meanlog = meanlog, sdlog = sdlog)
  stats::qlnorm(p       = rrisk_runif(n, min_p, max_p, type = type),
                meanlog = meanlog,
                sdlog   = sdlog)
}

rrisk_rgamma <- function(n, shape = 1, rate = 1, lower = 0, upper = Inf,
                         type = "MC") 
{
  min_p <- stats::pgamma(lower, shape = shape, rate = rate)
  max_p <- stats::pgamma(upper, shape = shape, rate = rate)
  stats::qgamma(p     = rrisk_runif(n, min_p, max_p, type = type),
                shape = shape,
                rate  = rate)
}

#---BEGIN: inverted gamma-------------------------------------------------------
# random number generator
rrisk_rinvgamma <- function(n, shape, rate, lower = 0, upper = Inf, 
                            type = "MC") 
{
  # it is correct: min_p needs upper, max_p needs lower
  min_p <- stats::pgamma(rate/upper, shape)
  max_p <- stats::pgamma(rate/lower, shape)
  1/stats::qgamma(p     = rrisk_runif(n, min_p, max_p, type = type),
                  shape = shape,
                  rate  = rate)
}

# pdf
rrisk_dinvgamma <- function(x, shape, rate)
{
  ifelse(test = x > 0,
         yes  = x^(-shape - 1) * exp(-rate/x) * rate^shape / gamma(shape),
         no   = 0)
}

# cdf
rrisk_pinvgamma <- function(q, shape, rate)
{
  ifelse(test = q > 0, 
         yes  = stats::pgamma(rate/q, shape, lower.tail = FALSE), 
         no   = 0)
}
#---END: inverted gamma---------------------------------------------------------

rrisk_rexp <- function(n, rate, lower = 0, upper = Inf, type = "MC") 
{
  min_p <- stats::pexp(lower, rate = rate)
  max_p <- stats::pexp(upper, rate = rate)
  stats::qexp(p    = rrisk_runif(n, min_p, max_p, type = type),
              rate = rate)
}

#---BEGIN: modified PERT--------------------------------------------------------
# random number generator
rrisk_rmodpert <- function(n, min, mode, max, shape = 4,
                           type = "MC") 
{
  alpha <- 1 + shape * (mode - min) / (max - min)
  beta  <- 1 + shape * (max - mode) / (max - min)
  q <- stats::qbeta(p      = rrisk_runif(n, type = type),
                    shape1 = alpha,
                    shape2 = beta)
  q * (max - min) + min
}

# pdf
rrisk_dmodpert <- function(x, min, mode, max, shape = 4)
{
  a <- 1 + shape * (mode - min) / (max - min)
  b <- 1 + shape * (max - mode) / (max - min)
  ifelse(test = x >= min & x <= max, 
         yes  = ((x - min)^(a-1)*(max - x)^(b-1))/(beta(a, b)*(max - min)^(a+b-1)),
         no   = 0)
  
  #alpha <- 1 + shape * (mode - min) / (max - min)
  #beta  <- 1 + shape * (max - mode) / (max - min)
  #z <- (x - min)/(max - min)
  #d <- stats::dbeta(x = z, shape1 = alpha, shape2 = beta)
  #d / (max - min)^(alpha + beta - 1)
}

# cdf
rrisk_pmodpert <- function(q, min, mode, max, shape = 4)
{
  alpha <- 1 + shape * (mode - min) / (max - min)
  beta  <- 1 + shape * (max - mode) / (max - min)
  z <- (q - min)/(max - min)
  stats::pbeta(q = z, shape1 = alpha, shape2 = beta)
}
#---END: modified PERT----------------------------------------------------------

#---BEGIN: shifted log-logistic-------------------------------------------------
# shifted log-logistic distribution
# location range: -Inf < location < Inf
# alpha range: 0 < alpha < Inf
# beta range: 0 < beta < Inf
rrisk_pll3 <- function(q, location, alpha, beta)
{
  ifelse(test = q > location,
         yes  = 1 / (1 + (beta / (q - location))^alpha),
         no   = 0)
}

rrisk_dll3 <- function(x, location, alpha, beta, 
                       lower = location, upper = Inf)
{
  z <- (x - location) / beta
  p_min <- rrisk_pll3(lower, location, alpha, beta)
  p_max <- rrisk_pll3(upper, location, alpha, beta)
  ifelse(test = x > location & x > lower & x < upper,
         yes  = ((alpha/beta)*z^(alpha - 1) / (1 + z^alpha)^2) / (p_max - p_min),
         no   = 0)
}

rrisk_qll3 <- function(p, location, alpha, beta)
{
  location + beta * exp(log(p/(1-p))/alpha)
}

rrisk_rll3 <- function(n, location, alpha, beta, 
                       lower = location, upper = Inf,
                       type = "MC") 
{
  p_min <- rrisk_pll3(lower, location, alpha, beta)
  p_max <- rrisk_pll3(upper, location, alpha, beta)
  rrisk_qll3(p        = rrisk_runif(n, p_min, p_max, type = type),
             location = location,
             alpha    = alpha,
             beta     = beta)
}
#---END: shifted log-logistic---------------------------------------------------

#---BEGIN: triangular distribution----------------------------------------------
rrisk_dtriang <- function(x, min, mode, max)
{
  vapply(
    X   = x,
    FUN = function(xx, min, mode, max)
    {
      if (xx < min)
        0
      else if (xx >= min && xx < mode)
        2*(xx - min) / ((max - min)*(mode - min))
      else if (xx == mode)
        2/(max - min)
      else if (xx > mode && xx <= max)
        2*(max - xx) / ((max - min)*(max - mode))
      else
        0
    },
    FUN.VALUE = numeric(1),
    min, mode, max,
    USE.NAMES = FALSE
  )
}

rrisk_ptriang <- function(q, min, mode, max)
{
  vapply(
    X   = q,
    FUN = function(xx, min, mode, max)
    {
      if (xx <= min)
        0
      else if (xx > min && xx <= mode)
        (xx - min)^2 / ((max - min)*(mode - min))
      else if (xx > mode && xx < max)
        1 - (max - xx)^2 / ((max - min)*(max - mode))
      else
        1
    },
    FUN.VALUE = numeric(1),
    min, mode, max,
    USE.NAMES = FALSE
  )
}

rrisk_rtriang <- function(n, min, mode, max, lower = min, upper = max,
                          type = "MC") 
{
  diff_a  <- max - min
  diff_b  <- mode - min
  diff_bb <- diff_a * diff_b
  diff_c  <- diff_a * (max - mode)
  
  # get min and max allowed p values, needed for trimmed distribution
  min_p <- ifelse(test = diff_bb != 0, 
                  yes  = (lower - min)^2 / diff_bb,
                  no   = 0)
  max_p <- ifelse(test = diff_c  != 0, 
                  yes  = 1 - (max - upper)^2 / diff_c,
                  no   = 1)
  
  # compute triangular distribution
  p <- rrisk_runif(n, min_p, max_p, type = type)
  break_point <- diff_b / diff_a
  ifelse(test = p <= break_point,
         yes  = min + sqrt(p * diff_bb),
         no   = max - sqrt((1 - p) * diff_c))
}
#---END: triangular distribution------------------------------------------------

# rrisk_rcumulative <- function(n, full_x, full_p, type = "MC") {
#   vapply(X   = rrisk_runif(n, type = type),
#          FUN = function(this_p, full_x, full_p) {
#            i <- which(full_p < this_p)
#            i <- i[length(i)]
#            m <- ((full_p[i+1] - full_p[i])/(full_x[i+1] - full_x[i]))
#            (this_p - full_p[i]) / m + full_x[i]
#          },
#          FUN.VALUE = numeric(1),
#          full_x, full_p,
#          USE.NAMES = FALSE)
# }

# it is faster without vapply
rrisk_rcumulative <- function(n, full_x, full_p, 
                              smooth = FALSE, type = "MC") 
{
  if (smooth) {
    cdf <- stats::splinefun(x      = full_x,
                            y      = full_p,
                            method = "monoH.FC")
    f <- function(x, a) cdf(x) - a
    search_interval <- range(full_x)
    z <- rep(NaN, n)
    runif_sample <- rrisk_runif(n, type = type)
    for (i in seq_len(n)) {
      z[i] <- stats::uniroot(f, 
                             interval = search_interval, 
                             a        = runif_sample[i],
                             f.lower  = -runif_sample[i], 
                             f.upper  = 1 - runif_sample[i])$root
    }
  } else {
    z <- rep(NaN, n)
    j <- 0
    for (p in rrisk_runif(n, type = type)) {
      tmp <- which(full_p < p)
      i <- tmp[length(tmp)]
      m <- ((full_p[i+1] - full_p[i])/(full_x[i+1] - full_x[i]))
      j <- j + 1
      z[j] <- (p - full_p[i]) / m + full_x[i]
    }
  }
  z
}

#---BEGIN: general--------------------------------------------------------------
# expand x and p
# full_x <- c(min, x, max)
# full_p <- c(0, rel_p, 0)
# it is faster without vapply
rrisk_rgeneral <- function(n, full_x, full_p, type = "MC") 
{
  # compute Area of pdf for each segment
  n_x <- length(full_x) - 1
  A_i <- rep(0, n_x)
  for (i in seq_len(n_x)) {
    A_i[i] <- 0.5 * (full_x[i+1] - full_x[i]) * (full_p[i+1] + full_p[i])
  }

  # full area
  A <- sum(A_i)
  # re-norm full_p
  full_p <- full_p / A
  # re-norm A_i; get cumulative sum, with first element is zero
  F_x <- c(0, cumsum(A_i / A))
  
  z <- rep(0, n)
  j <- 0
  for (p in rrisk_runif(n, type = type)) {
    tmp <- which(p >= F_x)
    i <- tmp[length(tmp)]
    m <- (full_p[i+1] - full_p[i])/(full_x[i+1] - full_x[i])
    if (m == 0)
      dx <- (p - F_x[i]) / full_p[i]
    else
      dx <- (sqrt(full_p[i]^2 + 2 * m * (p - F_x[i])) - full_p[i]) / m
    j <- j + 1
    z[j] <- full_x[i] + dx
  }
  z
}

rrisk_dgeneral <- function(full_x, full_p)
{
  # compute Area of each segment
  n_x <- length(full_x) - 1
  A_i <- rep(0, n_x)
  for (i in seq_len(n_x)) {
    A_i[i] <- 0.5 * (full_x[i+1] - full_x[i]) * (full_p[i+1] + full_p[i])
  }

  # get full area
  A <- sum(A_i)
  
  # re-norm full_p
  full_p / A
}
# rrisk_rgeneral <- function(n, full_x, full_p, type = "MC") 
# {
#   # expand x and p
#   #full_x <- c(min, x, max)
#   #full_p <- c(0, rel_p, 0)
#   # compute Area of pdf for each segement
#   A_i <- vapply(X         = seq_len(length(full_x)-1),
#                 FUN       = function(i, x, p) 
#                               0.5 * (x[i+1] - x[i]) * (p[i+1] + p[i]),
#                 FUN.VALUE = numeric(1),
#                 full_x, full_p,
#                 USE.NAMES = FALSE)
#   # full area
#   A <- sum(A_i)
#   # renorm p
#   full_p <- full_p / A
#   # renorm A_i
#   A_i <- A_i / A
#   # get cumulative sum, with first element is zero
#   cumsum_A_i <- c(0, cumsum(A_i))
#   # set the function for computing random values from the general dist
#   qgeneral <- function(this_p, x, p, F_x) {
#     i <- which(this_p >= F_x)
#     i <- i[length(i)]
#     m <- (p[i+1] - p[i])/(x[i+1] - x[i])
#     if (m == 0)
#       dx <- (this_p - F_x[i]) / p[i]
#     else
#       dx <- (sqrt(p[i]^2 + 2 * m * (this_p - F_x[i])) - p[i]) / m
#     x[i] + dx
#   }
#   # get random values from general distribution
#   vapply(X         = rrisk_runif(n, type = type), 
#          FUN       = qgeneral,
#          FUN.VALUE = numeric(1),
#          full_x, full_p, cumsum_A_i,
#          USE.NAMES = FALSE)
# }
#---END: general----------------------------------------------------------------

rrisk_rsigma_dist <- function(n, sd, nn, type = "MC") 
{
  random_values <- stats::qchisq(p  = rrisk_runif(n, type = type),
                                 df = nn)
  sd * sqrt(nn/random_values)
}

rrisk_yule_furry_process <- function(n, N0, rate, time, type = "MC")
{
  stats::qnbinom(p    = rrisk_runif(n, type = type),
                 size = N0,
                 prob = exp(-rate*time)) + N0
}
  
  
  
  