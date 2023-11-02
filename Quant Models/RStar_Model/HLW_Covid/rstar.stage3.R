#------------------------------------------------------------------------------#
# File:        rstar.stage3.R
#
# Description: This file runs the model in the third stage of the HLW estimation.
#------------------------------------------------------------------------------#
rstar.stage3 <- function(log.output,
                         inflation,
                         real.interest.rate,
                         nominal.interest.rate,
                         covid.dummy,
                         lambda.g,
                         lambda.z,
                         T.og0.omit,
                         a3.constraint=NA,
                         b2.constraint=NA,
                         run.se = TRUE,
                         sample.end) {

  stage <- 3

  #----------------------------------------------------------------------------#
  # Obtain initial parameter values
  #----------------------------------------------------------------------------#

  # Data must start 4 quarters before the estimation period
  T    <- length(log.output) - 4

  # Omit data during COVID period when estimating initial trend in output
  # See COVID note: using data through 19q4
  T.og <- T - T.og0.omit

  # Estimate log-linear trend in GDP through 19q4 (based on T.og ending in 19q4)
  # OLS: Bhat = (X'X)^{-1}X'Y
  x.og <- cbind(rep(1,(T+4)), 1:(T+4))
  y.og <- log.output
  b.og <- solve(t(x.og[1:(T.og+4),]) %*% x.og[1:(T.og+4),], t(x.og[1:(T.og+4),]) %*% y.og[1:(T.og+4)]) # (X'X)^{-1} (X'Y)

  # q(t): trend in output estimated through 19q4 and extended through current period
  # q = X*Bhat, where Bhat is estimated through 19q4
  q <- x.og %*% b.og

  # IS curve, estimated by nonlinear LS:
  # y(t) - q(t) = phi*d(t) + a_{y,1}*(y(t-1) - q(t-1) - phi*d(t-1)) + a_{y,2}(y(t-2) - q(t-2) - phi*d(t-2)) + a_r/2(r_{t-1} + r_{t-2}) eps(t)
  y.is     <- (log.output[5:(T+4)] - q[5:(T+4)]) * 100
  y.is.l1  <- (log.output[4:(T+3)] - q[4:(T+3)]) * 100
  y.is.l2  <- (log.output[3:(T+2)] - q[3:(T+2)]) * 100
  d        <- covid.dummy[5:(T+4)]
  d.l1     <- covid.dummy[4:(T+3)]
  d.l2     <- covid.dummy[3:(T+2)]
  ir.is    <- (real.interest.rate[4:(T+3)] + real.interest.rate[3:(T+2)])/2
  # If the start date is after 2020:Q1, run the model with COVID dummy included
  # Otherwise, initialize phi at zero
  if (T.og0.omit > 0) {
    nls.is   <- nls(y.is ~ phi*d + a_1*(y.is.l1 - phi*d.l1) + a_2*(y.is.l2 - phi*d.l2) +
                    a_r*ir.is + a_0*rep(1,T), start = list(phi=0,a_1=0,a_2=0,a_r=0,a_0=0))
    b.is     <- coef(nls.is)
  } else {
    nls.is   <- nls(y.is ~ a_1*(y.is.l1) + a_2*(y.is.l2) +
                      a_r*ir.is + a_0*rep(1,T), start = list(a_1=0,a_2=0,a_r=0,a_0=0))
    b.is     <- coef(nls.is)
    b.is["phi"] <- 0
  }
  r.is     <- y.is - predict(nls.is) # residuals
  s.is     <- sqrt(sum(r.is^2) / (length(r.is)-(length(b.is))))


  # Phillips curve
  # Estimate by LS, modified to include the covid dummy:
  # pi(t) = B(L)pi(t-1) + b_y*(y(t-1)-q(t-1)-phi*d(t-1)) + eps_pi(t)
  y.ph <- inflation[5:(T+4)]
  x.ph <- cbind(inflation[4:(T+3)],
                (inflation[3:(T+2)]+inflation[2:(T+1)]+inflation[1:T])/3,
                y.is.l1 - b.is["phi"]*d.l1)
  b.ph <- solve(t(x.ph) %*% x.ph, t(x.ph) %*% y.ph)
  r.ph <- y.ph - x.ph %*% b.ph
  s.ph <- sqrt(sum(r.ph^2) / (length(r.ph)-(length(b.ph))))

  # Starting values for the parameter vector
  initial.parameters <- c(b.is["a_1"], b.is["a_2"], b.is["a_r"], b.ph[1], b.ph[3], s.is, s.ph, 0.7, b.is["phi"]) # with guess for dummy


  # Initialization of state vector for Kalman filter using HP trend of log output
  log.output.hp.trend <- hpfilter(log.output,freq=36000,type="lambda",drift=FALSE)$trend
  g.pot <- log.output.hp.trend[(g.pot.start.index):length(log.output.hp.trend)]
  g.pot.diff <- diff(g.pot)
  xi.00 <- c(100*g.pot[3:1],100*g.pot.diff[2:1],0,0)


  #----------------------------------------------------------------------------#
  # Build data matrices
  #----------------------------------------------------------------------------#
  y.data <- cbind(100 * log.output[5:(T+4)],
                  inflation[5:(T+4)])
  x.data <- cbind(100 * log.output[4:(T+3)],
                  100 * log.output[3:(T+2)],
                  real.interest.rate[4:(T+3)],
                  real.interest.rate[3:(T+2)],
                  inflation[4:(T+3)],
                  (inflation[3:(T+2)]+inflation[2:(T+1)]+inflation[1:T])/3,
                  covid.dummy[5:(T+4)],
                  covid.dummy[4:(T+3)],
                  covid.dummy[3:(T+2)]) # covid dummy and 2 lags


  #----------------------------------------------------------------------------#
  # Estimate parameters using maximum likelihood
  #----------------------------------------------------------------------------#

  # Set an upper and lower bound on the parameter vectors:
  # The vector is unbounded unless values are otherwise specified
  theta.lb <- c(rep(-Inf,length(initial.parameters)))
  theta.ub <- c(rep(Inf,length(initial.parameters)))

  # Set a lower bound for the Phillips curve slope (b_2) of b2.constraint, if not NA
  # In HLW, b2.constraint = 0.025
  if (!is.na(b2.constraint)) {
      print(paste0("Setting a lower bound on b_2 of ",as.character(b2.constraint)))
      if (initial.parameters[5] < b2.constraint) {
          initial.parameters[5] <- b2.constraint
      }
      theta.lb[5] <- b2.constraint
  }

  # Set an upper bound for the IS curve slope (a_3) of a3.constraint, if not NA
  # In HLW, a3.constraint = -0.0025
  if (!is.na(a3.constraint)) {
      print(paste0("Setting an upper bound on a_3 of ",as.character(a3.constraint)))
      if (initial.parameters[3] > a3.constraint) {
          initial.parameters[3] <- a3.constraint
      }
      theta.ub[3] <- a3.constraint
  }

  # Set the initial covariance matrix (see footnote 6)
  P.00 <- calculate.covariance(initial.parameters, theta.lb, theta.ub, y.data, x.data, stage, lambda.g, lambda.z, xi.00)

  # Get parameter estimates via maximum likelihood
  f <- function(theta) {return(-log.likelihood.wrapper(theta, y.data, x.data, stage, lambda.g, lambda.z, xi.00, P.00)$ll.cum)}
  nloptr.out <- nloptr(initial.parameters, f, eval_grad_f=function(x) {gradient(f, x)},
                       lb=theta.lb,ub=theta.ub,
                       opts=list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8))
  theta <- nloptr.out$solution

  log.likelihood <- log.likelihood.wrapper(theta, y.data, x.data, stage, lambda.g, lambda.z, xi.00, P.00)$ll.cum

  #----------------------------------------------------------------------------#
  # Kalman filtering and standard error calculation
  #----------------------------------------------------------------------------#

  # Get state vectors (xi.tt, xi.ttm1, xi.tT, P.tt, P.ttm1, P.tT) via Kalman filter
  states <- kalman.states.wrapper(theta, y.data, x.data, stage, lambda.g, lambda.z, xi.00, P.00)

  # If run.se = TRUE, compute standard errors for estimates of the states (see footnote 7) and report run time
  # Note: If sample end data is 2020:Q2 or later only, will report SE for phi.
  if (run.se) {
      ptm <- proc.time()
      se <- kalman.standard.errors(T, states, theta, y.data, x.data, stage, lambda.g, lambda.z, xi.00, P.00, niter, a3.constraint, b2.constraint, sample.end)
      print("Standard error procedure run time")
      print(proc.time() - ptm)
  }

  # One-sided (filtered) estimates
  trend.filtered      <- states$filtered$xi.tt[,4] * 4
  z.filtered          <- states$filtered$xi.tt[,6]
  rstar.filtered      <- trend.filtered + z.filtered
  potential.filtered  <- states$filtered$xi.tt[,1]/100
  output.gap.filtered <- y.data[,1] - (potential.filtered * 100) - theta[length(theta)]*covid.dummy[5:(T+4)] # with covid dummy

  # Two-sided (smoothed) estimates
  trend.smoothed      <- states$smoothed$xi.tT[,4] * 4
  z.smoothed          <- states$smoothed$xi.tT[,6]
  rstar.smoothed      <- trend.smoothed + z.smoothed
  potential.smoothed  <- states$smoothed$xi.tT[,1]/100
  output.gap.smoothed <- y.data[,1] - (potential.smoothed * 100) - theta[length(theta)]*covid.dummy[5:(T+4)] # with covid dummy

  # Save variables to return
  return.list <- list()
  return.list$rstar.filtered      <- rstar.filtered
  return.list$trend.filtered      <- trend.filtered
  return.list$z.filtered          <- z.filtered
  return.list$potential.filtered  <- potential.filtered
  return.list$output.gap.filtered <- output.gap.filtered
  return.list$rstar.smoothed      <- rstar.smoothed
  return.list$trend.smoothed      <- trend.smoothed
  return.list$z.smoothed          <- z.smoothed
  return.list$potential.smoothed  <- potential.smoothed
  return.list$output.gap.smoothed <- output.gap.smoothed
  return.list$theta               <- theta
  return.list$log.likelihood      <- log.likelihood
  return.list$states              <- states
  return.list$xi.00               <- xi.00
  return.list$P.00                <- P.00
  return.list$y.data              <- y.data
  return.list$initial.parameters  <- initial.parameters
  if (run.se) { return.list$se    <- se }
  return(return.list)
}
