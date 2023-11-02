#------------------------------------------------------------------------------#
# File:        rstar.stage1.R
#
# Description: This file runs the model in the first stage of the HLW estimation.
#------------------------------------------------------------------------------#
rstar.stage1 <- function(log.output,
                         inflation,
                         covid.dummy,
                         T.og0.omit,
                         b2.constraint=NA) {

  stage <- 1

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
  # y(t) - q(t) = phi*d(t) + a_{y,1}*(y(t-1) - q(t-1) - phi*d(t-1)) + a_{y,2}(y(t-2) - q(t-2) - phi*d(t-2)) + eps(t)
  y.is     <- (log.output[5:(T+4)] - q[5:(T+4)]) * 100
  y.is.l1  <- (log.output[4:(T+3)] - q[4:(T+3)]) * 100
  y.is.l2  <- (log.output[3:(T+2)] - q[3:(T+2)]) * 100
  d        <- covid.dummy[5:(T+4)]
  d.l1     <- covid.dummy[4:(T+3)]
  d.l2     <- covid.dummy[3:(T+2)]
  # If the start date is after 2020:Q1, run the model with COVID dummy included
  # Otherwise, initialize phi at zero 
  if (T.og0.omit > 0) {
    nls.is   <- nls(y.is ~ phi*d + a_1*(y.is.l1 - phi*d.l1) + a_2*(y.is.l2 - phi*d.l2),
                  start = list(phi=0,a_1=0,a_2=0))
    b.is     <- coef(nls.is)
  } else {
    nls.is   <- nls(y.is ~ a_1*(y.is.l1) + a_2*(y.is.l2),
                    start = list(a_1=0,a_2=0))
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
  initial.parameters <- c(b.is["a_1"], b.is["a_2"], b.ph[1], b.ph[3], 0.85, s.is, s.ph, 0.5, b.is["phi"]) # with guess for dummy coefficient


  # Initialization of state vector for Kalman filter using HP trend of log output
  log.output.hp.trend <- hpfilter(log.output,freq=36000,type="lambda",drift=FALSE)$trend
  g.pot <- log.output.hp.trend[(g.pot.start.index):length(log.output.hp.trend)]
  xi.00 <- c(100*g.pot[3:1])


  #----------------------------------------------------------------------------#
  # Build data matrices
  #----------------------------------------------------------------------------#
  y.data <- cbind(100 * log.output[5:(T+4)],
                  inflation[5:(T+4)])
  x.data <- cbind(100 * log.output[4:(T+3)],
                  100 * log.output[3:(T+2)],
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
      if (initial.parameters[4] < b2.constraint) {
          initial.parameters[4] <- b2.constraint
      }
      theta.lb[4] <- b2.constraint
  }

  # Set the initial covariance matrix (see footnote 6)
  P.00 <- calculate.covariance(initial.parameters, theta.lb, theta.ub, y.data, x.data, stage, NA, NA, xi.00)

  # Get parameter estimates via maximum likelihood
  f <- function(theta) {return(-log.likelihood.wrapper(theta, y.data, x.data, stage, NA, NA, xi.00, P.00)$ll.cum)}
  nloptr.out <- nloptr(initial.parameters, f, eval_grad_f=function(x) {gradient(f, x)},
                       lb=theta.lb,ub=theta.ub,
                       opts=list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-8))
  theta <- nloptr.out$solution

  log.likelihood <- log.likelihood.wrapper(theta, y.data, x.data, stage, NA, NA, xi.00, P.00)$ll.cum

  #----------------------------------------------------------------------------#
  # Kalman filtering
  #----------------------------------------------------------------------------#

  # Get state vectors (xi.tt, xi.ttm1, xi.tT, P.tt, P.ttm1, P.tT) via Kalman filter
  states <- kalman.states.wrapper(theta, y.data, x.data, stage, NA, NA, xi.00, P.00)

  # One-sided (filtered) estimates
  potential.filtered  <- states$filtered$xi.tt[,1]/100
  output.gap.filtered <- y.data[,1] - (potential.filtered * 100) - theta[length(theta)]*covid.dummy[5:(T+4)] # with covid dummy

  # Two-sided (smoothed) estimates
  potential.smoothed  <- as.vector(states$smoothed$xi.tT[,1])/100
  output.gap.smoothed <- y.data[,1] - (potential.smoothed * 100) - theta[length(theta)]*covid.dummy[5:(T+4)] # with covid dummy

  # Save variables to return
  return.list                <- list()
  return.list$theta          <- theta
  return.list$log.likelihood <- log.likelihood
  return.list$states         <- states
  return.list$xi.00          <- xi.00
  return.list$P.00           <- P.00
  return.list$potential.filtered  <- potential.filtered
  return.list$output.gap.filtered <- output.gap.filtered
  return.list$potential.smoothed  <- potential.smoothed
  return.list$output.gap.smoothed <- output.gap.smoothed
  return(return.list)
}
