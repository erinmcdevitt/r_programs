#------------------------------------------------------------------------------#
# File:        unpack.parameters.stage2.R
#
# Description: This file generates coefficient matrices for the stage 2
#              state-space model for the given parameter vector.
#
# Stage 2 parameter vector: [a_y,1, a_y,2, a_r, a_0, a_g, b_pi, b_y, sigma_y~, sigma_pi, sigma_y*, phi]  
#------------------------------------------------------------------------------#
unpack.parameters.stage2 <- function(parameters, y.data, x.data, lambda.g, xi.00=NA, P.00=NA) {
  A         <- matrix(0, 2, 10)
  A[1, 1:2] <- parameters[1:2]   # a_y,1, a_y,2
  A[1, 3:4] <- parameters[3]/2   # a_r/2
  A[1, 7]   <- parameters[4]     # a_0
  A[2, 1]   <- parameters[7]     # b_y
  A[2, 5]   <- parameters[6]     # b_pi
  A[2, 6]   <- 1 - parameters[6] # 1 - b_pi
  A[1, 8]   <- parameters[11]    # phi
  A[1, 9]   <- -parameters[1]*parameters[11] # -a_y,1*phi
  A[1, 10]  <- -parameters[2]*parameters[11] # -a_y,2*phi
  A[2, 9]   <- -parameters[7]*parameters[11] # -b_y*phi
  A         <- t(A)
  
  H         <- matrix(0, 2, 4)
  H[1, 1  ] <- 1
  H[1, 2:3] <- -parameters[1:2] # -a_y,1, -a_y,2
  H[1, 4  ] <- parameters[5]    # a_g
  H[2, 2]   <- -parameters[7]   # -b_y
  H         <- t(H)

  R         <- diag(c(parameters[8]^2, parameters[9]^2)) # sigma_y~, sigma_pi
  Q         <- matrix(0, 4, 4)
  Q[1, 1]   <- parameters[10]^2              # sigma_y*
  Q[4, 4]   <- (lambda.g * parameters[10])^2 # sigma_y*

  F <- matrix(0, 4, 4)
  F[1, 1] <- F[1, 4] <- F[2, 1] <- F[3, 2] <- F[4,4] <- 1
  
  return(list("xi.00"=xi.00, "P.00"=P.00, "F"=F, "Q"=Q, "A"=A, "H"=H, "R"=R, "x.data"=x.data, "y.data"=y.data))
}
