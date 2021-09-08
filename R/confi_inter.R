#' @title Confidence Interval Function
#'
#' @description Computes the confidence intervals of a linear regression based on user input of significance level and the approach of asymptotic.
#' @param response A \code{vector} of response [y variable]
#' @param covariates A \code{matrix} of covariates [x variable]
#' @param alpha A \code{numeric} significance level, user input, default = 0.05
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{ci.beta}{Estimated confidence intervals}
#' }
#' @author Group7
#' @importFrom stats runif
#' @export
#' @examples
#' confi_inter(rep(1,10), matrix(1:20,nrow=10), alpha =  0.05)

confi_inter = function(response, covariates, alpha =  0.05) {

  # Make sure data formats are appropriate
  response <- as.vector(response)
  covariates <- as.matrix(covariates)

  # Define parameters
  n <- length(response)
  inte <- as.vector(rep(1,n))
  covariates <- cbind(inte,covariates)
  p <- ncol(covariates)
  df <- n - p


  # Estimate beta through Eq. (6.1)
  beta.hat <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response

  # Estimate of the residual variance (sigma2) from Eq. (6.3)
  # Compute residuals
  pred <- covariates%*%as.matrix(beta.hat)
  resid <- response - pred
  sigma2.hat <- (1/df)*t(resid)%*%resid
  sigma2.hat  <- sigma2.hat[1,1]

  # Estimate of the variance of the estimated beta from Eq. (6.2)
  var.beta <- sigma2.hat*solve(t(covariates)%*%covariates)
  var.beta <- as.vector(diag(var.beta))


  # Estimate of the confidence interval based on alpha
  quant <- 1 - alpha/2
  ci.beta <- c(beta.hat - qnorm(quant)*sqrt(var.beta), beta.hat +
                 qnorm(quant)*sqrt(var.beta))

  ci.beta<- matrix(ci.beta,ncol =2)

  rownames(ci.beta) <- rownames(ci.beta, do.NULL = FALSE, prefix = "Beta.")

  colnames(ci.beta) <- c(as.character(alpha/2),as.character(1-alpha/2))
  return(ci.beta)
}
