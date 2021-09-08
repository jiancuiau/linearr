#' @title Plotting Function
#'
#' @description Compute plots of linear regression -
#' - residuals vs fitted, residuals and histogram of residuals.
#' @param response A \code{vector} of response [y variable]
#' @param covariates A \code{matrix} of covariates [x variable]
#' @param type A \code{list} used to control the kinds of plot. Various plots are -
#' RF - residuals vs fitted plot,
#' QQR - QQ plot of residuals,
#' HR - histogram of residuals,
#' all - all three plots
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{RF Plot}{Plot of residuals vs fitted values}
#'      \item{QQ Plot}{Quantile-Quantile Plot of residuals}
#'      \item{HR Plot}{Histogram or density of residuals plot}
#' }
#' @author Group7
#' @importFrom stats runif
#' @export
#' @examples
#' ln_plot(rep(1,10), matrix(1:20,nrow=10),'all')
ln_plot = function(response,covariates,type){

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

  if(type == 'RF') {

    # Residuals VS fitted-values
    plot(pred,resid)
  }
  else if (type == 'QQR'){

    # qq-plot of residuals
    qqnorm(resid, pch = 1, frame = FALSE)
    qqline(resid, col = "steelblue", lwd = 2)

  }
  else if (type == 'HR'){
    # histogram of residuals
    hist(resid)
  }
  else if (type == 'all'){
    par(mfrow = c(1,3))
    plot(pred,resid)
    qqnorm(resid, pch = 1, frame = FALSE)
    qqline(resid, col = "steelblue", lwd = 2)
    hist(resid)
  }
}
