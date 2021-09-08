#' @title Mean Square Prediction Error (MSPE) Function
#'
#' @description Compute Mean square prediction error (MSPE) in matrix form where number of observations in the data is n
#' @param response A \code{vector} dependent [y variable]
#' @param covariates A \code{matrix} independent [x variable]
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{mspe}{Estimated mean square prediction error}
#' }
#' @author Group7
#' @importFrom stats runif
#' @export
#' @examples
#' l_mspe(rep(1,10), matrix(1:20,nrow=10))
#MSPE
l_mspe<-function(response,covariates){
  # data formatting
  response <- as.vector(response)
  covariates <- as.matrix(covariates)
  inte <- as.vector(rep(1,length(response)))
  covariates <- cbind(inte,covariates)
  #compute beta.hat
  beta.hat <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response
  #compute residual
  residuals <- response - (covariates)%*%as.matrix(beta.hat)
  #compute mspe
  mspe<-(1/length(response))*sum(residuals^2)
  return(mspe)
}






