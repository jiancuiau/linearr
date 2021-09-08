#' @title F test Function
#'
#' @description Compute F-test statistic in matrix form and the corresponding p-value
#' @param response A \code{vector} of response [y variable]
#' @param covariates A \code{matrix} of covariates [x variable]
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{f.value}{Calculated F value}
#'      \item{fp.value}{p value after comparing f calculated and f observed}
#' }
#' @author Group7
#' @importFrom stats runif
#' @export
#' @examples
#' l_ftest(rep(1,10), matrix(1:20,nrow=10))
l_ftest<-function(response,covariates){
  #data formatting
  response <- as.vector(response)
  covariates <- as.matrix(covariates)
  n<-length(response)
  inte <- as.vector(rep(1,n))
  covariates <- cbind(inte,covariates)
  #compute beta,hat
  beta.hat <- solve(t(covariates)%*%covariates)%*%t(covariates)%*%response
  #compute fiited.value
  fitted.values<-covariates%*%beta.hat
  #compute ssm and sse
  ssm<-sum((fitted.values-mean(response))^2)
  sse<-sum((response-fitted.values)^2)
  #get n and p through eg 6.1
  p<-dim(covariates)[2]
  #computedmf and dfe
  dmf<-p-1
  dfe<-n-p
  #compute msm and mse
  msm<-ssm/dmf
  mse<-sse/dfe
  #compute fvalue and pvalue
  f.value<-msm/mse
  fp.value <- pf(f.value,p-1,n-p,lower.tail = FALSE)
  return(list(f.value,fp.value))
}



