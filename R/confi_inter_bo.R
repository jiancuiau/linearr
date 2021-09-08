#' @title Confidence Interval Function by Bootstrap Approach
#'
#' @description Computes the confidence intervals of a linear regression based on user input of significance level and the approach of bootstrap.
#' @param response A \code{vector} of response [y variable]
#' @param covariates A \code{matrix} of covariates[x variable]
#' @param alpha A \code{numeric} significance level, user input, Between 1 and 0. default = 0.05
#' @param m A \code{numeric} The number of bootstrap replicates, Usually this will be a single positive integer. user input, default = 200
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{ci.beta}{Estimated confidence intervals}
#' }
#' @author Group7
#' @importFrom stats runif
#' @export
#' @examples
#' confi_inter_bo(response, covariates, alpha= 0.05, m = 200)


confi_inter_bo = function(response, covariates, alpha= 0.05, m = 200) {

  response <- as.vector(response)
  covariates <- as.matrix(covariates)

  # Define parameters
  n <- length(response)
  p <- ncol(covariates)
  df <- n - p
  m = 200
  p+1
  temarray <- array(rep(0,(p+1)*m*2),dim = c((p+1),2,m))

  for (b in 1:m) {
    i <- sample(1:n, size = n, replace = TRUE)
    y <- response[i]
    x <- covariates[i,]
    temarray[,,b] <- confi_inter(y, x, alpha =  0.05)
  }

  out_come <- apply(temarray,c(1,2),mean)

  rownames(out_come) <- rownames(out_come, do.NULL = FALSE, prefix = "Beta.")

  colnames(out_come) <- c(as.character(alpha/2),as.character(1-alpha/2))

  return(out_come)
}
