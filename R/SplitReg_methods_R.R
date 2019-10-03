construct.SplitReg <- function(object, fn_call, x, y){
  class(object) <- append("SplitReg", class(object))
  num_betas <- 1
  num_groups <- ncol(object$betas)
  mux_train <- apply(x, 2, mean)
  muy_train <- mean(y)
  object$intercepts <- as.numeric(muy_train) - as.numeric(mux_train %*% object$betas)
  object$call <- fn_call
  return(object)
}

SplitReg_scalar_predict <- function(object, newx, type){
  if(type[1]=="response"){
    coef <- apply(object$betas, 1, mean)
    output <- mean(object$intercepts) + as.numeric(newx %*% coef)
  } else {
    intercept <- mean(object$intercepts)
    coef <- apply(object$betas, 1, mean)
    output <- c(intercept, coef)  
  }  
  return(output)
}

#' @title Make predictions from a cv.SplitReg object.
#' @method predict cv.SplitReg
#' @param object Fitted cv.SplitReg object.
#' @param newx Matrix of new values of x at which prediction are to be made. Ignored if type is "coefficients".
#' @param index Indices indicating values of lambda_S at which to predict. Defaults to the optimal value.
#' @param type Either "response" for predicted values or "coefficients" for the estimated coefficients.
#' @param ... Additional arguments for compatibility

#' @return Either a matrix with predictions or a vector of coefficients
#' 
#' @description 
#' Make predictions from a cv.SplitReg object, similar to other predict methods.
#' 
#' @seealso 
#' \code{\link{predict.cv.SplitReg}}
#' 
#' @examples 
#' library(MASS)
#' set.seed(1)
#' beta <- c(rep(5, 5), rep(0, 45))
#' Sigma <- matrix(0.5, 50, 50)
#' diag(Sigma) <- 1
#' x <- mvrnorm(50, mu = rep(0, 50), Sigma = Sigma)
#' y <- x %*% beta + rnorm(50)
#' fit <- SplitReg(x, y, num_models=2)
#' x.new <- mvrnorm(50, mu = rep(0, 50), Sigma = Sigma)
#' split.predictions <- predict(fit, newx = x.new, type="response")
#' 
#' @export
#' 
predict.SplitReg <- function(object, newx, type = c("response", "coefficients"), ...){

  if(type[1]=="response"){
    if(missing(newx)){
      stop("newx value has to be supplied")
    }
    if(is.matrix(newx)){
      p <- ncol(newx)
    } else if(is.numeric(newx)){
      p <- length(newx)
    } else {
      stop("newx has to be a vector or a matrix")
    } 
    if(p != dim(object$betas)[1]){
      stop("newx does not have the right number of elements")
    }
    output <- SplitReg_scalar_predict(object, newx, type)
  } else {
    output <- SplitReg_scalar_predict(object, newx, type)
    output <- as.matrix(output)
  }
  return(output)
}

#' @title Extract coefficients from a SplitReg object.
#' @method coef SplitReg
#' @param object Fitted cv.SplitReg object.
#' @param ... Additional arguments for compatibility

#' @return A vector of coefficients
#' 
#' @description 
#' Extract coefficients from a SplitReg object.
#' 
#' @seealso 
#' \code{\link{SplitReg}}
#' 
#' @examples 
#' library(MASS)
#' set.seed(1)
#' beta <- c(rep(5, 5), rep(0, 45))
#' Sigma <- matrix(0.5, 50, 50)
#' diag(Sigma) <- 1
#' x <- mvrnorm(50, mu = rep(0, 50), Sigma = Sigma)
#' y <- x %*% beta + rnorm(50)
#' fit <- SplitReg(x, y, num_models=2)
#' split.coefs <- coef(fit)
#' 
#' @export
#' 
coef.SplitReg <- function(object,...){
  return(predict(object, type = "coefficients"))
}