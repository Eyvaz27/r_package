#' Building a model with Top Top features
#'
#' This function develops a prediction algorithm
#' based on a Top Ten features in 'x' that are most
#' predictive of the target 'y'
#' @param x a n x p matrix of n observations
#' and p predictors
#' @param y a vector of length n representinf the response
#' @return a vector of coefficients from the final fitted model with Top Ten Features
#' @author Eyvaz Najafli
#' @details
#' This function runs a univariate regression of y
#' on each predictor in x and calculates the 'p' value
#' indicating the significance of association. The final
#' set of 10 predictors is taken from the 10 smallest p-value
#' @seealso \code{lm}
#' @export
#' @importFrom  stats lm
#'

topten <- function (x, y){
  p<- ncol(x)
  if(p<10){
    stop("There are less than 10 predictors")
  }
  pvalues <- numeric(p)
  for(i in seq_len(p)){
    fit <- lm(y~ x[,i])
    summ <- summary(fit)
    pvalues[i] <- summ$coefficients[2, 4]
  }
  ord <- order(pvalues)
  ord <- ord[1:10]
  x10 <- x[, ord]
  best.fit <- lm(y ~ x10)
  coef(best.fit)
}

#' Prediction with Top Ten features
#'
#' This function takes a set of coefficients produced by
#' \code{topten} function and makes a prediction for each of
#' the values provided in the input 'X' matrix
#'
#' @param X a n x 10 matrix containing the observation values
#' @param b a vector of coefficients obtained from the \code{topten} function
#' @return a numeric vector containing the predicted values
#' @export
#'

predict10 <- function(X, b){
  X <-  cbind(1, X)
  drop(X %*% b)
}

