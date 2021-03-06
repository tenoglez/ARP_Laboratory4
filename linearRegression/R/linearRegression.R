#' A package for handling linear regression models using QR decomposition.
#'
#' @description The linearRegression package provides a class to handle linear regression models, \code{linreg}.\cr\cr
#' In the package there is also the function \code{linregRC} which return an object of class \code{linreg} with all statistics
#' stored in it.
#' 
#' @details The class linear regression has the following structure:
#' \itemize{
#'    \item Attributes\cr\itemize{
#'      \item \code{coefficients}
#'      \item \code{predicted}
#'      \item \code{residuals}
#'    }
#'   \item Methods\cr\itemize{
#'      \item \code{coef()}: returns the vector of residuals \code{e}.
#'      \item \code{pred()}: returns the predicted values \code{y}.
#'      \item \code{resid()}: return the coefficients as a named vector.
#'      \item \code{print()}: prints out the coefficients and coefficient names, similar as done by the \code{lm} class.
#'      \item \code{plot()}: plots graphs using \code{ggplot2}.
#'      \item \code{summary()}: returns a similar printout as printed for \code{lm} objects, but presents 
#'      the coefficients with their standard error, t-value and p-value as well as the estimate of \code{sigma} and the degrees
#'      of freedom in the model.
#'    }
#' }

#'
#' @author \itemize{
#'      \item Teno González Dos Santos - \url{https://github.com/tenoglez}
#'      \item Josué Álvarez Robles - \url{https://github.com/ejarkm}
#'      \item Jose Luis López Ruiz - \url{https://github.com/pepelu8000}
#'    }
#' 
#' @references Linear regression - \url{https://en.wikipedia.org/wiki/Linear_regression}\cr
#' QR Decomposition - \url{https://en.wikipedia.org/wiki/QR_decomposition}
#'
#'
#' @docType package
#' @name linearRegression
"_PACKAGE"