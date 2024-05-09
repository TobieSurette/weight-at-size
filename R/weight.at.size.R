#' @title Allometric analysis
#' 
#' @description Functions to estimate allometric coefficients from data.
#' 
#' @param x Size.
#' @param y Weight.
#' @param precision Weight measurement precision. For a given precision \code{p}, weight obsrvervation \code{w}
#'                  will be assumed to lie in the interval \code{[w-p/2, w+p/2]}.
#' @param robust Logical value specifying whether to include a second error term for handling data outliers. Default is \code{FALSE}.
#' @param plot Logical value specifying whether to plot the fitted model over the data observations. Default is \code{FALSE}.
#' 
#' @return A vector of \alpha and \beta coeffcients from the allometric relation $y = \alpha x^{\beta}$ will be returned.
#' 

fit.allometric <- function(x, y, precision, robust = FALSE, plot = FALSE){
   # Check input parameters:
   if (length(x) != length(y)) stop("'x' and 'y' vectors have inconsistent lengths.")
   if (missing(precision) & any(y==0)) stop("Weights 'y' cannot be zero if precision is not specified.")
   
   # Check precision argument:
   if (!missing(precision)){
      if (length(precision) == 1) precision <- rep(precision, length(x))
      if (length(x) != length(precision)) stop("Weight precision vector 'precision' is inconsistent with size and weight data.")
      if (any(precision < 0)) stop("Weight precision vector 'precision' must be positive.")
      precision[is.na(precision)] <- 0
   }
   
   # Collapse data into frequencies (this accelerates the code):
   if (missing(precision)){
      data <- aggregate(list(f = x), by = data.frame(x = x, y = y), length)
   }else{
      data <- aggregate(list(f = x), by = data.frame(x = x, y = y, precision = precision), length)
   }    
   
   # Initialize allometric parameters:
   ix <- which((data$x > 0) & (data$y > 0))
   m <- lm(log(data$y[ix]) ~ log(data$x[ix]), weights = data$f[ix])
   theta <- c(log.alpha = coef(m)[[1]], beta = coef(m)[[2]], log.sigma = log(summary(m)$sigma))
   if (robust){
      # Add extra parameters for handling outliers:
      theta[["log.sigma.outlier"]] <- 2 * log(2)                     # Outliers have a wider distribution.
      theta[["log.sigma"]]         <- theta[["log.sigma"]] - log(2)  # Regular observations have a tighter distribution.
      theta[["logit.p.outlier"]] <- -3                               # Assume around 5% initially.
   }
     
   loglike.allometric <- function(theta, x, y, f, precision){
      # Regression mean:
      mu <- theta["log.alpha"] + theta["beta"] * log(x)
      sigma <- exp(theta["log.sigma"])
      if (missing(precision)){
         # Continuous weight observations:
         if (!("logit.p.outlier" %in% names(theta))){
            # Regular regression:
            v <- f * dnorm(log(y), mu, sigma, log = TRUE)
         }else{
            # Robust regression:
            sigma.outlier <- exp(theta["log.sigma"]) + exp(theta["log.sigma.outlier"]) 
            p.outlier <- 1 / (1 + exp(-theta[["logit.p.outlier"]]))
            v <- f * log((1-p.outlier) * dnorm(log(y), mu, sigma) + p.outlier * dnorm(log(y), mu, sigma.outlier))
         }
      }else{
         # Discrete weight observations:
         upper <- y+0.5*precision
         lower <- y-0.5*precision                         
         lower[lower < 0] <- 0 
         if (!("logit.p.outlier" %in% names(theta))){
            # Regular regression:
            v <- f * log((pnorm(log(upper), mu, sigma) - pnorm(log(lower), mu, sigma)) / (upper - lower))
         }else{
            # Robust regression:
            sigma.outlier <- exp(theta["log.sigma"]) + exp(theta["log.sigma.outlier"]) 
            p.outlier <- 1 / (1 + exp(-theta[["logit.p.outlier"]]))
            v <- f * log((1-p.outlier) * ((pnorm(log(upper), mu, sigma) - pnorm(log(lower), mu, sigma)) / (upper - lower)) + 
                            p.outlier  * ((pnorm(log(upper), mu, sigma.outlier) - pnorm(log(lower), mu, sigma.outlier)) / (upper - lower))) 
         }         
      }
      
      return(-sum(v))
   }

   # Fit model:
   if (missing(precision)){
      # Data are treated as infinitely precise
      loglike.allometric(theta,  x = data$x, y = data$y, f = data$f)
      theta <- optim(theta, fn = loglike.allometric, x = data$x, y = data$y, f = data$f, control = list(trace = 3))$par
   }else{
      # Include data precision in model:
      loglike.allometric(theta,  x = data$x, y = data$y, f = data$f, precision = data$precision)
      theta <- optim(theta, fn = loglike.allometric, x = data$x, y = data$y, f = data$f, precision = data$precision, control = list(maxit = 2000, trace = 3))$par
      theta <- optim(theta, fn = loglike.allometric, x = data$x, y = data$y, f = data$f, precision = data$precision, control = list(maxit = 2000, trace = 3))$par
      theta <- optim(theta, fn = loglike.allometric, x = data$x, y = data$y, f = data$f, precision = data$precision, control = list(maxit = 2000, trace = 3))$par
   }
   
   # Plot results:   
   if (plot == TRUE){
      log.x <- seq(1, 5, len = 100)
      mu <- theta["log.alpha"] + theta["beta"] * log.x
      sigma <- exp(theta["log.sigma"])

      plot(log(jitter(rep(data$x, times = data$f), amount = 0.5)), 
           log(jitter(rep(data$y, times = data$f), amount = 0.5)), 
           cex = 0.5, xlab = "size", ylab = "weight")
      lines(log.x, mu, lwd = 2, col = "red2")
      lines(log.x, mu - sigma, lwd = 1, lty = "dashed", col = "red2")
      lines(log.x, mu + sigma, lwd = 1, lty = "dashed", col = "red2")
      
      if ("logit.p.outlier" %in% names(theta)){
         sigma.outlier <- exp(theta["log.sigma"]) + exp(theta["log.sigma.outlier"]) 
         p.outlier <- 1 / (1 + exp(-theta[["logit.p.outlier"]]))
         lines(log.x, mu - sigma.outlier, lwd = 1, lty = "dashed", col = "red2")
         lines(log.x, mu + sigma.outlier, lwd = 1, lty = "dashed", col = "red2") 
      }
   }

   # Convert parameter vector to standard form:
   theta["alpha"] <- exp(theta[["log.alpha"]])
   theta["sigma"] <- exp(theta[["log.sigma"]])
   if ("logit.p.outlier" %in% names(theta)) theta["p.outlier"] <- 1 / (1 + exp(-theta[["logit.p.outlier"]]))
   if ("log.sigma.outlier" %in% names(theta)) theta["sigma.outlier"] <- exp(theta[["log.sigma"]]) + exp(theta[["log.sigma.outlier"]])    
   theta <- theta[setdiff(names(theta), c("log.alpha", "log.sigma", "log.sigma.outlier", "logit.p.outlier"))]
   theta <- c(theta[c("alpha", "beta", "sigma")], theta[setdiff(names(theta), c("alpha", "beta", "sigma"))])
   
   return(theta)
}
