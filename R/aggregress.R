#' aggregress: A package for regression analysis with aggregated data.
#'
#' The aggregress package makes adjustments to the lm object output
#' when the inputted data are in aggregated form.
#' That is, when the data frame has a list of unique values of
#' all variables with the frequencies recorded in a column of weights.
#' The resulting object is indistinguishable from that from the
#' original unaggregated data.
#' It includes adjustments to the statistics and diagnostics.
#'
#' To quote the help files for the \code{lm} function:
#' "Therefore, the sigma estimate and residual degrees of freedom
#' may be suboptimal; in the case of replication weights, even wrong.
#' Hence, standard errors and analysis of variance tables should be
#' treated with care."
#' This packages makes adjustments so that the sigma estimate and
#' residual degrees of freedom are not suboptimal or wrong; they are correct.
#'
#' @docType package
#' @name aggregress
NULL


#' Summarizing Linear Model Fits with Aggregated Data (DEPRECATED)
#'
#' A modification of the \code{summary} method for class \code{lm},
#' for the case when .
#'
#' @param object an object of class "\code{lm}",
#' estimated with \code{weights} equal to the counts in each distinct
#' combination of covariates in an aggregated dataset.
#'
#' @seealso
#' Instead, use \code{agg_lm} for model fits to produce objects in class
#' "\code{agg_lm}" then summarize with \code{summary_agg_lm}.
#' Modifies the \code{summary.lm} function in the \code{stats} library.
#' @return \code{adj_wtd_lm_summary} computes and returns a list of summary
#' statistics of the fitted linear model given in \code{object}.
#'
adj_wtd_lm_summary <- function(wtd_lm) {

  # Copy the lm object.
  adj_wtd_lm <- wtd_lm

  # Replace the df for the residual (n - k).
  adj_wtd_lm$df.residual <- sum(adj_wtd_lm$weights) - length(coef(adj_wtd_lm))

  # Make a copy of the summary to adjust the R-bar_squared.
  sum_adj_wtd_lm <- summary(adj_wtd_lm)

  # Replace the value of the R-bar-squared.
  sum_adj_wtd_lm$adj.r.squared <-
    1 - (1 - sum_adj_wtd_lm$r.squared) *
    (sum(adj_wtd_lm$weights) - 1) /
    (sum(adj_wtd_lm$weights) - length(coef(adj_wtd_lm)))

  # Return the summary with the adjusted weighted regression
  # to replace the original individual-level data.
  return(sum_adj_wtd_lm)

}


#' Fitting Linear Models with Aggregated Data
#'
#' \code{agg_lm} is used to fit a linear regression model
#' when the data are in aggregated form. That is, each unique
#' combination of the covariates is assigned an integer count
#' that is passed to the \code{weights} argument.
#' It is useful for data that has been aggregated with a
#' \code{GROUP BY <list of variables>} clause in an \code{SQL} query.
#' It is designed to imitate the \code{lm} function, with appropriate
#' adjustments for the sample size of the original un-aggregated data.
#' Aside from the \code{weights} argument being mandatory, the arguments
#' are identical to those in \code{lm}.
#'
#' @seealso \code{lm} function in the \code{stats} library.
#' \code{summary_agg_lm} for summaries of objects in class "\code{agg_lm}".
#' @return \code{agg_lm} returns an object of class "\code{agg_lm}",
#' the analogue for an object of class "\code{lm}", except with
#' aggregated data.
#' @export
#'
agg_lm <- function (formula, data, subset, weights, na.action, method = "qr",
                    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
                    contrasts = NULL, offset, ...)
{
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if (method == "model.frame")
    return(mf)
  else if (method != "qr")
    warning(gettextf("method = '%s' is not supported. Using 'qr'",
                     method), domain = NA)
  mt <- attr(mf, "terms")
  y <- model.response(mf, "numeric")
  if (is.matrix(y))
    stop("'y' must be a vector when using aggregated data")
  w <- as.vector(model.weights(mf))
  if (is.null(w))
    stop("'weights' must be specified when using aggregated data")
  if (!is.null(w) && !is.numeric(w))
    stop("'weights' must be a numeric vector")
  offset <- as.vector(model.offset(mf))
  if (!is.null(offset)) {
    if (length(offset) != NROW(y))
      stop(gettextf("number of offsets is %d, should equal %d (number of observations)",
                    length(offset), NROW(y)), domain = NA)
  }
  if (is.empty.model(mt)) {
    x <- NULL
    z <- list(coefficients = numeric(),
              residuals = y,
              fitted.values = 0 * y,
              weights = w,
              rank = 0L,
              df.residual = sum(w)
    )
    if (!is.null(offset)) {
      z$fitted.values <- offset
      z$residuals <- y - offset
    }
  }
  else {
    x <- model.matrix(mt, mf, contrasts)
    z <-  lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, ...)
  }
  class(z) <- c("agg_lm")
  z$df.residual <- sum(w) - z$rank
  z$na.action <- attr(mf, "na.action")
  z$offset <- offset
  z$contrasts <- attr(x, "contrasts")
  z$xlevels <- .getXlevels(mt, mf)
  z$call <- cl
  z$terms <- mt
  if (model)
    z$model <- mf
  if (ret.x)
    z$x <- x
  if (ret.y)
    z$y <- y
  if (!qr)
    z$qr <- NULL
  z
}


#' Summarizing Linear Model Fits with Aggregated Data
#'
#' \code{summary} method for class \code{agg_lm}.
#'
#' @param object an object of class "\code{agg_lm}",
#' usually a result of a call to \code{agg_lm}.
#'
#' @seealso \code{summary.lm} function in the \code{stats} library.
#' \code{agg_lm} for model fits to produce objects in class "\code{agg_lm}".
#' @return \code{summary_agg_lm} computes and returns a list of summary statistics
#' of the fitted linear model given in \code{object}.
#' @export
#'
summary_agg_lm <- function (object, correlation = FALSE, symbolic.cor = FALSE,
                            ...)
{
  z <- object
  p <- z$rank
  rdf <- z$df.residual
  if (p == 0) {
    r <- z$residuals
    # n <- length(r)
    w <- z$weights
    n <- sum(w)
    if (is.null(w)) {
      rss <- sum(r^2)
    }
    else {
      rss <- sum(w * r^2)
      r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    ans <- z[c("call", "terms", "weights")]
    class(ans) <- "summary.lm"
    ans$aliased <- is.na(coef(object))
    ans$residuals <- r
    ans$df <- c(0L, n, length(ans$aliased))
    ans$coefficients <- matrix(NA, 0L, 4L)
    dimnames(ans$coefficients) <- list(NULL, c("Estimate",
                                               "Std. Error", "t value", "Pr(>|t|)"))
    ans$sigma <- sqrt(resvar)
    ans$r.squared <- ans$adj.r.squared <- 0
    return(ans)
  }
  if (is.null(z$terms))
    stop("invalid 'agg_lm' object:  no 'terms' component")
  if (!inherits(object, "agg_lm"))
    warning("calling summary.agg_lm(<fake-lm-object>) ...")
  Qr <- stats:::qr.lm(object)
  # n <- NROW(Qr$qr)
  r <- z$residuals
  f <- z$fitted.values
  w <- z$weights
  n <- sum(w)
  if (is.na(z$df.residual) || n - p != z$df.residual)
    warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
  if (is.null(w)) {
    mss <- if (attr(z$terms, "intercept"))
      sum((f - mean(f))^2)
    else sum(f^2)
    rss <- sum(r^2)
  }
  else {
    mss <- if (attr(z$terms, "intercept")) {
      m <- sum(w * f/sum(w))
      sum(w * (f - m)^2)
    }
    else sum(w * f^2)
    rss <- sum(w * r^2)
    r <- sqrt(w) * r
  }
  resvar <- rss/rdf
  if (is.finite(resvar) && resvar < (mean(f)^2 + var(f)) *
      1e-30)
    warning("essentially perfect fit: summary may be unreliable")
  p1 <- 1L:p
  R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
  se <- sqrt(diag(R) * resvar)
  est <- z$coefficients[Qr$pivot[p1]]
  tval <- est/se
  ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
  ans$residuals <- r
  ans$coefficients <- cbind(Estimate = est, `Std. Error` = se,
                            `t value` = tval, `Pr(>|t|)` = 2 * pt(abs(tval), rdf,
                                                                  lower.tail = FALSE))
  ans$aliased <- is.na(z$coefficients)
  ans$sigma <- sqrt(resvar)
  ans$df <- c(p, rdf, NCOL(Qr$qr))
  if (p != attr(z$terms, "intercept")) {
    df.int <- if (attr(z$terms, "intercept"))
      1L
    else 0L
    ans$r.squared <- mss/(mss + rss)
    ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n -
                                                       df.int)/rdf)
    ans$fstatistic <- c(value = (mss/(p - df.int))/resvar,
                        numdf = p - df.int, dendf = rdf)
  }
  else ans$r.squared <- ans$adj.r.squared <- 0
  ans$cov.unscaled <- R
  dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1,
                                                             1)]
  if (correlation) {
    ans$correlation <- (R * resvar)/outer(se, se)
    dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
    ans$symbolic.cor <- symbolic.cor
  }
  if (!is.null(z$na.action))
    ans$na.action <- z$na.action
  class(ans) <- "summary.lm"
  ans
}


#' Generate data to test the aggregress package.
#'
#' \code{gen_agg_lm} generates data to test the \code{aggregress}
#' package. It is an internal function used for testing.
#'
#' @param model a string to denote the model with which
#' to generate the data frame.
#'
#' @returns a data frame of response and covariates for
#' linear regression.
#'
gen_agg_lm <- function(model) {

  if (model == 'lpm') {

    # Initialize the data frame with covariates.
    agg_lm_df <- data.frame(expand.grid(x1 = seq(1,3),
                                        x2 = seq(5,10),
                                        x3 = c(2, 2, 2, 4, 4, 6)))

    # Add an outcome according to a linear probability model.
    # All coefficients are ones.
    agg_lm_df[, 'probs'] <- rowSums(agg_lm_df[, c('x1', 'x2', 'x3')]) / 20


    # Draw a binary dependent variable.
    agg_lm_df[, 'y'] <- as.integer(runif(nrow(agg_lm_df)) <=  agg_lm_df[, 'probs'])


  } else {
    stop(sprintf('Model type %s not supported.', model))
  }

  return(agg_lm_df)

}
