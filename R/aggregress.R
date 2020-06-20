#' aggregress: A package for regression analysis with aggregated data.
#'
#' The aggregress package makes adjustments to the lm object output
#' when the inputted data are in aggregated form.
#' That is, when the data frame has a list of unique values of
#' all variables with the frequencies recorded in a column of weights.
#' The resulting object is indistinguishable from that from the
#' original unaggregated data.
#' It includes adjustments to the statistics and diagnostics for a
#' regression model.
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
                    model = TRUE, x = FALSE, y = FALSE, y_squared = NULL,
                    qr = TRUE, singular.ok = TRUE,
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
  is_LPM <- all(y %in% c(0, 1))
  if (!is_LPM & is.null(y_squared))
    stop('For a linear regression model that is not a linear ',
         "probability model, 'y_squared' must be specified")
  w <- as.vector(model.weights(mf))
  if (is.null(w))
    stop("'weights' must be specified when using aggregated data")
  if (!is.numeric(w))
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
  z$is_LPM <- is_LPM
  if (model)
    z$model <- mf
  if (ret.x)
    z$x <- x
  if (ret.y | !is_LPM)
    z$y <- y
  if (!is_LPM)
    z$y_squared <- y_squared
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
    if (is.null(w))
      stop("model object is not an agg_lm object",
           "'weights' must be specified when using aggregated data")
    n <- sum(w)
    rss <- sum(w * r^2)
    # r <- sqrt(w) * r
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
  y <- z$y
  y_squared <- z$y_squared
  if (is.null(w))
    stop("invalid 'agg_lm' object:",
         "'weights' must be specified when using aggregated data")
  n <- sum(w)
  if (is.na(z$df.residual) || n - p != z$df.residual)
    warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")

  if (attr(z$terms, "intercept")) {
    m <- sum(w * f/sum(w))
    mss <- sum(w * (f - m)^2)
  } else {
    mss <- sum(w * f^2)
  }
  if (z$is_LPM) {
    rss <- sum(w * r^2)
  } else {
    # rss <- sum(w * r^2)
    rss <- sum(y_squared) - 2*sum(w * y * f) + sum(w * f^2)
  }
  # r <- sqrt(w) * r
  resvar <- rss/rdf
  if (is.finite(resvar) && resvar < (mean(f)^2 + var(f)) * 1e-30)
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
  } else {
    ans$r.squared <- ans$adj.r.squared <- 0
  }
  # Covariance matrix is still wrong on off-diagonal if not a LPM.
  if (z$is_LPM) {
    ans$cov.unscaled <- R
    dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1, 1)]
    if (correlation) {
      ans$correlation <- (R * resvar)/outer(se, se)
      dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
      ans$symbolic.cor <- symbolic.cor
    }
  } else {
    ans$cov.unscaled <- NULL
    if (correlation) {ans$symbolic.cor <- NULL}
  }
  if (!is.null(z$na.action))
    ans$na.action <- z$na.action
  class(ans) <- "summary.lm"
  ans
}


#' Predict Method for Linear Model Fits with Aggregated Data
#'
#' \code{predict} method for class \code{agg_lm}.
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
predict.agg_lm <- function (object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
                            interval = c("none", "confidence", "prediction"), level = 0.95,
                            type = c("response", "terms"), terms = NULL, na.action = na.pass,
                            pred.var = res.var/weights, weights = 1, ...)
{
  tt <- terms(object)
  if (!inherits(object, "agg_lm"))
    warning("calling predict.lm(<fake-lm-object>) ...")
  if (missing(newdata) || is.null(newdata)) {
    mm <- X <- model.matrix(object)
    mmDone <- TRUE
    offset <- object$offset
  }
  else {
    Terms <- delete.response(tt)
    m <- model.frame(Terms, newdata, na.action = na.action,
                     xlev = object$xlevels)
    if (!is.null(cl <- attr(Terms, "dataClasses")))
      .checkMFClasses(cl, m)
    X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
    offset <- rep(0, nrow(X))
    if (!is.null(off.num <- attr(tt, "offset")))
      for (i in off.num) offset <- offset + eval(attr(tt,
                                                      "variables")[[i + 1]], newdata)
    if (!is.null(object$call$offset))
      offset <- offset + eval(object$call$offset, newdata)
    mmDone <- FALSE
  }
  n <- length(object$residuals)
  p <- object$rank
  p1 <- seq_len(p)
  piv <- if (p)
    qr.lm(object)$pivot[p1]
  if (p < ncol(X) && !(missing(newdata) || is.null(newdata)))
    warning("prediction from a rank-deficient fit may be misleading")
  beta <- object$coefficients
  predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
  if (!is.null(offset))
    predictor <- predictor + offset
  interval <- match.arg(interval)
  if (interval == "prediction") {
    if (missing(newdata))
      warning("predictions on current data refer to _future_ responses\n")
    if (missing(newdata) && missing(weights)) {
      w <- weights.default(object)
      if (!is.null(w)) {
        weights <- w
        warning("assuming prediction variance inversely proportional to weights used for fitting\n")
      }
    }
    if (!missing(newdata) && missing(weights) && !is.null(object$weights) &&
        missing(pred.var))
      warning("Assuming constant prediction variance even though model fit is weighted\n")
    if (inherits(weights, "formula")) {
      if (length(weights) != 2L)
        stop("'weights' as formula should be one-sided")
      d <- if (missing(newdata) || is.null(newdata))
        model.frame(object)
      else newdata
      weights <- eval(weights[[2L]], d, environment(weights))
    }
  }
  type <- match.arg(type)
  if (se.fit || interval != "none") {
    w <- object$weights
    res.var <- if (is.null(scale)) {
      r <- object$residuals
      rss <- sum(if (is.null(w)) r^2 else r^2 * w)
      df <- object$df.residual
      rss/df
    }
    else scale^2
    if (type != "terms") {
      if (p > 0) {
        XRinv <- if (missing(newdata) && is.null(w))
          qr.Q(qr.lm(object))[, p1, drop = FALSE]
        else X[, piv] %*% qr.solve(qr.R(qr.lm(object))[p1,
                                                       p1])
        ip <- drop(XRinv^2 %*% rep(res.var, p))
      }
      else ip <- rep(0, n)
    }
  }
  if (type == "terms") {
    if (!mmDone) {
      mm <- model.matrix(object)
      mmDone <- TRUE
    }
    aa <- attr(mm, "assign")
    ll <- attr(tt, "term.labels")
    hasintercept <- attr(tt, "intercept") > 0L
    if (hasintercept)
      ll <- c("(Intercept)", ll)
    aaa <- factor(aa, labels = ll)
    asgn <- split(order(aa), aaa)
    if (hasintercept) {
      asgn$"(Intercept)" <- NULL
      avx <- colMeans(mm)
      termsconst <- sum(avx[piv] * beta[piv])
    }
    nterms <- length(asgn)
    if (nterms > 0) {
      predictor <- matrix(ncol = nterms, nrow = NROW(X))
      dimnames(predictor) <- list(rownames(X), names(asgn))
      if (se.fit || interval != "none") {
        ip <- matrix(ncol = nterms, nrow = NROW(X))
        dimnames(ip) <- list(rownames(X), names(asgn))
        Rinv <- qr.solve(qr.R(qr.lm(object))[p1, p1])
      }
      if (hasintercept)
        X <- sweep(X, 2L, avx, check.margin = FALSE)
      unpiv <- rep.int(0L, NCOL(X))
      unpiv[piv] <- p1
      for (i in seq.int(1L, nterms, length.out = nterms)) {
        iipiv <- asgn[[i]]
        ii <- unpiv[iipiv]
        iipiv[ii == 0L] <- 0L
        predictor[, i] <- if (any(iipiv > 0L))
          X[, iipiv, drop = FALSE] %*% beta[iipiv]
        else 0
        if (se.fit || interval != "none")
          ip[, i] <- if (any(iipiv > 0L))
            as.matrix(X[, iipiv, drop = FALSE] %*% Rinv[ii,
                                                        , drop = FALSE])^2 %*% rep.int(res.var,
                                                                                       p)
        else 0
      }
      if (!is.null(terms)) {
        predictor <- predictor[, terms, drop = FALSE]
        if (se.fit)
          ip <- ip[, terms, drop = FALSE]
      }
    }
    else {
      predictor <- ip <- matrix(0, n, 0L)
    }
    attr(predictor, "constant") <- if (hasintercept)
      termsconst
    else 0
  }
  if (interval != "none") {
    tfrac <- qt((1 - level)/2, df)
    hwid <- tfrac * switch(interval, confidence = sqrt(ip),
                           prediction = sqrt(ip + pred.var))
    if (type != "terms") {
      predictor <- cbind(predictor, predictor + hwid %o%
                           c(1, -1))
      colnames(predictor) <- c("fit", "lwr", "upr")
    }
    else {
      if (!is.null(terms))
        hwid <- hwid[, terms, drop = FALSE]
      lwr <- predictor + hwid
      upr <- predictor - hwid
    }
  }
  if (se.fit || interval != "none") {
    se <- sqrt(ip)
    if (type == "terms" && !is.null(terms) && !se.fit)
      se <- se[, terms, drop = FALSE]
  }
  if (missing(newdata) && !is.null(na.act <- object$na.action)) {
    predictor <- napredict(na.act, predictor)
    if (se.fit)
      se <- napredict(na.act, se)
  }
  if (type == "terms" && interval != "none") {
    if (missing(newdata) && !is.null(na.act)) {
      lwr <- napredict(na.act, lwr)
      upr <- napredict(na.act, upr)
    }
    list(fit = predictor, se.fit = se, lwr = lwr, upr = upr,
         df = df, residual.scale = sqrt(res.var))
  }
  else if (se.fit)
    list(fit = predictor, se.fit = se, df = df, residual.scale = sqrt(res.var))
  else predictor
}



#' Generate data to test the aggregress package.
#'
#' \code{gen_agg_lm} generates data to test the \code{aggregress}
#' package. It is an internal function used for testing.
#'
#' @param model a string to denote the model with which
#' to generate the data frame. It can be 'lpm' for a linear
#' proability model, 'lm' for a linear regression model
#' or 'log' for a logistic regression model.
#'
#' @returns a data frame of response variables and covariates for
#' linear regression.
#'
gen_agg_lm <- function(model) {

  if (model == 'lpm') {
    # Linear proability model.

    # Initialize the data frame with covariates.
    aggreg_df <- data.frame(expand.grid(x1 = seq(1,3),
                                        x2 = seq(5,10),
                                        x3 = c(2, 2, 2, 4, 4, 6)))

    # Add an outcome according to a linear probability model.
    # All coefficients are ones.
    aggreg_df[, 'probs'] <- rowSums(aggreg_df[, c('x1', 'x2', 'x3')]) / 20


    # Draw a binary dependent variable.
    aggreg_df[, 'y'] <- as.integer(runif(nrow(aggreg_df)) <=  aggreg_df[, 'probs'])


  } else if (model == 'lm') {

    # Initialize the data frame with covariates.
    aggreg_df <- data.frame(expand.grid(x1 = seq(1,3),
                                        x2 = seq(5,10),
                                        x3 = c(2, 2, 2, 4, 4, 6)))

    # Add an outcome according to a linear probability model.
    # All coefficients are ones.
    aggreg_df[, 'X_beta'] <- rowSums(aggreg_df[, c('x1', 'x2', 'x3')]) / 20


    # Draw a continuous dependent variable.
    aggreg_df[, 'y'] <- aggreg_df[, 'X_beta'] + rnorm(nrow(aggreg_df))


  } else if (model == 'log') {

    # Initialize the data frame with covariates.
    aggreg_df <- data.frame(expand.grid(x1 = seq(1,3),
                                        x2 = seq(5,10),
                                        x3 = c(2, 2, 2, 4, 4, 4, 6, 6, 6)))

    # Add an outcome according to a linear probability model.
    # All coefficients are ones.
    aggreg_df[, 'X_beta'] <- rowSums(aggreg_df[, c('x1', 'x2', 'x3')])*2 - 25

    # Calculate the probabilities.
    aggreg_df[, 'probs'] <- exp(aggreg_df[, 'X_beta'])/(1 + exp(aggreg_df[, 'X_beta']))


    # Draw a binary dependent variable.
    aggreg_df[, 'y'] <- as.integer(runif(nrow(aggreg_df)) <=  aggreg_df[, 'probs'])


  } else {
    stop(sprintf('Model type %s not supported.', model))
  }

  return(aggreg_df)

}
