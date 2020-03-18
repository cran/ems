#' @title Survival models performance analysis
#'
#' @name survPerformance
#'
#' @description Colection of functions for survival models performance analysis.
#'
#' \code{R2sh} estimates a distance-based estimator of survival predictive accuracy proposed by Schemper and Henderson. It was inspirated in survAUC::schemper function, but receives the predicted values directly. Besides that, \code{R2sh} does bootstrap resampling and returns its confidence interval estimate.
#'
#'
#' \code{R2pm} calculates a estimator of survival predictive accuracy proposed by Kent & O'Quigley and its bootstrap confidence interval.
#'
#'
#' \code{cal.Slope} returns the calibration slope of a survival model and its bootstrap confidence interval.
#'
#' @param time A vector of event times.
#' @param status A indicator vector of event occurrence.
#' @param lin.pred A vector of linear predictors of a survival model for each observation. (prognostic index)
#' @param data A data.frame where to find column vectors.
#' @param R The number of bootstrap replicates. Usually this will be a single positive integer. For importance resampling, some resamples may use one set of weights and others use a different set of weights. In this case R would be a vector of integers where each component gives the number of resamples from each of the rows of weights. To be passed to \code{\link[boot]{boot}}.
#'
#' @return
#'
#' \code{R2sh} returns a list with the following components:
#' \itemize{
#' \item \code{D}: The estimator of predictive accuracy obtained from the covariate-free null model.
#' \item \code{Dx}: The estimator of predictive accuracy obtained from the Cox model.
#' \item \code{V}: The estimator of relative gains in predictive accuracy.
#' \item \code{Mhat}: The absolute distance estimator obtained from the   Cox model (evaluated at the event times of the test data).
#' \item \code{Mhat.0}: The absolute distance estimator obtained from the covariate-free null model (evaluated at the event times of the test data).
#' \item \code{timep}: The event times of the test data.
#' \item \code{lower}: V lower confidence limit.
#' \item \code{upper}: V upper confidence limit.
#' \item \code{boot}: An object of class "\code{\link[boot]{boot}}".
#' \item \code{bootCI}: Boot confidence intervals resampling.
#' }
#'
#' \code{R2pm} returns a list with the following components:
#' \itemize{
#' \item \code{r2}: The estimator of predictive accuracy obtained from the Cox model.
#' \item \code{lower}: r2 lower confidence limit.
#' \item \code{upper}: r2 upper confidence limit.
#' \item \code{boot}: An object of class "\code{\link[boot]{boot}}".
#' \item \code{bootCI}: Boot confidence intervals resampling.
#' }
#'
#' \code{cal.Slope} returns a list with the following components:
#' \itemize{
#' \item \code{slope}: The calibration slope measure of a survival model.
#' \item \code{lower}: slope lower confidence limit.
#' \item \code{upper}: slope upper confidence limit.
#' \item \code{boot}: An object of class "\code{\link[boot]{boot}}".
#' \item \code{bootCI}: Boot confidence intervals resampling.
#' }
#'
#' @references
#' Schemper, M. and R. Henderson (2000).
#' Predictive accuracy and explained variation in Cox regression.
#' Biometrics 56, 249-255.
#'
#' Davison, A.C. and Hinkley, D.V. (1997) Bootstrap Methods and Their Application, Chapter 5. Cambridge University Press.
#'
#' DiCiccio, T.J. and Efron B. (1996) Bootstrap confidence intervals (with Discussion). Statistical Science, 11, 189-228.
#'
#' Efron, B. (1987) Better bootstrap confidence intervals (with Discussion). Journal of the American Statistical Association, 82, 171-200.
#'
#' Kent, John T., and J. O. H. N. O'QUIGLEY. "Measures of dependence for censored survival data." Biometrika 75.3 (1988): 525-534.
#'
#' van Houwelingen, Hans C. "Validation, calibration, revision and combination of prognostic survival models." Statistics in medicine 19.24 (2000): 3401-3415.
#'
#' Rahman, M. Shafiqur, et al. "Review and evaluation of performance measures for survival prediction models in external validation settings." BMC medical research methodology 17.1 (2017): 60.

#'
#' @author Lunna Borges <lunna.borges@epimedsolutions.com>
#'
#' @examples
#'
#' #### Survival model ####
#'
#' data(breastCancer)
#' class(breastCancer$gradd1) <- "character"
#' class(breastCancer$gradd2) <- "character"
#'
#' traindata <- breastCancer[sample(nrow(breastCancer), nrow(breastCancer)*2/3),]
#' newdata <- breastCancer[-sample(nrow(breastCancer), nrow(breastCancer)*2/3),]
#' model <- rms::cph(survival::Surv(rectime, censrec) ~ rms::rcs(age,6) +
#'  rms::rcs(nodes,3) + rms::rcs(pgr,3) + gradd1 + gradd2 +
#'  hormon, data = traindata)
#'
#' lp <- predict(model, newdata = newdata)
#'
#' #### R2sh  example ####
#'
#' R2sh(newdata$rectime, newdata$censrec, lp, data = newdata, R = 50)
#'
#' #### R2pm example ####
#'
#' R2pm(lp, R = 50)
#'
#' #### cal.slope example ####
#'
#' cal.Slope(newdata$rectime, newdata$censrec, lp, R = 50)
#'
#' @import survival
#' @import boot
#' @import rms
#' @export

R2sh <- function(time, status, lin.pred, data, R){

  f.Mt <- function(tempo, tutti.tempi, stima.surv, tempi.evento,
                   Stj, ind.censura, num.sogg) {
    Stj1 <- unique(Stj[tempi.evento == tempo])
    primo <- rep(1 - Stj1, num.sogg)
    primo[tutti.tempi <= tempo] <- 0
    secondo <- Stj1 * (1 - ind.censura)
    secondo[tutti.tempi > tempo] <- 0
    terzo <- ind.censura * (((1 - Stj1) * Stj1)/stima.surv +
                              Stj1 * (1 - Stj1/stima.surv))
    terzo[tutti.tempi > tempo] <- 0
    terzo[is.na(terzo)] <- 0
    ris <- primo + secondo + terzo
    return(sum(ris)/num.sogg)
  }

  f.Mt.cox <- function(tempo, tutti.tempi, stima.surv, tempi.evento,
                       Stj0, ind.censura, num.sogg, lin.pred) {
    Stj00 <- unique(Stj0[tempi.evento == tempo])
    Stj1 <- Stj00^exp(lin.pred)
    primo <- 1 - Stj1
    primo[tutti.tempi <= tempo] <- 0
    secondo <- Stj1 * (1 - ind.censura)
    secondo[tutti.tempi > tempo] <- 0
    terzo <- ind.censura * (((1 - Stj1) * Stj1)/stima.surv +
                              Stj1 * (1 - Stj1/stima.surv))
    terzo[tutti.tempi > tempo] <- 0
    terzo[is.na(terzo)] <- 0
    ris <- primo + secondo + terzo
    return(sum(ris)/num.sogg)
  }

  f.assegna.surv <- function(tempo, tempi.eventi) {
    if (any(tempo == tempi.eventi)) {
      pos <- (c(1:length(tempi.eventi)) * as.numeric(tempo ==
                                                       tempi.eventi))
      pos <- pos[pos != 0]
    }
    else {
      tmp <- (tempo - tempi.eventi)
      if (all(tmp < 0))
        pos <- NA
      else {
        tmp[tmp < 0] <- Inf
        pos <- order(tmp)[1]
      }
    }
    return(pos)
  }

  bootData <- data.frame(time, status, lin.pred)

  R2boot <- function(x,indices){

    x = x[indices,]

    tsurv <- as.numeric(x$time)
    surv <- as.numeric(x$status)
    lin.pred <- x$lin.pred
    num.sogg <- length(tsurv)
    km <- survfit(Surv(tsurv, surv) ~ 1)
    km.fit <- survfit(Surv(time, status) ~ 1, data = x)
    tempi.eventi <- km$time[km$n.event != 0]
    pos.surv <- apply(as.matrix(tsurv), 1, f.assegna.surv, tempi.eventi)
    surv.tj <- approx(km.fit$time, km.fit$surv, xout = tempi.eventi,
                      method = "constant", f = 0, yleft = 1, yright =
                        min(km.fit$surv, na.rm = T))$y
    surv.tot.km <- (surv.tj)[pos.surv]
    ind.censura <- as.numeric(!as.logical(surv))
    Mt <- apply(as.matrix(tempi.eventi), 1, f.Mt, tsurv, surv.tot.km,
                tempi.eventi, surv.tj, ind.censura, num.sogg)

    numero.eventi <- km$n.event[km$n.event != 0]
    surv0.tj.cox <- approx(km.fit$time, km.fit$surv, xout = tempi.eventi, method = "constant", f = 0, yleft = 1, yright = min(km.fit$surv, na.rm = T))$y
    surv0.tot.cox <- (surv0.tj.cox)[pos.surv]
    surv.tot.cox <- surv0.tot.cox^exp(lin.pred)
    Mtx <- apply(as.matrix(tempi.eventi), 1, f.Mt.cox, tsurv,
                 surv.tot.cox, tempi.eventi, surv0.tj.cox, ind.censura,
                 num.sogg, lin.pred)

    Gkm <- survfit(Surv(tsurv, ind.censura) ~ 1)
    tempi.censure <- Gkm$time[Gkm$n.event != 0]
    if (!length(tempi.censure)) {
      cens.tot.km <- rep(1, length(tempi.eventi))
    } else {
      pos.surv.censure <- apply(as.matrix(tempi.eventi), 1, f.assegna.surv, tempi.censure)
      cens.tot.km <- (Gkm$surv[Gkm$n.event != 0])[pos.surv.censure]
      cens.tot.km[tempi.eventi < min(Gkm$time[Gkm$n.event != 0])] <- 1
    }
    pesi <- numero.eventi/cens.tot.km
    peso.tot <- sum(pesi)
    D <- sum(Mt * pesi)/peso.tot
    Dx <- sum(Mtx * pesi)/peso.tot
    V <- (D - Dx)/D

    bootOutput <- V

    return(bootOutput)

  }

  bootR2 <- boot::boot(bootData, R2boot, R)
  CI <- boot::boot.ci(bootR2, type = "basic")

  tsurv <- as.numeric(time)
  surv <- as.numeric(status)
  lin.pred <- lin.pred
  num.sogg <- length(tsurv)
  km <- survfit(Surv(tsurv, surv) ~ 1)
  km.fit <- survfit(Surv(time, status) ~ 1, data = data)
  tempi.eventi <- km$time[km$n.event != 0]
  pos.surv <- apply(as.matrix(tsurv), 1, f.assegna.surv, tempi.eventi)
  surv.tj <- approx(km.fit$time, km.fit$surv, xout = tempi.eventi,
                    method = "constant", f = 0, yleft = 1, yright =
                    min(km.fit$surv, na.rm = T))$y
  surv.tot.km <- (surv.tj)[pos.surv]
  ind.censura <- as.numeric(!as.logical(surv))
  Mt <- apply(as.matrix(tempi.eventi), 1, f.Mt, tsurv, surv.tot.km,
              tempi.eventi, surv.tj, ind.censura, num.sogg)



  numero.eventi <- km$n.event[km$n.event != 0]
  surv0.tj.cox <- approx(km.fit$time, km.fit$surv, xout = tempi.eventi, method = "constant", f = 0, yleft = 1, yright = min(km.fit$surv, na.rm = T))$y
  surv0.tot.cox <- (surv0.tj.cox)[pos.surv]
  surv.tot.cox <- surv0.tot.cox^exp(lin.pred)
  Mtx <- apply(as.matrix(tempi.eventi), 1, f.Mt.cox, tsurv,
               surv.tot.cox, tempi.eventi, surv0.tj.cox, ind.censura,
               num.sogg, lin.pred)

  Gkm <- survfit(Surv(tsurv, ind.censura) ~ 1)
  tempi.censure <- Gkm$time[Gkm$n.event != 0]
  if (!length(tempi.censure)) {
    cens.tot.km <- rep(1, length(tempi.eventi))
  } else {
    pos.surv.censure <- apply(as.matrix(tempi.eventi), 1, f.assegna.surv, tempi.censure)
    cens.tot.km <- (Gkm$surv[Gkm$n.event != 0])[pos.surv.censure]
    cens.tot.km[tempi.eventi < min(Gkm$time[Gkm$n.event != 0])] <- 1
  }
  pesi <- numero.eventi/cens.tot.km
  peso.tot <- sum(pesi)
  D <- sum(Mt * pesi)/peso.tot
  Dx <- sum(Mtx * pesi)/peso.tot
  V <- (D - Dx)/D

  output <- list(D = D, Dx = Dx, V = V,
                 Mhat = Mtx, Mhat.0 = Mt, timep = tempi.eventi,
                 lower = CI$basic[4], upper = CI$basic[5],
                 boot = bootR2, bootCI = CI)
  return(output)
}

#' @rdname survPerformance
#' @export

R2pm <- function(lin.pred, R){

  bootData <- data.frame(lin.pred)

  sigma.episilon = (pi ^ 2) / 6 # variancia do erro de um modelo Weibull equivalente

  R2boot <- function(x,indices){

    sigma.episilon = (pi ^ 2) / 6
    bootOutput <-  var(x[indices,])/(var(x[indices,]) + sigma.episilon)

    return(bootOutput)
  }


  bootR2 <- boot::boot(bootData, R2boot, R)
  CI <- boot::boot.ci(bootR2, type = "basic")

  # sugerem uma recalibracao do modelo antes desse calculo, mas no momento n?o me ? possivel
  output <- list( r2 = var(lin.pred)/(var(lin.pred) + sigma.episilon), lower = CI$basic[4], upper = CI$basic[5], boot = bootR2, bootCI = CI)

  return(output)

}

#' @rdname survPerformance
#' @export

cal.Slope <- function(time, status, lin.pred, R){

# bootstrap data
bootData <- data.frame(time, status, lin.pred)

fitCoef <- function(x, indices){

  bootFit <- rms::cph(Surv(time,status) ~ lin.pred, data = x[indices,])
  bootOutput <- as.numeric(bootFit$coef)

  return(bootOutput)
}

fitCoefBoot <-boot:: boot(bootData, fitCoef, R)
CI <- boot::boot.ci(fitCoefBoot, type = "basic")

fit <- rms::cph(Surv(time,status) ~ lin.pred)
output <- list(slope = as.numeric(fit$coef), lower = CI$basic[4], upper = CI$basic[5], boot = fitCoefBoot, bootCI = CI)

return(output)
}
