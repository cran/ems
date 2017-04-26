
#' @import stats
#' @import graphics


rateFunnel <- function(unit, y, n, o, e, y.type = c("SMR","SRU"), p = c(.95,.998), theta = 1, method = c("exact","normal"), direct = FALSE, ..., printUnits = FALSE, auto.xlab = TRUE, xlab = c("Volume of cases","Expected values"), ylab = y.type[1], xlim = c(0, max(rho)), ylim = c(min(lowerCI[[which(p == max(p))]]), max(upperCI[[which(p == max(p))]])), myunits = NULL, overdispersion = FALSE, digits = 5){

  if(!is.factor(unit)){stop("Unit must be a factor.")}
  if (!is.numeric(y)){stop("y must be numeric.")}
  if (!is.logical(direct)){stop("direct must be TRUE or FALSE.")}
  if (direct){
    if (!is.numeric(n)){stop("n must be numeric.")}
    if (!is.numeric(o)){stop("o must be numeric.")}
  } else{
    if (!is.numeric(e)){stop("e must be numeric.")}
  }
  if (!is.numeric(theta)){stop("theta must be numeric.")}
  if (!is.numeric(p)){stop("p must be a numeric vector.")}
  if (!is.vector(p)){stop("p must be a vector.")}
  if (method[1] != "normal" && method[1] != "exact"){stop("method must be either 'normal' or 'exact'.")}
  if (y.type[1] != "SMR" && y.type[1] != "SRU"){stop("y.type must be either 'SMR' or 'SRU'.")}
  if (!is.logical(printUnits)){stop("printUnits must be TRUE or FALSE.")}
  # if (!is.logical(plot)){stop("plot must be TRUE or FALSE.")}
  if (!is.logical(overdispersion)){stop("overdispersion must be TRUE or FALSE.")}
  exc <- NULL
  if (any(n == 0)){
    exc <- unit[which(n == 0)]
    warning(paste0("The following units were excluded due to absence of observations: ", exc, "\n"))
  }
  if (any(! myunits %in% unit)){
    warning(paste0("There is no unit called ", myunits[which(! myunits %in% unit)], "\n"))
    myunits <- myunits[-which(! myunits %in% unit)]
  }

  if (direct){
    rates.table <- data.frame(unit, Rate = y, Admissions = n, Observed = o)
    thirdcolname <-  "Admissions"
  } else {
    rates.table <- data.frame(unit, Rate = y, Expected = e, Observed = o)
    thirdcolname <- "Expected"
  }

  if (length(exc) > 0){
    rates.table <- rates.table[-which(rates.table$unit %in% exc),]
    if (any(myunits %in% exc)){
      myunits <- myunits[-which(myunits %in% exc)]
    }
  }

  upperCI <- list()
  lowerCI <- list()
  ylowCI <- list()
  yuppCI <- list()
  lowOUT <- list()
  uppOUT <- list()
  outofcontrol <- list()
  outcolname <- c()

  if (direct){
    rates.table <-  rates.table[order(rates.table$Admissions),] # ordering by total of admissions
    smr <- rates.table$Rate
    admissions <- rates.table$Admissions
    observed <- rates.table$Observed
    admissionsRange <- seq(1, max(admissions))
    observedRange <- seq(1, max(observed),length.out = length(admissionsRange))
    rho <- admissions
    unitnames <- data.frame(Unit = rates.table$unit)

    if (auto.xlab){ xlab = xlab[1] }

    # Exact formula using binomial approximation
    if (method[1] == "exact"){
      prob <- observedRange/admissionsRange
      for (i in 1:length(p)){
        rp <- qbinom(p[i], size = admissionsRange, prob)
        alpha <- (pbinom(rp, size = admissionsRange, prob) - p[i]) / ((pbinom(rp, size = admissionsRange, prob)) - pbinom(rp - 1, size = admissionsRange, prob))
        upperCI[[i]] <- theta + (rp - alpha) / admissionsRange
        lowerCI[[i]] <- theta - (rp - alpha) / admissionsRange
        ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% admissions == TRUE)]
        yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% admissions == TRUE)]
        lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE,FALSE)
        uppOUT[[i]]<- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
        outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        outcolname[i] <- paste0(p[i]*100,"%CI")
      }
        rates.table <- cbind(rates.table, outofcontrol)
        x <- admissions; y <- smr; range <- admissionsRange
    }
    if (method[1] == "normal"){
      if (any(admissions < 100)){
        warning("Normal distribuition is not a good approximation for units whose has less than 100 admissions. There are ", length(which(admissions < 100)), " units with less than 100 admissions")
      }
      if (overdispersion){
        phi <- (1/nrow(rates.table)) * sum(((y - theta) ^ 2 * admissions)/(theta))
        for (i in 1:length(p)){
          zp <- qnorm(1 - (1 - p[i]) / 2)
          upperCI[[i]] <- theta + zp * sqrt(theta * phi / admissionsRange)
          lowerCI[[i]] <- theta - zp * sqrt(theta * phi / admissionsRange)
          ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% admissions == TRUE)]
          yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% admissions == TRUE)]
          lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
          uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
          outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        }
      } else {
        for (i in 1:length(p)){
          zp <- qnorm(1 - (1 - p[i]) / 2)
          upperCI[[i]] <- theta + zp * sqrt(theta / admissionsRange)
          lowerCI[[i]] <- theta - zp * sqrt(theta / admissionsRange)
          ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% admissions == TRUE)]
          yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% admissions == TRUE)]
          lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
          uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
          outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
          outcolname[i] <- paste0(p[i]*100,"%CI")
        }
      }
        rates.table <- cbind(rates.table, outofcontrol)
        x <- admissions; y <- smr; range <- admissionsRange
    }
  } else {
    rates.table <-  rates.table[order(rates.table$Expected),] # ordering by expected death
    smr <- rates.table$Rate
    e <- rates.table$Expected
    expectedRange <- seq(1, max(e)) #using expected death as precision parameter
    rho <- e
    unitnames <- data.frame(Unit = rates.table$unit)
    if (auto.xlab){ xlab = xlab[2]}
  # "exact" formula using poisson approximation
    if (method[1] == "exact"){
      lambda <- theta*expectedRange
      for (i in 1:length(p)){
        rp <- qpois(p[i], lambda)
        alpha <- (ppois(rp, lambda) - p[i]) / (ppois(rp, lambda) - ppois(rp - 1, lambda))
        upperCI[[i]] <- theta + (rp - alpha)/expectedRange
        lowerCI[[i]] <- theta - (rp - alpha)/expectedRange
        ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
        yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
        lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
        uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
        outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        outcolname[i] <- paste0(p[i]*100,"%CI")
      }
        rates.table <- cbind(rates.table, outofcontrol)
        x <- e; y <- smr; range <- expectedRange
    }
  # Normal approximation
    if (method[1] == "normal"){
      if (any(e < 100)){
        warning("Normal distribuition is not a good approximation for units whose has less than 100 expected values. There are ", length(which(e < 100)), " units with less than 100 expected values")
      }
      if (overdispersion){
        phi <- (1/nrow(rates.table)) * sum(((y - theta) ^ 2 * e)/(theta))
        for (i in 1:length(p)){
          zp <- qnorm(1 - (1 - p[i]) / 2)
          upperCI[[i]] <- theta + zp * sqrt(theta * phi / expectedRange)
          lowerCI[[i]] <- theta - zp * sqrt(theta * phi / expectedRange)
          ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
          yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
          lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
          uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
          outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
          outcolname[i] <- paste0(p[i]*100,"%CI")
        }
      } else {
      for (i in 1:length(p)){
          zp <- qnorm(1 - (1 - p[i]) / 2)
          upperCI[[i]] <- theta + zp * sqrt(theta/expectedRange)
          lowerCI[[i]] <- theta - zp * sqrt(theta/expectedRange)
          ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
          yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
          lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
          uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
          outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        outcolname[i] <- paste0(p[i]*100,"%CI")
      }
    }
      rates.table <- cbind(rates.table, outofcontrol)
      x <- e; y <- smr; range <- expectedRange
  }
 }

  output <- list(x = x, y = y, theta = theta, range = range, tab = rates.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = myunits, p = p, unitnames = unitnames, printUnits = printUnits)
  colnames(output$tab) <- c("Unit", y.type[1], thirdcolname, "Observed", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2] <- round(output$tab[,2], digits)
  output
}
