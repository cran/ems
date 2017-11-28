
#' @import stats
#' @import graphics



propFunnel <- function(unit, o, n, theta, p = c(.95,.998), method = c("exact","normal"), ..., printUnits = FALSE, ylab = "%", xlab = "Volume", ylim = c(0, min(upperCI[[which(p == max(p))]]) + 2.5*theta), xlim = c(0, max(n)), myunits = NULL, digits = 5, overdispersion){

  if(!is.factor(unit)){stop("Unit must be a factor.")}
  if(!is.numeric(n)){stop("n must be numeric.")}
  if (!is.numeric(o)){stop("o must be numeric.")}
  if (!is.numeric(theta)){stop("theta must be numeric.")}
  if (theta < 0 | theta > 1){stop("theta must be between 0 and 1.")}
  if (!is.vector(p)){stop("p must be a vector.")}
  if (method[1] != "normal" && method[1] != "exact"){stop("method must be either 'normal' or 'exact'.")}
  if (!is.logical(printUnits)){stop("printUnits must be TRUE or FALSE.")}
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

  y <- (o / n) * 100
  theta <- theta * 100
  gdetheta <- theta * (100 - theta)
  # n = n precision parameter

  upperCI <- list()
  lowerCI <- list()
  ylowCI <- list()
  yuppCI <- list()
  lowOUT <- list()
  uppOUT <- list()
  outofcontrol <- list()
  outcolname <- c()

  prop.table <- data.frame(unit, y, o, n)
  prop.table <- prop.table[order(prop.table$n),]

  # Calculate the z-score
  z_score <- (y - theta) * sqrt( n / gdetheta)

  # Calculate the 10% and 90% percentiles.
  q90 <- quantile(z_score,probs=c(0.9))
  q10 <- quantile(z_score,probs=c(0.1))

  # Set z-scores larger than the 90% percentile to the 90% percentile.
  z_score <- ifelse(z_score>q90,q90,z_score)

  # Set z-scores smaller than the 10% percentile to the 10% percentile.
  z_score <- ifelse(z_score<q10,q10,z_score)

  # Calculate the Winsorised estimate
  # Used when overdispersion of the indicator
  phi <- (1 / nrow(prop.table)) * sum(z_score ^ 2)

  if (length(exc) > 0){
    prop.table <- prop.table[-which(prop.table$unit %in% exc),]
    if (any(myunits %in% exc)){
      myunits <- myunits[-which(myunits %in% exc)]
    }
  }
  unitnames <- data.frame(Unit = prop.table$unit)
  admissionsRange <- seq(1,max(n))
  observedRange <- seq(1, max(o), length.out = length(admissionsRange))
  prob <- observedRange / admissionsRange

  # using exact binomial approximation
  if (method[1] == "exact"){
    for (i in 1:length(p)){
      rp <- qbinom(p[i], size = admissionsRange, prob)
      alpha <- (pbinom(rp, size = admissionsRange, prob) - p[i]) / ((pbinom(rp, size = admissionsRange, prob)) - pbinom(rp - 1, size = admissionsRange, prob))
      upperCI[[i]] <- theta + ((rp - alpha) / admissionsRange) * 100
      lowerCI[[i]] <- theta - ((rp - alpha) / admissionsRange) * 100
      ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
      yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
      lowOUT[[i]] <- ifelse(prop.table$y < ylowCI[[i]],TRUE,FALSE)
      uppOUT[[i]]<- ifelse(prop.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100,"%CI")
      prop.table <- cbind(prop.table, outofcontrol[[i]])
    }
  } else { # using normal approximation

    if (overdispersion){
      if (phi > (1 + 2 * sqrt( 2 / nrow(prop.table) ))){ # overdispersion = TRUE
      # phi <- (1/nrow(prop.table)) * sum(((y - theta) ^ 2 * (n/gdetheta)))
       warning("The funnel limits were inflated due overdispersion presence.")

      for (i in 1:length(p)){
        zp <- qnorm(1 - (1 - p[i]) / 2)
        upperCI[[i]] <- theta + zp * sqrt(gdetheta * phi / admissionsRange)
        lowerCI[[i]] <- theta - zp * sqrt(gdetheta * phi / admissionsRange)
        ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
        yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
        lowOUT[[i]] <- ifelse(prop.table$y < ylowCI[[i]],TRUE,FALSE)
        uppOUT[[i]]<- ifelse(prop.table$y > yuppCI[[i]], TRUE, FALSE)
        outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        outcolname[i] <- paste0(p[i]*100,"%CI")
        prop.table <- cbind(prop.table, outofcontrol[[i]])
      }
    }
  } else {
      for (i in 1:length(p)){
        zp <- qnorm(1 - (1 - p[i]) / 2)
        upperCI[[i]] <- theta + zp * sqrt(gdetheta / admissionsRange)
        lowerCI[[i]] <- theta - zp * sqrt(gdetheta / admissionsRange)
        ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
        yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
        lowOUT[[i]] <- ifelse(prop.table$y < ylowCI[[i]],TRUE,FALSE)
        uppOUT[[i]]<- ifelse(prop.table$y > yuppCI[[i]], TRUE, FALSE)
        outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        outcolname[i] <- paste0(p[i]*100,"%CI")
        prop.table <- cbind(prop.table, outofcontrol[[i]])
      }
    }
  }

  x <- prop.table$n; y <- prop.table$y; range <- admissionsRange
  output <- list(x = x, y = y, theta = theta, range = range, tab = prop.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = myunits, p = p, unitnames = unitnames, printUnits = printUnits, phi = phi)
  colnames(output$tab) <- c("Unit","y(%)", "Observed","Admissions", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2] <- round(output$tab[,2], digits)
  output
}
