
#' @import stats
#' @import graphics


changePropFunnel <- function(unit, o1, o2, n1, n2, p = c(.95,.998), pi1 = sum(o1)/sum(n1), pi2 = sum(o2)/sum(n2), method = c("diff","ratio"), ..., printUnits = FALSE, xlab = "Sample size per period", auto.ylab = TRUE, ylab = c("Proportions difference","Proportions ratio log"), ylim = c(max(lowerCI[[which(p == max(p))]]) - 6*theta, min(upperCI[[which(p == max(p))]]) + 6*theta), xlim = c(0,max(rho)), myunits = NULL, digits = 5){

  if(!is.factor(unit)){stop("Unit must be a factor.")}
  if(!is.numeric(n1)){stop("n1 must be numeric.")}
  if(!is.numeric(n2)){stop("n2 must be numeric.")}
  if (!is.numeric(o1)){stop("o1 must be numeric.")}
  if (!is.numeric(o2)){stop("o2 must be numeric.")}
  if (!is.logical(printUnits)){stop("printUnits must be TRUE or FALSE.")}
  if (!is.logical(auto.ylab)){stop("auto.ylab must be TRUE or FALSE.")}
  if (method[1] != "diff" && method[1] != "ratio"){stop("method must be either 'diff' or 'ratio'.")}
  exc <- NULL
  if (any(n1 == 0 | n2 == 0)){
    exc <- unit[which(n1 == 0 | n2 == 0)]
    warning(paste0("The following units were excluded due to absence of observations: ", exc, "\n"))
  }
  if (any(! myunits %in% unit)){
    warning(paste0("There is no unit called ", myunits[which(! myunits %in% unit)], "\n"))
    myunits <- myunits[-which(! myunits %in% unit)]
  }

  if (any(o1 == 0)){o1 <- o1 + .5} # To don't generate NaN values.
  if (any(o2 == 0)){o2 <- o2 + .5}
  if (any(n1 == 0)){n1 <- n1 + 1}
  if (any(n2 == 0)){n2 <- n2 + 1}

  upperCI <- list()
  lowerCI <- list()
  ylowCI <- list()
  yuppCI <- list()
  lowOUT <- list()
  uppOUT <- list()
  outofcontrol <- list()
  outcolname <- c()

  if (method[1] == "diff"){
    secondcolname <- "y"
    y <- (o2/n2) - (o1/n1)
    theta <- pi2 - pi1
    pim <- (pi2 + pi1) / 2 # mean proportion
    vary <- ((pim + (theta / 2)) * (1 - pim - (theta / 2))) / n2 + ((pim - (theta / 2)) * (1 - pim + (theta / 2))) / n1
    gdetheta <- ((pim + (theta / 2)) * (1 - pim - (theta / 2))) + ((pim - (theta / 2)) * (1 - pim + (theta / 2)))
    rho <- gdetheta / vary
    change.table <- data.frame(unit,y,o1,n1,o2,n2,rho)
    change.table <- change.table[order(change.table$rho),]
    if (length(exc) > 0){
      change.table <- change.table[-which(change.table$unit %in% exc),]
      if (any(myunits %in% exc)){
        myunits <- myunits[-which(myunits %in% exc)]
      }
    }
    unitnames <- data.frame(Unit = change.table$unit)
    expectedRange <- seq(0, max(change.table$rho)+5)

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow repeted values in xCI
      if (ceiling(change.table$rho)[i] == ceiling(change.table$rho)[i+1]){
        change.table$rho[i+1] <- change.table$rho[i+1] + 1
      }
    }
    for (i in 1:length(p)){
      zp <- qnorm(1 - (1 - p[i]) / 2)
      upperCI[[i]] <- theta + zp * sqrt(gdetheta / expectedRange)
      lowerCI[[i]] <- theta - zp * sqrt(gdetheta / expectedRange)
      ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      lowOUT[[i]] <- ifelse(change.table$y < ylowCI[[i]],TRUE, FALSE)
      uppOUT[[i]] <- ifelse(change.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100,"%CI")
      change.table <- cbind(change.table, outofcontrol[[i]])
    }
    if (auto.ylab){ylab = ylab[1]}
  } else {
    # logarithmic scale!!
    secondcolname <- "log(y)"
    y <- (o2/n2) / (o1/n1)
    theta <- pi2 / pi1
    pig <- sqrt(pi2 * pi1)
    varlogy <- ((theta ^ (-1/2) - pig) / (n2*pig)) + (( theta ^ (1/2) - pig) / (n1*pig))
    gdetheta <- (theta ^ (-1/2) + theta ^ (1/2) - 2 * pig) / pig
    rho <- gdetheta / varlogy
    change.table <- data.frame(unit,"y" = log(y),o1,n1,o2,n2,rho)
    change.table <- change.table[order(change.table$rho),]
    if (length(exc) > 0){
      change.table <- change.table[-which(change.table$unit %in% exc),]
      if (any(myunits %in% exc)){
        myunits <- myunits[-which(myunits %in% exc)]
      }
    }
    unitnames <- data.frame(Unit = change.table$unit)
    expectedRange <- seq(0, max(change.table$rho)+5)

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow more than 2 repeted values in xCI
      if (any(ceiling(change.table$rho)[(i+1):length(change.table$rho)] == ceiling(change.table$rho)[i])){
        change.table$rho[i] <- change.table$rho[i] + 1
      }
    }

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow 2 repeted values in xCI
      if (ceiling(change.table$rho)[i] == ceiling(change.table$rho)[i+1]){
        change.table$rho[i+1] <- change.table$rho[i+1] + 1
      }
    }

    for (i in 1:length(p)){
      zp <- qnorm(1 - (1 - p[i]) / 2)
      upperCI[[i]] <- log(theta) + zp * sqrt(gdetheta / expectedRange)
      lowerCI[[i]] <- log(theta) - zp * sqrt(gdetheta / expectedRange)
      ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      lowOUT[[i]] <- ifelse(change.table$y < ylowCI[[i]],TRUE, FALSE)
      uppOUT[[i]]<- ifelse(change.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100,"%CI")
      change.table <- cbind(change.table, outofcontrol[[i]])
    }
    if (auto.ylab){ylab = ylab[2]}

    theta <- log(theta)
  }

  x <- change.table$rho; y <- change.table$y; range <- expectedRange
  output <- list(x = x, y = y, theta = theta, range = range, tab = change.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = myunits, p = p, unitnames = unitnames, printUnits = printUnits)
  colnames(output$tab) <- c("Unit",secondcolname, "Obs1", "N1", "Obs2","N2","rho", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2:7] <- round(output$tab[,2:7], digits)
  output
}
