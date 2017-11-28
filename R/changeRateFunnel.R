
#' @import stats
#' @import graphics


changeRateFunnel <- function(unit, n1, n2, o1, e1, o2, e2, lambda1 = sum(o1)/sum(n1), lambda2 = sum(o2)/sum(n2), y.type = c("SMR","SRU"), p = c(.95,.998), ..., printUnits = FALSE, auto.xlab = TRUE, xlab = c("Average observed count","Expectation per period"), auto.ylab = TRUE, ylab = c(paste0(y.type[1],"'s Ratio"),paste0("Log(", y.type[1],"'s Ratio)")), ylim = c(max(lowerCI[[which(p == max(p))]]) - 1.5*theta, min(upperCI[[which(p == max(p))]]) + 1.5*theta), xlim = c(0,max(rho)), myunits = NULL, digits = 5, overdispersion){

  if(!is.factor(unit)){stop("Unit must be a factor.")}
  if(!is.numeric(n1)){stop("n1 must be numeric.")}
  if(!is.numeric(n2)){stop("n2 must be numeric.")}
  if (!is.numeric(o1)){stop("o1 must be numeric.")}
  if (!is.numeric(o2)){stop("o2 must be numeric.")}
  if (!is.numeric(e1)){stop("e1 must be numeric.")}
  if (!is.numeric(e2)){stop("e2 must be numeric.")}
  if (y.type[1] != "SMR" && y.type[1] != "SRU"){
    stop("y.type must be either 'SMR' or 'SRU'.")
    }
  if (!is.vector(p)){stop("p must be a vector.")}
  if (!is.logical(printUnits)){stop("printUnits must be TRUE or FALSE.")}
  if (!is.logical(overdispersion)){stop("overdispersion must be TRUE or FALSE.")}
  if (!is.logical(auto.xlab)){stop("auto.xlab must be TRUE or FALSE.")}
  if (!is.logical(auto.ylab)){stop("auto.ylab must be TRUE or FALSE.")}
  exc <- NULL
  if (any(n1 == 0 | n2 == 0)){
    exc <- unit[which(n1 == 0 | n2 == 0)]
    warning(paste0("The following unit were excluded due to absence of observations: ", exc, "\n"))
  }
  if (any(! myunits %in% unit)){
    warning(paste0("There is no unit called ", myunits[which(! myunits %in% unit)], "\n"))
    myunits <- myunits[-which(! myunits %in% unit)]
  }

  if (any(o1 == 0)){o1 <- o1 + .5} #To don't generate NaN values.
  if (any(o2 == 0)){o2 <- o2 + .5}
  if (any(e1 == 0)){e1 <- e1 + .5}
  if (any(e2 == 0)){e2 <- e2 + .5}

  y <- (o2/e2)/(o1/e1)
  theta <- lambda2/lambda1
  upperCI <- list()
  lowerCI <- list()
  ylowCI <- list()
  yuppCI <- list()
  lowOUT <- list()
  uppOUT <- list()
  outofcontrol <- list()
  outcolname <- c()
  phi <- NULL # will be calculated only if overdispersion = TRUE and e1 != e2

  if (all.equal(e1, e2) == TRUE){
    # then y = o2/o1
    # exact methods
    secondcolname <- "y"
    rho <- (o1 + o2)/2  # precision parameter: average observed count
    change.table <- data.frame(unit,y,o1,e1,n1,o2,e2,n2,rho)
    change.table <- change.table[order(change.table$rho),]
    if (length(exc) > 0){
      change.table <- change.table[-which(change.table$unit %in% exc),]
      if (any(myunits %in% exc)){
        myunits <- myunits[-which(myunits %in% exc)]
      }
    }
    unitnames <- data.frame(Unit = change.table$unit)
    expectedRange <- seq(1, max(change.table$rho)+5)
    lambda <- theta*expectedRange

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow repeted values in xCI
      if (ceiling(change.table$rho)[i] == ceiling(change.table$rho)[i+1]){
        change.table$rho[i+1] <- change.table$rho[i+1] + 1
      }
    }
    for (i in 1:length(p)){
      rp <- qpois(p[i], lambda)
      alpha <- (ppois(rp, lambda) - p[i]) / (ppois(rp, lambda) - ppois(rp - 1, lambda))
      upperCI[[i]] <- theta + (rp - alpha) / expectedRange
      lowerCI[[i]] <- theta - (rp - alpha) / expectedRange
      ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
      lowOUT[[i]] <- ifelse(change.table$y < ylowCI[[i]], TRUE, FALSE)
      uppOUT[[i]]<- ifelse(change.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100, "%CI")
      change.table <- cbind(change.table, outofcontrol[[i]])
    }
    if (auto.xlab){xlab = xlab[1]}
    if (auto.ylab){ylab = ylab[1]}
  } else {
    # normal approximation
    # logarithmic scale!!
      secondcolname <- "log(y)"
      lambdaghat <- (o1 + o2) / (theta ^ (1/2) * e2 + theta ^ (-1/2) * e1)
      lambdagbarra <- (sum(o1) + sum(o2)) / (sum(e1) + sum(e2)) #assuming e1 = e2 = e, then varlogy|lambdagbarra = gdethetha / e
      varlogy <- ((theta ^ (-1/2)) / (e2* lambdaghat)) + ((theta ^ (1/2)) / (e1* lambdaghat))
      gdetheta <- (theta ^ (-1/2) + theta ^ (1/2)) / lambdagbarra
      rho <- gdetheta / varlogy # precision parameter
      change.table <- data.frame(unit,"y" = log(y),o1,e1,n1,o2,e2,n2,rho)
      change.table <- change.table[order(change.table$rho),]
      if (length(exc) > 0){
        change.table <- change.table[-which(change.table$unit %in% exc),]

        if (any(myunits %in% exc)){
          myunits <- myunits[-which(myunits %in% exc)]
        }
      }
      unitnames <- data.frame(Unit = change.table$unit)
      expectedRange <- seq(1, max(change.table$rho)+5)

      # Calculate the z-score
      z_score <- (y - theta) * sqrt(rho / gdetheta)

      # Calculate the 10% and 90% percentiles.
      q90 <- quantile(z_score,probs=c(0.9))
      q10 <- quantile(z_score,probs=c(0.1))

      # Set z-scores larger than the 90% percentile to the 90% percentile.
      z_score <- ifelse(z_score>q90,q90,z_score)

      # Set z-scores smaller than the 10% percentile to the 10% percentile.
      z_score <- ifelse(z_score<q10,q10,z_score)

      # Calculate the Winsorised estimate
      # Used when overdispersion of the indicator
      phi <- (1 / nrow(change.table)) * sum(z_score ^ 2)

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow repeted values in xCI
      if (ceiling(change.table$rho)[i] == ceiling(change.table$rho)[i+1]){
        change.table$rho[i+1] <- change.table$rho[i+1] + 1
      }
    }
      if (overdispersion){

        print(paste0("phi = ", phi))

        if(phi > (1 + 2 * sqrt( 2 / nrow(change.table) ))){

          warning("The funnel limits were inflated due overdispersion presence.")

          for (i in 1:length(p)){
            zp <- qnorm(1 - (1 - p[i]) / 2)
            upperCI[[i]] <- log(theta) + zp * sqrt(gdetheta * phi / expectedRange)
            lowerCI[[i]] <- log(theta) - zp * sqrt(gdetheta * phi / expectedRange)
            ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
            yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
            lowOUT[[i]] <- ifelse(change.table$y < ylowCI[[i]],TRUE, FALSE)
            uppOUT[[i]]<- ifelse(change.table$y > yuppCI[[i]], TRUE, FALSE)
            outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
            outcolname[i] <- paste0(p[i]*100,"%CI")
            change.table <- cbind(change.table, outofcontrol[[i]])
          }
        }
      } else{
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
      }
    if (auto.xlab){xlab = xlab[2]}
    if (auto.ylab){ylab = ylab[2]}

      theta <- log(theta)
  }

  x <- change.table$rho; y <- change.table$y; range <- expectedRange
  output <- list(x = x, y = y, theta = theta, range = range, tab = change.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = myunits, p = p, unitnames = unitnames, printUnits = printUnits, phi = phi)
  colnames(output$tab) <- c("Unit",secondcolname, "Obs1", "Exp1","N1", "Obs2","Exp2","N2","rho", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2:9] <- round(output$tab[,2:9], digits)
  output
}
