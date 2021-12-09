#' Standardized Resource Use (SRU)
#'
#' @name SRU
#'
#' @description \code{SRU} calculates the standardized resource use for ICUs (Intensive Care Units) from information regarding admissions of individual patients. Resource use is represented by the patient's length of stay (LOS). Therefore the SRU for each unit is defined as the observed LOS divided by it's expected LOS. To estimate the expected LOS for each ICU one must define a severity score, here defined by the SAPS 3 score. In theory, the 'score' could be any score/probability that estimates death for each ICU admission.
#'
#' The \code{plot.SRU} function will return a \code{\link{SMR}} versus SRU scatter plot with its medians and tertiles. Thus, it classifies each unit in the quadrants formed by these two medians as: most efficient (ME) which is the lower left quadrant (both SRU and SMR below their medians);  least efficient (LE) is the upper right quadrant (both SRU and SMR above their medians); and least achieving (LA) - the lower right quadrant (SRU below and SMR above their medians); and over achieving (OA) - the upper left quadrant (SRU above and SMR below their medians).
#'
#'  \code{print.SRU} Prints a object of class 'SRU'.
#'
#'  \code{cut_in} is used to find limits to define severity classes which are used in \code{SRU} function. The severity classes are necessary to calculate the average of days to produce one survivor and consequently to estimate the expected LOS in each ICU. Its rationale is to find the limits for the severity classes that yeld a desired average of days to produce one survivor. At some point in time, we made a study to test if different arrangements of the severity classes would yeld different classifications in the efficiency quadrants. Despite the fact that this study did not show any difference from each approach, we left the function in the package. Therefore, any arbitrary severity classes should yeild the same results.
#'
#'  \code{SRUcalc} is a simpler function to estimte SRU and returns, for each unit, the SRU value, the observed and expected number of deaths, and the observed and expected LOS.
#'
#' @param prob Individual predicted probability of death (ranging from 0 to 1) in a vector.
#'
#' @param death Observed death. Accepted values are 0 (absence) or 1 (presence) in a vector.
#'
#' @param unit A character or factor variable indicating the ICU where the patient is admitted.
#'
#' @param los A numeric variable indicating the observed length of stay for each patient.
#'
#' @param los.exp Estimated length of stay (LOS). This argument is optional and will be required only if \code{type = 2}. If the user has an alternative model to estimate the individual LOS, the predicted individual LOS should be passed to this argument. If this is the case, the predicted ICU LOS is estimated as the mean of the individual predictions of the LOS of these groups.
#'
#' @param class A factor variable indicating the class of severity score (e.g. SAPS 3). In the case of SAPS 3, this is a cut in the SAPS 3 score, grouping patients into severity classes. This will be required if the argument \code{original = FALSE} and NAs are not allowed; if \code{original = TRUE}, class is ignored.
#'
#' @param score A numeric vector with the Acute Physiology Score (SAPS) 3 score for each admission. The function will use this argument to know to which severity class each patient will be assigned to. It is used only when \code{originals = TRUE} and ignored otherwise. NAs are not allowed.
#'
#' @param plot Logical; If \code{TRUE}, plots a SMR versus SRU scatter plot.
#'
#' @param type A Way to calculate SRU. If \code{type = 1}, it does as the original article to estimate the ICU's expected LOS (default). First, it multiplies the overall average of days of each severity class by the number of survivors in the same severity class in that ICU. Than, it sums the expected LOS for each severity class in that ICU. If \code{type = 2}, the user must provide the \code{los.exp} (expected LOS) for each subject (i.e. from a prediction model), and the function will estimate the ICU's expected LOS as the mean of all individual LOS for patients in that ICU.
#'
#' @param digits,digits2 Integer indicating the number of decimals to be used in the output.
#'
#' @param originals Logical; If \code{TRUE}, it uses the severity classes and average days as the original article and will override the \code{class} argument, if any. It requires the \code{score} argument and it must be the SAPS 3 score. We recommend not to set it \code{TRUE} unless you really know what you are doing. Even if one wishes to have severity classes identical to the original paper, it is better to set the severity classes before running the analysis. This way, the function will estimate the average days from the data instead of using the fixed average days from the original paper.
#'
#' @param x  For \code{print.SRU} or \code{plot.SRU}, an object of class 'SRU'.
#'
#' @param xlim,ylim Limits of x and y axis for \code{plot.SRU}.
#'
#' @param xlab,ylab Labels of x and y axis for \code{plot.SRU}.
#'
#' @param points.arg List of arguments passed to \code{\link[graphics]{points}} for plotting points correponding to ICU's SMR and SRU.
#'
#' @param med.arg List of arguments passed to \code{\link[graphics]{abline}} for plotting lines corresponding to SRU's and SMR's medians.
#'
#' @param tert.arg List of arguments passed to \code{\link[graphics]{abline}} for plotting lines corresponding to SRU's and SMR's tertiles.
#'
#' @param auto.legend Logical; If \code{TRUE}, prints a legend with parameters in \code{leg.arg} arguments.
#'
#' @param leg.arg List of arguments passed to \code{\link[graphics]{legend}} for plotting legends in \code{plot.SRU}.
#' @param bty A character string which determines the type of box that is drawn about plots. See  \code{\link[graphics]{par}}
#'
#' @param ... Arguments to be passed to \code{\link[graphics]{plot.default}} or to \code{\link[base]{print}}.
#'
#' @param days For \code{cut_in}, this is a vector of days to get an avarage. See example.
#'
#' @param min For \code{cut_in}, this is the minimum desired quantity of patients in each severity class (default = 200) to estimate the average days.
#'
#' @param exc.ICU Logical; For \code{cut_in}, if \code{TRUE}, ICUs without surviving patients are ignored.
#'
#' @param complete Logical; For \code{cut_in}, if \code{TRUE}, shows additional information about severity classes.
#'
#' @param myunits A character vector with the unit names which one would like to benchmark among all units. These units will be highlighted with dots of different collors in the plot. Default is \code{NULL}.
#'
#' @param myunitspts.arg List of arguments passed to \code{\link[graphics]{points}} for plotting points correponding to \code{myunits}'s SMR and SRU.
#'
#' @param myunitstext.arg List of arguments passed to \code{\link[graphics]{text}} for labelling points correponding to \code{myunits}'s position.
#'
#' @return Two tables: one with information about severity classes and the respective quantities required to estimate the expected LOS, and another with information about ICUs classified as Most Efficient (ME) or Least Efficient (LE).
#' \itemize{
#' \item \code{Sev} Severity class.
#' \item\code{Total} Total of patients.
#' \item \code{Surv} Total of survivors.
#' \item \code{Total.LOS} Total length of stay (days).
#' \item \code{AvDays} Average days to produce a survivor.
#' \item \code{N.Unit} Quantity of ICUs.
#' \item \code{N.Pat} Quantity of patients.
#' \item\code{SMR} Standardized Mortality Ratio Mean (standard deviation).
#' \item \code{SRU} Standardized Resource Use Mean (standard deviation).
#' }
#'
#' Most Efficient ICUs have SRU, SMR < median. Least Efficient ICUs have SRU, SMR > median.
#'
#' \code{cut_in} returns a vector with the limits to cut the severity score.
#'
#' \code{SRUcalc} returns a table with:
#' \itemize{
#' \item \code{Unit} ICUs names.
#' \item\code{SMR or SRU} Standardized Rate.
#' \item \code{N} Number of subjects analyzed.
#' \item \code{Observed} Observed number of deaths.
#' \item \code{Expected} Expected number of deaths.
#' \item \code{LOS_esp} Expected length of stay.
#' }
#'
#' @references
#' Rothen HU, Stricker K, Einfalt J, Bauer P, Metnitz PGH, Moreno RP, Takala J (2007) Variability in outcome and resource use in intensive care units. Intensive Care Med 33:1329-1336
#'
#' @seealso \code{\link{SMR}}, \code{\link{reclass}}, \code{\link{funnel}}
#'
#' @author Lunna Borges and Pedro Brasil
#'
#' @examples
#'
#' # Loading the dataset
#' data(icu)
#'
#' # Removing data with inapropriate values and some editing
#' icu <- icu[-which(icu$los < 0 ),]
#' icu$Saps3DeathProbabilityStandardEquation <- icu$Saps3DeathProbabilityStandardEquation / 100
#'
#' # Setting classes acording to limits of SAPS 3 score
#' days <- seq(1,100)
#' cut_lims <- cut_in(icu$Saps3Points, icu$los, icu$UnitDischargeName,
#'                    icu$Unit, days, exc.ICU = TRUE)
#' icu$class <- cut(icu$Saps3Points, breaks = cut_lims, include.lowest = TRUE)
#'
#' # Estimating the SRU benchmarking myunit A and B
#' x <- SRU(prob = icu$Saps3DeathProbabilityStandardEquation,
#' death = icu$UnitDischargeName, unit = icu$Unit,
#' los = icu$los, score = icu$Saps3Points,
#' originals = TRUE, type = 1, plot = FALSE, myunits = c("A","B"))
#' x
#' plot(x)
#'
#' # To see the units rankings and individual SMR and SRU, ordering by its SRU
#' x$rates[order(x$rates$sru),]
#'
#' # SRU with diferent severity classes created by cut_in function
#' y <- SRU(prob = icu$Saps3DeathProbabilityStandardEquation,
#' death = icu$UnitDischargeName, unit = icu$Unit,
#' los = icu$los, score = icu$Saps3Points,
#' originals = FALSE, type = 1, plot = FALSE, class = icu$class)
#' y
#'
#' # Using SRUcalc
#' SRUcalc(prob = icu$Saps3DeathProbabilityStandardEquation,
#'         death = icu$UnitDischargeName, unit = icu$Unit, los = icu$los,
#'         score = icu$Saps3Points)
#'
#' rm(x, y, days, icu, cut_lims)
#'
#' @import stats
#' @import graphics
#' @import utils
#' @export

SRU <- function(prob, death, unit, los, los.exp, class, score, plot = FALSE, type = 1, digits = 2, digits2 = 5, originals = FALSE, myunits = NULL){
  if ( any(is.na(prob)) ){
    stop("'prob' must not have any NA value.")
  }
  if(any(min(prob) < 0 | max(prob) > 1)){
    stop("'prob' must range from 0 to 1.")
  }
  if(any(is.na(death))){
    stop("'death' must not have any NA value.")
  }
  if(any(death != 0 & death != 1)){
    stop("'death' variable must be coded as 0 and 1.")
  }
  if(!is.factor(unit)){
    stop("'unit' must be a factor.")
  }
  if(any(is.na(los))){
    stop("'los' must not have any NA value.")
  }
  if(!is.numeric(los)){
    stop("'los' must be numeric.")
  }
  if (any(los < 0)){
    stop("There is at least one negative 'los'. It must be a positive number.")
  }
  if(plot != TRUE && plot != FALSE){
    stop("'plot' must be either 'TRUE' or 'FALSE'.")
  }
  if(type != 1 && type != 2){
    stop("'type' must be either 1 or 2.")
  }
  if(type == 2){
    if(any(is.na(los.exp))){
      stop("'los.exp' must not have any NA value.")
    }
    if(!is.numeric(los.exp)){
      stop("'los.exp' must be numeric.")
    }
    if (any(los.exp < 0)){
      stop("There is at least one negative 'los.exp'. It must be a positive number.")
    }
  }
  if(originals != TRUE && originals != FALSE){
    stop("'originals' must be either 'TRUE' or 'FALSE'.")
  }

  if (originals){

    if(any(is.na(score))){
      stop("'score' must not have any NA value.")
    }
    if(!is.numeric(score)){
      stop("'score' must be numeric.")
    }
    class = cut(score, breaks = c(min(score),24,34,44,54,64,74,84,94,max(score)),
                include.lowest = T)
  }else{

    if(any(is.na(class))){
      stop("'class' must not have any NA value.")
    }
    if(!is.factor(class)){
      stop("'class' must be a factor.")
    }
  }
  if (any(! myunits %in% unit)){
    warning(paste0("There is no unit called ", myunits[which(!myunits %in% unit)], "\n"))
    myunits <- myunits[-which(! myunits %in% unit)]
  }

  if(type == 1){dt <- data.frame(prob, death, unit, los, class)}
  if(type == 2){dt <- data.frame(prob, death, unit, los, los.exp, class)}
  ### Quantity of discharge per unit
  unit_death <- table(dt$unit, dt$death)
  ### Excluding units whose didn't have any survivals
  exc <- rownames(unit_death)[which(unit_death[,1] == 0)]
  if (!is.null(exc) & length(exc) > 0){
    dt <- dt[-which(dt$unit %in% exc),]
    dt <- droplevels(dt)
    unit_death <- table(dt$unit,dt$death)
    warning(paste(c("The following units were excluded due to absence of survivals:", exc), collapse = ", "))
    if (any(myunits %in% exc)){
      myunits <- myunits[-which(myunits %in% exc)]
    }
  }

  ### sum of LOS for each severity class
  cla_los <- aggregate(dt$los, by = list(Class = dt$class), FUN = sum)
  sum_los <- cla_los[2]	        	#los cum by class

  ### sum of discharge for wach severity class
  cla_dea <- table(dt$class, dt$death)
  surv <- cla_dea[,1]			#survivors by class
  total <-   cla_dea[,1] + cla_dea[,2]		#total patients by class

  if (originals) {
    average_days <- matrix(c(2.3,3.2,4.3,7.2,11,16.6,22.2,29.4,39), ncol = 1)
  } else {
    average_days <- sum_los / surv 	#average days of resource used by patients, by class
  }
  if (type == 1){
    # unit_death[,1]					#survivors by unit
    unit_class <- data.frame(dt$unit,dt$class, dt$death)
    unit_class <- unit_class[-which(unit_class$dt.death==1),]
    unit_class <- table(unit_class)
    unit_class <- matrix(unit_class, nrow(unit_death), nrow(average_days))	#survivors by unit, by class
    #rownames(unit_class)=rownames(unit_death); colnames(unit_class)=rownames(cla_dea)
    A <- matrix(average_days[,1], nrow(unit_death), nrow(average_days), byrow=T) #average days by class matrix
    rec_unit_class <- A*unit_class		#expected average days to produce a survivor by unit, by class
    LOS_ICU_esp <- apply(rec_unit_class, MARGIN=1, FUN = sum)		#expected los
  }
  if (type == 2){
    LOS_ICU_esp <- aggregate(dt$los.exp, by <- list(Class <- dt$unit), FUN = sum)[,2]
  }

  LOS_ICU_obs <- aggregate(dt$los, by = list(Class = dt$unit), FUN = sum)[,2]		#total observed los by unit
  sru <- LOS_ICU_obs / LOS_ICU_esp					#SRU by unit
  # unit_death[,2]			#deaths by unit
  B <- aggregate(dt$prob,by=list(Class=dt$unit),FUN=sum)[,2]	#death probability sum by unit
  smr <- unit_death[,2] / B		#SMR by unit
  M1 <- median(sru);	M2 <- median(smr)
  Q1 <- quantile(sru, prob = c(.33,.66))		#tertiles
  Q2 <- quantile(smr, prob = c(.33,.66))
  rates <- data.frame(sru, smr, unit = labels(smr))				#efficient quadrant by unit
  rates$group <- NA
  rates$group[which(rates[,1] < M1 & rates[,2] < M2)] <- "ME"
  rates$group[which(rates[,1] >= M1 & rates[,2] >= M2)] <- "LE"
  rates$group[which(rates[,1] >= M1 & rates[,2] < M2)] <- "OVER"
  rates$group[which(rates[,1] < M1 & rates[,2] >= M2)] <- "UNDER"

  rates.ef <- rates[which(rates$group == "ME" | rates$group == "LE"),]  #units classified as "ME" or "LE"
  rates.ef$LT <- NA
  rates.ef$Menor.Median <- NA
  rates.ef$Maior.Median <- NA
  rates.ef$HT <- NA
  rates.ef$unit <- as.factor(rownames(rates.ef))

  rates.ef$LT <- ifelse (rates.ef[,1] < Q1[1] & rates.ef[,2] < Q2[1], TRUE, FALSE)
  rates.ef$HT <- ifelse (rates.ef[,1] > Q1[2] & rates.ef[,2] > Q2[2], TRUE, FALSE)
  rates.ef$Menor.Median <- ifelse(rates.ef$group == "ME", TRUE, FALSE)
  rates.ef$Maior.Median <- ifelse(rates.ef$group == "LE", TRUE, FALSE)

  quant_unit <- c(length(rates.ef$unit[which(rates.ef$LT == TRUE)]),
                  length(rates.ef$unit[which(rates.ef$Menor.Median == TRUE)]),
                  length(rates.ef$unit[which(rates.ef$Maior.Median == TRUE)]),
                  length(rates.ef$unit[which(rates.ef$HT == TRUE)]),
                  length(rates.ef[,1]))		#number of units by class

  sru.mean <- c(mean(rates.ef$sru[which(rates.ef$LT == TRUE)]),	#average SRU in each class
                mean(rates.ef$sru[which(rates.ef$Menor.Median == TRUE)]),
                mean(rates.ef$sru[which(rates.ef$Maior.Median == TRUE)]),
                mean(rates.ef$sru[which(rates.ef$HT == TRUE)]),
                mean(rates.ef$sru))

  sru.sd <- c(sd(rates.ef$sru[which(rates.ef$LT == TRUE)]),					#SRU standard deviation in each class
              sd(rates.ef$sru[which(rates.ef$Menor.Median == TRUE)]),
              sd(rates.ef$sru[which(rates.ef$Maior.Median == TRUE)]),
              sd(rates.ef$sru[which(rates.ef$HT == TRUE)]),
              sd(rates.ef$sru))

  smr.mean <- c(mean(rates.ef$smr[which(rates.ef$LT == TRUE)]),				#average SMR in each class
                mean(rates.ef$smr[which(rates.ef$Menor.Median == TRUE)]),
                mean(rates.ef$smr[which(rates.ef$Maior.Median == TRUE)]),
                mean(rates.ef$smr[which(rates.ef$HT == TRUE)]),
                mean(rates.ef$smr))

  smr.sd <- c(sd(rates.ef$smr[which(rates.ef$LT == TRUE)]),					#SMR standard deviation in each class
              sd(rates.ef$smr[which(rates.ef$Menor.Median == TRUE)]),
              sd(rates.ef$smr[which(rates.ef$Maior.Median == TRUE)]),
              sd(rates.ef$smr[which(rates.ef$HT == TRUE)]),
              sd(rates.ef$smr))

  pacient_total <- sum(table(dt$unit)[match(rates.ef$unit, as.factor(names(table(dt$unit))))])	 							#total of patients
  pacient_LT <- sum(table(dt$unit)[match(rates.ef$unit[which(rates.ef$LT == TRUE)], as.factor(names(table(dt$unit))))])				#LT total patients
  pacient_Menor.Median <- sum(table(dt$unit)[match(rates.ef$unit[which(rates.ef$Menor.Median == TRUE)], as.factor(names(table(dt$unit))))])	#<MEDIAN total patients
  pacient_Maior.Median <- sum(table(dt$unit)[match(rates.ef$unit[which(rates.ef$Maior.Median==TRUE)], as.factor(names(table(dt$unit))))])	#>MEDIAN total patients
  pacient_HT <- sum(table(dt$unit)[match(rates.ef$unit[which(rates.ef$HT == TRUE)], as.factor(names(table(dt$unit))))])				#HT total patients

  n.pacients <- c(pacient_LT, pacient_Menor.Median, pacient_Maior.Median, pacient_HT, pacient_total)

  unit_admissions = apply(unit_death,1,sum)

  output <- list(
    LOS.surv = data.frame(row.names=NULL,labels(total), total, surv, round(as.numeric(sum_los$x), digits2), round(average_days,digits2)),
    estim.eff = data.frame(N.Unit=quant_unit, N.Pat = n.pacients, SMR = paste(format(smr.mean,digits = digits, nsmall = digits), " ",
                                                                              "(", format(smr.sd,digits = digits, nsmall = digits),")", sep=""), SRU=paste(format(sru.mean, digits = digits, nsmall = digits)," ","(",format(sru.sd,digits = digits,nsmall = digits),")", sep = "")),
    rates = rates,
    med = c(M1,M2),
    tert = c(Q1,Q2),
    LOS_obs = LOS_ICU_obs,
    LOS_esp = LOS_ICU_esp,
    ratesef = which(rates$group == "ME" | rates$group == "LE"),
    totalICU = nrow(unit_death), totalAd = unit_admissions,
    myunits = myunits
  )
  colnames(output$LOS.surv) <- c("Sev","Total","Surv","Total.LOS","AvDays")
  rownames(output$estim.eff) <- c("Low.Tert","<Median",">Median","High.Tert","All.Units")

  class(output) <- "SRU"

  if (plot == TRUE){plot(output)}

  output
}

#' @rdname SRU
#' @export

print.SRU <- function(x, ...){
  print(x$LOS.surv, ...)
  cat("--------------------------------------------------------\n")
  if (length(x$ratesef) > 1){
    print(x$estim.eff, ...)
    if (x$estim.eff$N.Unit[1] == 1 | x$estim.eff$N.Unit[4] == 1){
      cat("--------------------------------------------------------\n")
      cat("It is not possible to calculate standard deviation with a single unit.\n")
    }
  } else{
    cat("There are", as.character(length(x$rates[which(x$rates$group == "ME")])), "Most Efficient and", as.character(length(x$rates[which(x$rates$group == "LE")])), "Least Efficient ICUs in",x$totalICU, "ICUs.", sep= " ")
  }

}

#' @rdname SRU
#' @export
plot.SRU <- function(x, ..., xlim = range(x$rates[,2]), ylim = range(x$rates[,1]), xlab = "SMR", ylab = "SRU", points.arg = list(pch = 21, col = "white", bg = "cadetblue3",cex=1.5), med.arg = list(col="dodgerblue4",lwd = 2,lty = 1), tert.arg = list(col = "darkorange2", lty = 2, lwd = 1), auto.legend = TRUE, leg.arg = list(x = "top", bty = "n", xpd = NA, inset = -.2, ncol = 2), bty = "n", myunits = x$myunits, myunitspts.arg = list(pch = 21, col = "white", bg = "red",cex=1.5), myunitstext.arg = list(pos = 1, font = 2, cex = .8)){

  plot(0, 0, ..., xlim = xlim, ylim = ylim, type = 'n', xlab = "", ylab = "", bty = bty, xaxt = "n", yaxt = "n")

  mtext(text = xlab, side = 1, line = 2.5, font = 2)

  mtext(text = ylab, side = 2, line = 2.5, font = 2)

  axis(side = 1, tick = T, pos = NA, lwd = 1, lwd.ticks = 1, col.axis = "gray60", col.ticks = "gray60", lty = 1, col = "gray60")

  axis(side = 2, tick = T, pos = NA, lwd = 1, lwd.ticks = 1, col.axis = "gray60", col.ticks = "gray60", lty = 1, col = "gray60")

  med.arg$v <- x$med[2]
  med.arg$h <- x$med[1]
  tert.arg$h <- x$tert[1:2]
  tert.arg$v <- x$tert[3:4]
  points.arg$x <- x$rates[,c(2,1)]
  do.call(abline, med.arg)
  do.call(abline, tert.arg)
  do.call(points, points.arg)

  if(length(myunits) > 0){

    myunitspts.arg$x <- x$rates[which(rownames(x$rates[,c(2,1)]) == myunits),c(2,1)]
    myunitstext.arg$x <- x$rates[which(rownames(x$rates[,c(2,1)]) == myunits),c(2)]
    myunitstext.arg$y <- x$rates[which(rownames(x$rates[,c(2,1)]) == myunits),c(1)]
    myunitstext.arg$labels <- x$myunits
    do.call(points, myunitspts.arg)
    do.call(text, myunitstext.arg)
  }

  if(auto.legend){
    leg.arg$legend <- c("Median", "Tertile")
    leg.arg$col <- c(med.arg$col, tert.arg$col)
    leg.arg$lty <- c(med.arg$lty, tert.arg$lty)
    leg.arg$lwd <- c(med.arg$lwd, tert.arg$lwd)
    do.call(legend, leg.arg)
  }
}


#' @rdname SRU
#' @export
cut_in <- function (score, los, death, unit, days, min = 200, exc.ICU = TRUE, complete = FALSE, digits = 5){
  if(length(which(is.na(score))) > 0){
    stop("'score' must not have any NA value.")}
  if(!is.numeric(score)){
    stop("score must be numeric.")}
  if(!is.numeric(los)){
    stop("LOS must be numeric.")}
  if(any(as.factor(death) != 0 & as.factor(death) != 1)){
    stop("Observed death variable must be coded as 0 and 1.")}
  if (!is.numeric(days)){
    stop("Days to be tested must be numeric.")}
  if(!is.factor(unit)){
    stop("Unit must be a factor.")}
  if(exc.ICU != TRUE && exc.ICU != FALSE){
    stop("Exc.ICU must be either 'TRUE' or 'FALSE'.")}
  if(complete != TRUE && complete != FALSE){
    stop("Complete must be either 'TRUE' or 'FALSE'.")}

  dt <- data.frame(score, los, death, unit)

  if (exc.ICU == TRUE){
    unit_death <- table(dt$unit, dt$death)
    exc <- rownames(unit_death)[which(unit_death[,1] == 0)]

    if (!is.null(exc) & length(exc) > 0){
      dt <- dt[-which(dt$unit %in% exc),]			#exclude units without survivors
      dt <- droplevels(dt)
      warning(paste(c("The following units were excluded due to absence of survivals:", exc), collapse=" "))
    }
  }
  un <- sort(unique(dt$score))
  breaks <- NULL; avdays <- c(); pt.corte <- c(); b <- c(); d <- c(); da <- c(); tail <- c()
  j <- 1;	w <- days[1]

  for (i in 2:length(un)) {
    breaks[j:i] <- un[j:i]
    cond <- which(dt$score >= breaks[j] & dt$score < breaks[i])

    if (length(dt$score[cond]) >= min) {
      sum_los <- sum(dt$los[cond])		#los sum
      surv <- (length(dt$death[cond]) - sum(dt$death[cond]))		#survivors
      avdays <- append(avdays, sum_los / surv) 		#average days used to produce a survivor
      b <- append(b,breaks[i-1])		#possibles cut offs

      if (tail(avdays,1) > w){
        tail <- append(tail, tail(avdays,1))
        da <- append(da, w)
        pt.corte <- append(pt.corte, b[which.min(abs(avdays - w))])
        d <- append(d, avdays[which.min(abs(avdays - w))])	#average days
        w <- days[match(which(days > tail(d,1) & days > w), days)][1]
        avdays <- c()
        b <- c()
        i <- which(un == tail(pt.corte, 1) + 1)
        j <- which(un == tail(pt.corte, 1) + 1)
      }
      i <- i + 1
    } else {i <- i + 1}
  }

  corte <- c(min(score), pt.corte, max(score))
  L <- c()
  lowest <- min(score) - 1

  for (k in 1:length(corte)){			#patients by class
    cond2 <- length(which(dt$score > lowest & dt$score <= corte[k + 1]))
    lowest <- corte[k + 1]
    L <- append(L, cond2)
  }
  L=L[-length(L)]

  if (L[length(L)] < min){ 	#If last class has less than minimum of patients, put it together with the second-last
    pt.corte <- pt.corte[-length(pt.corte)]
    da <- da[-length(da)]
    tail <- tail[-length(tail)]
    d <- d[-length(d)]
    L2 <- L[-length(L)]
    corte <- c(min(score), pt.corte, max(score))
  }
  if (complete == TRUE){
    cond3 <- which(dt$score > corte[length(corte) - 1] & dt$score <= max(dt$score))
    sum_los <- sum(dt$los[cond3])
    surv <- (length(dt$death[cond3]) - sum(dt$death[cond3]))
    high.avday <- sum_los / surv

    if (L[length(L)] < min){
      output <- data.frame("Days"=da, "Tail" = round(tail,digits), "Corte" = pt.corte, "AvDays" = round(d,digits), "Total.Pacients" = L2[-length(L2)])
      highest <- c("-", "-", max(dt$score), round(high.avday, digits), sum(L[length(L)], L[length(L) - 1]))
      output <- rbind(output, highest)
    }else{
      output <- data.frame("Days" = da, "Tail" = round(tail, digits), "Corte" = pt.corte, "AvDays" = round(d,digits), "Total.Pacients" = L[-length(L)])
      highest <- c("-", "-", max(dt$score), round(high.avday, digits), L[length(L)])
      output <- rbind(output, highest)
    }
  } else {corte}
}

#' @rdname SRU
#' @export
SRUcalc <- function(prob, death, unit, los, score, digits = 2){
  if (any(is.na(prob))) {
    stop("'prob' must not have any NA value.")
  }
  if (any(min(prob) <0 | max(prob) > 1)) {
    stop("'prob' must range from 0 to 1.")
  }
  if (any(is.na(death))) {
    stop("'death' must not have any NA.")
  }
  if (any(death != 0 & death != 1)) {
    stop("'death' must be coded as 0 and 1.")
  }
  if (!is.factor(unit)) {
    stop("'unit' must be a factor.")
  }
  if (any(is.na(los))) {
    stop("'los' must not have NA.")
  }
  if (!is.numeric(los)) {
    stop("'los' must be numeric.")
  }
  if (any(los < 0)) {
    stop("'los' must be positive numbers.")
  }
  if (any(is.na(score))) {
    stop("'score' must not have NA.")
  }
  if(!is.numeric(score)){
    stop("'score' must be numeric.")
  }

  class <- cut(score, breaks=c(min(score),24,34,44,54,64,74,84,94,max(score)), include.lowest = T)
  dt <-  data.frame(prob, death, unit, los, class)
  unit_death <- table(dt$unit, dt$death)
  exc <- rownames(unit_death)[which(unit_death[,1] == 0)]
  if (!is.null(exc) & length(exc) > 0){
    dt <- dt[-which(dt$unit %in% exc),]
    dt <- droplevels(dt)
    unit_death <- table(dt$unit,dt$death)
    warning(paste(c("The following units were excluded due to absence of survivals:", exc), collapse = ", "))
  }
  admissions <- as.numeric(table(dt$unit))
  average_days <- matrix(c(2.3,3.2,4.3,7.2,11,16.6,22.2,29.4,39), ncol = 1)
  unit_class <- data.frame(dt$unit, dt$class, dt$death)
  unit_class <- unit_class[-which(unit_class$dt.death == 1),]
  unit_class <- table(unit_class)
  unitnames <- rownames(unit_class)
  unit_class <- matrix(unit_class, length(unique(dt$unit)), nrow(average_days))
  A <- matrix(average_days[,1], length(unique(dt$unit)), nrow(average_days), byrow = TRUE) #resources by class matrix
  rec_unit_class <- A * unit_class		#expected average days to produce a survivor by unit and by class
  LOS_esp <- apply(rec_unit_class, MARGIN = 1, FUN = sum)
  LOS_obs <- aggregate(dt$los, by = list(Class = dt$unit), FUN = sum)[,2]		#observed los by unit
  sru <- LOS_obs / LOS_esp

  expected <- SMR.table(data = dt, group.var = "unit", obs.var = "death", pred.var = "prob")$Expected[-1]

  output <- data.frame(Unit = unitnames, SRU = round(sru, digits), N = admissions, Observed = unit_death[,2], Expected = expected, LOS_obs = LOS_obs, LOS_exp = LOS_esp)
  output
}
