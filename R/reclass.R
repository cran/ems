#' Comparisson of the Standardized Resource Use (SRU)
#'
#' @name reclass
#'
#' @description Compares ICU's (intensive care units) SRU with diferent severity classes or compares ICU's SRU at two diferents times. This comparison checks if the ICUs remains in the same quadrant after a time period, and highlights their rank changes over time.
#'
#' \code{plot.reclass} Plots a SMR vs. SRU scatter plot with the ICUs which had their quadrant/rank classification changed.
#'
#' \code{print.reclass} Prints a table with information about which ICUs changed from a classification to another.
#'
#' @param x,y Objects of class 'SRU'. x is the SRU analsys from the 1st period (e.g. first trimester) and y from the 2nd period (e.g. second trimester). For \code{print.reclass} or \code{plot.reclass}, x is an object of class 'reclass'.
#'
#' @param same Logical; If \code{TRUE}, compare the same units, with the same severity classes at two consecutive time periods (default). If \code{same = TRUE} and the ICUs do not match exactly in 'x' and 'y', there is a warning and non matching units are discarded from the analysis. If \code{FALSE}, it compares the same units, with the different severity classes within the same period. In this case, if the ICUs do not match exactly in 'x' and'y', the function will return an error.
#'
#' @param plot Logical. If \code{TRUE} (default), plots a SMR vs. SRU scatter plot highlighting the ICUs which had their classification changed.
#'
#' @param digits Integer indicating the number of decimal places to be used in the output.
#'
#' @param compare The way one prefer to benchmark the ICUs: by "SRU" (default), "SMR" or "BOTH". If "BOTH", the ICUs will be ranked by their SRU.
#'
#' @param decreasing Logical. Should the sort order of ICU's rank be increasing or decreasing?
#'
#' @param complete.rank Logical. If \code{TRUE} (default), returns all ICUs ranked. If \code{FALSE}, returns only ICUs whose changed their efficiency classification ranked.
#'
#' @param xlim_x,ylim_x Limits for x and y axis for 1st stage plot for \code{plot.reclass}.
#'
#' @param xlim_y,ylim_y Limits for x and y axis for 2nd stage plot for \code{plot.reclass}.
#'
#' @param xlab,ylab Labels of x and y axis for \code{plot.reclass}.
#'
#' @param points.arg_x,points.arg_y List of arguments passed to \code{\link[graphics]{points}} for plotting points correponding to units' SMR and SRU in 1st and 2nd stage plots for \code{plot.reclass}.
#'
#' @param med.arg_x,med.arg_y List of arguments passed to \code{\link[graphics]{abline}} for plotting lines corresponding to SRU and SMR medians in 1st and 2nd stage plots for \code{plot.reclass}.
#'
#' @param tert.arg_x,tert.arg_y List of arguments passed to \code{\link[graphics]{abline}} for plotting lines corresponding to SRU and SMR tertiles in 1st and 2nd stage plots for \code{plot.reclass}.
#'
#' @param text.arg_x,text.arg_y List of arguments passed to \code{\link[graphics]{text}} for plotting units labels in 1st and 2nd stage plots for \code{plot.reclass}.
#'
#' @param worse.arg_x,worse.arg_y List of arguments passed to \code{\link[graphics]{points}} for plotting points correponding to units which got your rank worse in 1st and 2nd stage plots for \code{plot.reclass}.
#'
#' @param better.arg_x,better.arg_y List of arguments passed to \code{\link[graphics]{points}} for plotting points correponding to units which got your rank better in 1st and 2nd stage plots for \code{plot.reclass}.
#'
#' @param auto.legend Logical. If \code{TRUE}, it prints a legend with \code{leg.arg} arguments for \code{plot.reclass}.
#'
#' @param leg.arg List of arguments passed to \code{\link[graphics]{legend}} for plotting legends corresponding to SRU and SMR medians and tertiles in 1st and 2nd stage plots for \code{plot.reclass}.
#' @param main.arg_x,main.arg_y List of arguments passed to \code{\link[graphics]{plot}} for overall title for 1st and 2nd stage plots for \code{plot.reclass}.
#'
#' @param ... Arguments to be passed to methods, such as \code{\link[graphics]{graphical parameters}} (see \code{\link[graphics]{par}}).
#'
#' @return \code{reclass} retunrs  a data.frame with the following columns:
#' \itemize{
#' \item \code{Unit} Names of the ICU.
#' \item\code{Admission} Number of admissions in each ICU.
#' \item \code{From} ICU's initial efficiency quadrant.
#' \item \code{To} ICU's final efficiency quadrant.
#' \item \code{SRU.1st} ICU's initial SRU estimate.
#' \item \code{SRU.2nd} ICU's final SRU estimate.
#' \item \code{SMR.1st} ICU's initial SMR estimate.
#' \item \code{SMR.2nd} ICU's final SMR estimate.
#' \item \code{Rank1} ICU's initial SRU (or SMR) rank.
#' \item \code{Rank2} ICU's final SRU (or SMR) rank.
#' }
#'
#' \code{plot.reclass} returns a scatter plot with grpahical comparison of the two periods/stages with their respective medians and tertiles.
#'
#' @seealso \code{\link{SRU}}, \code{\link{SMR}}, \code{\link{funnel}}
#'
#' @author Lunna Borges and Pedro Brasil
#'
#' @examples
#'
#' data(icu)
#' # A little editing
#' icu$Saps3DeathProbabilityStandardEquation <- icu$Saps3DeathProbabilityStandardEquation / 100
#' icu <- icu[-which(icu$los < 0 ),]
#'
#' # Subseting the data for the 1st quarter
#' x <- droplevels(icu[which(format(as.Date(icu$UnitAdmissionDate),"%m") %in% c("01","02","03")),])
#'
#' # Subseting the data for the 2nd quarter
#' y <- droplevels(icu[which(format(as.Date(icu$UnitAdmissionDate),"%m") %in% c("04","05","06")),])
#'
#' # Running the SRU analysis for both quarters
#' FirstQ <- SRU(prob = x$Saps3DeathProbabilityStandardEquation, death = x$UnitDischargeName,
#' unit = x$Unit, los = x$los, score = x$Saps3Points, originals = TRUE, type = 1, plot = FALSE)
#' FirstQ
#'
#' SecondQ <- SRU(prob = y$Saps3DeathProbabilityStandardEquation, death = y$UnitDischargeName,
#' unit = y$Unit, los = y$los, score = y$Saps3Points, originals = TRUE, type = 1, plot = FALSE)
#' SecondQ
#'
#' z <- reclass(x = FirstQ, y = SecondQ, same = TRUE)
#' z
#' plot(z)
#'
#' rm(icu, x, y, FirstQ, SecondQ, z)
#'
#' @import stats
#' @import graphics
#' @export

reclass <- function(x, y, same = TRUE, plot = FALSE, digits = 2, compare = c("SRU","SMR","BOTH"),decreasing = FALSE, complete.rank = TRUE){
  if(class(x) != "SRU" || class(y)!= "SRU"){
    stop("'x'and 'y' must be objects of class 'SRU'.")
  }
  totalAd <- list(x$totalAd, y$totalAd)
  x <- x$rates; y <- y$rates
  if(!is.data.frame(x)){
    stop("'x$rates' must be a data frame.")
  }
  if(!is.data.frame(y)){
    stop("'y$rates' must be a data frame.")
  }
  if(compare[1] != "SMR" && compare[1] != "SRU" && compare[1] != "BOTH"){
    stop("'compare' must be either 'SMR' or 'SRU' or 'BOTH'.")
  }
  if(compare[1] == "BOTH"){
    warning(paste0("As compare = 'BOTH', we rank the ICUs by their SRU."))
  }
  if(!is.logical(same)){
    stop("'same' must be either 'TRUE' or 'FALSE'.")
  }
  if(same){
    a <- x[which(x$unit %in% y$unit),]
    b <- y[which(y$unit %in% a$unit),]
    exc = nrow(a) != nrow(x) | nrow(b) != nrow(y)

    if (exc){
    warning(paste0(c("Some units were excluded because their absence in the 1st or 2nd stage.")))
      totalAdmissions <- totalAd[[1]][which(names(totalAd[[1]]) %in% a$unit)] + totalAd[[2]][which(names(totalAd[[2]]) %in% b$unit)]
    } else {
      totalAdmissions <- totalAd[[1]] + totalAd[[2]]
      }
    x <- droplevels(a); y <- droplevels(b)
  } else {
    if(nrow(x) != nrow(y)){
      stop("To compare two diferents models x and y must have the same ICUs.")
    }
    if (compare[1] == "SMR"){
      warning(paste0("SMR is the same in both models."))
    }
    totalAdmissions <- totalAd[[1]]
  }
  if(ncol(x) != 4 | ncol(y) != 4 ){
    stop("x$rates and y$rates must have 4 columns each.")
  }
  if (!all.equal(x[,3], y[,3])){
    stop("x e y must have informations about the same ICUs.")
  }
  if(isTRUE(all.equal(x[,4], y[,4]))){
    stop("There isn't any change.")
  }
  if(!is.logical(complete.rank)){
    stop("complete.rank must be either 'TRUE' or 'FALSE'.")
  }

  dt <- data.frame("Unit" = x$unit, "Now" = x$group, "After" = y$group,"Admissions" = totalAdmissions)
  dt$change <- ifelse(dt$Now != dt$After, "CHANGE", "SAME")

  nchanges <- length(which(dt$change == "CHANGE"))
  change_from_to <- data.frame("Unit" = dt$Unit[which(dt$change == "CHANGE")],"Admissions" = dt$Admissions[which(dt$change == "CHANGE")], "From" = dt$Now[which(dt$change == "CHANGE")], "To" = dt$After[which(dt$change == "CHANGE")])

# SMR and SRU only for that units whose changed their quadrants
  smrsru_yy <- data.frame("Unit" = change_from_to$Unit, "SMR" = y$smr[which(y$unit %in% change_from_to$Unit)], "SRU" = y$sru[which(y$unit %in% change_from_to$Unit)])
  smrsru_xx <- data.frame("Unit" = change_from_to$Unit, "SMR" = x$smr[which(y$unit %in% change_from_to$Unit)], "SRU" = x$sru[which(y$unit %in% change_from_to$Unit)])

  if(!complete.rank){
   tab <- cbind(change_from_to, "SRU.1st" = round(smrsru_xx[,3],digits),"SRU.2nd" = round(smrsru_yy[,3],digits))

    if (same){
      tab <- cbind(tab,"SMR.1st" = round(smrsru_xx[,2],digits), "SMR.2nd" = round(smrsru_yy[,2],digits))

      if (compare[1] == "SRU"){
        tab$Rank1 <- round(c(rank(tab$SRU.1st)))
        tab$Rank2 <- round(c(rank(tab$SRU.2nd)))
        tab <- tab[order(tab$Rank2,decreasing = decreasing),]
        rownames(tab) <- seq(1,nchanges)
      }
      if (compare[1] == "SMR"){
        tab$Rank1 <- round(c(rank(tab$SMR.1st)))
        tab$Rank2 <- round(c(rank(tab$SMR.2nd)))
        tab <- tab[order(tab$Rank2,decreasing = decreasing),]
        rownames(tab) <- seq(1,nchanges)

      }
      if (compare[1] == "BOTH"){
        tab$Rank1 <- round(c(rank(tab$SRU.1st)))
        tab$Rank2 <- round(c(rank(tab$SRU.2nd)))
        tab <- tab[order(tab$Rank2,decreasing = decreasing),]
        rownames(tab) <- seq(1,nchanges)
      }

      smrsru_x <- cbind(tab$SMR.1st,tab$SRU.1st)
      smrsru_y <- cbind(tab$SMR.2nd,tab$SRU.2nd)

    } else{
      tab <- cbind(tab, "SMR" = round(smrsru_yy[,2],digits))

      if (compare[1] == "SRU"){
        tab$Rank1 <- round(c(rank(tab$SRU.1st)))
        tab$Rank2 <- round(c(rank(tab$SRU.2nd)))
        tab <- tab[order(tab$Rank2,decreasing = decreasing),]
        rownames(tab) <- seq(1,nchanges)
      }
      if (compare[1] == "SMR"){
        tab$Rank <- round(c(rank(tab$SMR)))
        tab <- tab[order(tab$Rank,decreasing = decreasing),]
        rownames(tab) <- seq(1,nchanges)
      }
      if (compare[1] == "BOTH"){
        tab$Rank1 <- round(c(rank(tab$SRU.1st)))
        tab$Rank2 <- round(c(rank(tab$SRU.2nd)))
        tab <- tab[order(tab$Rank2,decreasing = decreasing),]
        rownames(tab) <- seq(1,nchanges)
      }

      smrsru_x <- cbind(tab$SMR,tab$SRU.1st)
      smrsru_y <- cbind(tab$SMR,tab$SRU.2nd)

    }
  }
# SMR and SRU for all units.
  sru_x = x$sru; smr_x = x$smr; sru_y = y$sru; smr_y =  y$smr

  if(complete.rank){
    tab <- data.frame("Unit" = dt$Unit,"Admissons" = totalAdmissions, "From" = dt$Now, "To" = dt$After, "SRU.1st" = round(sru_x,digits),"SRU.2nd" = round(sru_y,digits))

    if (same){
      tab <- cbind(tab,"SMR.1st" = round(smr_x,digits), "SMR.2nd" = round(smr_y,digits))

      if (compare[1] == "SRU"){
        tab$Rank1 <- round(c(rank(tab$SRU.1st)))
        tab$Rank2 <- round(c(rank(tab$SRU.2nd)))
        tab <- tab[order(tab$Rank2,decreasing = decreasing),]
        rownames(tab) <- seq(1,nrow(tab))
      }
      if (compare[1] == "SMR"){
        tab$Rank1 <- round(c(rank(tab$SMR.1st)))
        tab$Rank2 <- round(c(rank(tab$SMR.2nd)))
        tab <- tab[order(tab$Rank2,decreasing = decreasing),]
        rownames(tab) <- seq(1,nrow(tab))

      }
      if (compare[1] == "BOTH"){
        tab$Rank1 <- round(c(rank(tab$SRU.1st)))
        tab$Rank2 <- round(c(rank(tab$SRU.2nd)))
        tab <- tab[order(tab$Rank2,decreasing = decreasing),]
        rownames(tab) <- seq(1,nrow(tab))
      }

      smrsru_x <- cbind(tab$SMR.1st,tab$SRU.1st)
      smrsru_y <- cbind(tab$SMR.2nd,tab$SRU.2nd)

    } else{
      tab <- cbind(tab, "SMR" = round(smr_y,digits))

      if (compare[1] == "SRU"){
        tab$Rank1 <- round(c(rank(tab$SRU.1st)))
        tab$Rank2 <- round(c(rank(tab$SRU.2nd)))
        tab <- tab[order(tab$Rank2,decreasing = decreasing),]
        rownames(tab) <- seq(1,nrow(tab))
      }
      if (compare[1] == "SMR"){
        tab$Rank <- round(c(rank(tab$SMR)))
        tab <- tab[order(tab$Rank,decreasing = decreasing),]
        rownames(tab) <- seq(1,nrow(tab))
      }
      if (compare[1] == "BOTH"){
        tab$Rank1 <- round(c(rank(tab$SRU.1st)))
        tab$Rank2 <- round(c(rank(tab$SRU.2nd)))
        tab <- tab[order(tab$Rank2,decreasing = decreasing),]
        rownames(tab) <- seq(1,nrow(tab))
      }

      smrsru_x <- cbind(tab$SMR,tab$SRU.1st)
      smrsru_y <- cbind(tab$SMR,tab$SRU.2nd)

    }
  }

  M_x <- c(median(x$sru), median(x$smr))
  M_y <- c(median(y$sru), median(y$smr))

  Q1_x <- quantile(x$sru, prob = c(.33, .66))
  Q2_x <- quantile(x$smr, prob = c(.33, .66))
  Q1_y <- quantile(y$sru, prob = c(.33, .66))
  Q2_y <- quantile(y$smr, prob = c(.33, .66))

  #making matrices with green/red points for changind quadrants

  worse_x <- matrix(ncol = 2, dimnames = list(NULL, c("SMR", "SRU")))
  worse_y <- matrix(ncol = 2, dimnames = list(NULL, c("SMR", "SRU")))
  better_x <- matrix(ncol = 2, dimnames = list(NULL, c("SMR", "SRU")))
  better_y <- matrix(ncol = 2, dimnames = list(NULL, c("SMR", "SRU")))

  if (compare[1] == "SRU"){
    for (i in 1:nrow(tab)){
      if (tab[,3][i] == "ME" && tab[,4][i] == "LE" | tab[,3][i] == "ME" && tab[,4][i] =="UNDER" | tab[,3][i] == "OVER" && tab[,4][i] == "LE" | tab[,3][i] == "OVER" && tab[,4][i] == "UNDER" | tab[,3][i] == "UNDER" && tab[,4][i] == "LE" ){
        worse_x <- rbind(worse_x, smrsru_x[i,])
        worse_y <- rbind(worse_y, smrsru_y[i,])
      }
      if (tab[,3][i] == "LE" && tab[,4][i] == "OVER" | tab[,3][i] == "LE" && tab[,4][i] == "ME" | tab[,3][i] == "UNDER" && tab[,4][i] == "ME" | tab[,3][i] == "UNDER" && tab[,4][i] == "OVER"){
        better_x <- rbind(better_x, smrsru_x[i,])
        better_y <- rbind(better_y, smrsru_y[i,])
      }
    }
  }

  if (compare[1] == "SMR"){
    for (i in 1:nrow(tab)){
      if (tab[,3][i] == "ME" && tab[,4][i] == "OVER" | tab[,3][i] == "ME" && tab[,4][i] == "LE" | tab[,3][i] == "UNDER" && tab[,4][i] == "LE" | tab[,3][i] == "UNDER" && tab[,4][i] == "OVER"){
        worse_x <- rbind(worse_x, smrsru_x[i,])
        worse_y <- rbind(worse_y, smrsru_y[i,])
      }
      if (tab[,3][i] == "OVER" && tab[,4][i] == "ME" | tab[,3][i] == "OVER" && tab[,4][i] == "UNDER" | tab[,3][i] == "LE" && tab[,4][i] == "ME" | tab[,3][i] == "LE" && tab[,4][i] == "UNDER"){
        better_x <- rbind(better_x, smrsru_x[i,])
        better_y <- rbind(better_y, smrsru_y[i,])
      }
    }
  }

  if (compare[1] == "BOTH"){
    for (i in 1:nrow(tab)){
      if (tab[,3][i] == "ME" && tab[,4][i] == "OVER" | tab[,3][i] == "ME" && tab[,4][i] == "LE" | tab[,3][i] == "ME" && tab[,4][i] == "UNDER" | tab[,3][i] == "OVER" && tab[,4][i] == "LE" | tab[,3][i] == "UNDER" && tab[,4][i] == "LE" ){
        worse_x <- rbind(worse_x, smrsru_x[i,])
        worse_y <- rbind(worse_y, smrsru_y[i,])
      }
      if (tab[,3][i] == "OVER" && tab[,4][i] == "ME" | tab[,3][i] == "LE" && tab[,4][i] == "ME" | tab[,3][i] == "UNDER" && tab[,4][i] == "ME"  | tab[,3][i] == "LE" && tab[,4][i] == "UNDER" | tab[,3][i] == "LE" && tab[,4][i] == "ME" | tab[,3][i] == "LE" && tab[,4][i] == "OVER" | tab[,3][i] == "UNDER" && tab[,4][i] == "ME" ){
        better_x <- rbind(better_x, smrsru_x[i,])
        better_y <- rbind(better_y, smrsru_y[i,])
      }
    }
  }

  output <- list(tab = tab, smrsru_y = smrsru_y, nchanges = nchanges, smrsru_x = smrsru_x, med_x = M_x, med_y = M_y, tert_x = c(Q1_x,Q2_x), tert_y = c(Q1_y,Q2_y), sru_x = sru_x, smr_x = smr_x, sru_y = sru_y, smr_y =  smr_y, worse_y = worse_y, worse_x = worse_x, better_y = better_y, better_x = better_x)

  class(output) <- "reclass"

  if (plot == TRUE){plot(output)}

  output
}

#' @rdname reclass
#' @export
print.reclass <- function(x, ...){
  print(x$tab, ...)
  cat("------------------------------\n")
  cat("Total:", nrow(x$tab))
}

#' @rdname reclass
#' @export
plot.reclass <- function(x, ..., xlim_x = range(x$smr_x), ylim_x = range(x$sru_x), xlim_y = range(x$smr_y), ylim_y = range(x$sru_y), xlab = "SMR", ylab= "SRU", points.arg_x = list(pch = 21, col = "white", bg = "yellow", cex = 2), points.arg_y = list(pch = 21, col = "white", bg = "yellow", cex = 2), med.arg_x = list(col = "dodgerblue4",lwd = 2,lty = 1), med.arg_y = list(col = "dodgerblue4", lwd = 2, lty = 1), tert.arg_x = list(col = "darkorange2", lty = 2, lwd = 1), tert.arg_y = list(col = "darkorange2", lty = 2, lwd = 1), text.arg_x = list(labels = seq(1,nrow(x$tab)), cex=.6), text.arg_y = list(labels = seq(1,nrow(x$tab)), cex=.6), worse.arg_x = list(x = x$worse_x, pch = 21, col = "white", bg = "tomato", cex = 2), worse.arg_y = list(x = x$worse_y, pch = 21, col = "white", bg = "tomato", cex = 2), better.arg_x = list(x = x$better_x, pch = 21, col = "white", bg = "mediumseagreen", cex = 2), better.arg_y = list(x = x$better_y, pch = 21, col = "white", bg = "mediumseagreen", cex = 2), auto.legend = TRUE, leg.arg = list(x = "topleft", bty = "n", xpd = NA, inset = c(-1.8,-.2), ncol = 1, horiz = F, pch = 19, cex = .8, pt.cex = 1.5), main.arg_x = list(main = "1st Stage"), main.arg_y = list(main = "2nd Stage")){
  med.arg_x$v <- x$med_x[2]
  med.arg_x$h <- x$med_x[1]
  med.arg_y$v <- x$med_y[2]
  med.arg_y$h <- x$med_y[1]
  tert.arg_x$h <- x$tert_x[1:2]
  tert.arg_x$v <- x$tert_x[3:4]
  tert.arg_y$h <- x$tert_y[1:2]
  tert.arg_y$v <- x$tert_y[3:4]
  points.arg_x$x <- cbind(x$smr_x, x$sru_x)
  points.arg_y$x <- cbind(x$smr_y, x$sru_y)
  text.arg_x$x <- x$smrsru_x
  text.arg_y$x <- x$smrsru_y

  par(mfrow = c(1,2))
  plot(0, 0, ..., xlim = xlim_x, ylim = ylim_x, type = 'n', xlab = "", ylab = "", main = main.arg_x[[1]])

  mtext(text = xlab, side = 1, line = 2.5, font = 2)

  mtext(text = ylab, side = 2, line = 2.5, font = 2)

  do.call(abline, med.arg_x)
  do.call(abline, tert.arg_x)
  do.call(points, points.arg_x)
  do.call(points, worse.arg_x)
  do.call(points, better.arg_x)
  do.call(text, text.arg_x)

  plot(0, 0, ..., xlim = xlim_y, ylim = ylim_y, type = 'n', xlab = "", ylab = "", main = main.arg_y[[1]])

  mtext(text = xlab, side = 1, line = 2.5, font = 2)

  mtext(text = ylab, side = 2, line = 2.5, font = 2)

  do.call(abline, med.arg_y)
  do.call(abline, tert.arg_y)
  do.call(points, points.arg_y)
  do.call(points, worse.arg_y)
  do.call(points, better.arg_y)
  do.call(text, text.arg_y)
  if(auto.legend){
    leg.arg$legend <- c("Worse rank", "Better rank", "Unchanged rank")
    leg.arg$col <- c(worse.arg_y$bg, better.arg_y$bg, points.arg_y$bg)
    do.call(legend, leg.arg)
  }
  on.exit(par(mfrow = c(1,1)))
}

