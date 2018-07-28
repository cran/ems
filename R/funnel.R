#' @title Funnel plot for benchmarking health units
#'
#' @name funnel
#'
#' @description Produces a variety of funnel plots comparing health units or ICUs (intensive care units) making easy to identify those units which deviate from the group. There is a function that calculates all the values required and returns the values for all units and the funnel, and there is a function that calls graphical parameters from the former values. The options of funnels available are the funnel for rate, for ratio of rates, for proportions, for difference of proportions and for ratio of proportions.
#'
#' The funnel for rates are usually plots of either SMR or SRU at vertical axis. If the direct method is chosen, the horizontal axis will display the number of admissions. If the indirect method is chosen instead, the expected number of deaths will be displayed for SMR or the expected length of stay for SRU. As consequence of this differentiation, the interpretation regarding the clssification of the points displayed will be the same in every case.
#'
#' The funnel for ratio of rates are usually plots of ratios of SMRs (or SRUs) within the same units. These two SMRs are, for example, from the same units in different time periods. Therefore, it expresses how the SMR changed over time. If the number of expected deaths is different in both periods, the plot will return at the horizontal axis a parametrization of the geometric mean of the expected number of deaths in both periods for each unit. If the number of expected deaths is identical in both periods, the plot will return at the horizontal axis the arithmetic mean of the observed number of deaths in both periods for each unit.
#'
#' The funnel for proportions plots on the vertical axis the percentage of observed deaths of the units and on the horizontal axis the number (volume) of admissions. The funnel for ratio of proportions and for difference of proportions are usually used to express the fraction of deaths of the same units in different time period. Therefore, they express how the fraction of deaths changed over time in each unit. If one picks the difference of proportions, the horizontal axis will display a parametrization of the arithmetic mean of the number of admissions in both periods. If one picks the ratio of proportions, the horizontal axis will display a parametrization of the geometric mean of the number of admissions in both periods.
#'
#' @param unit A factor vector representing the unit names.
#'
#' @param y A numeric vector representing the "Standardized rate" for each unit, usually the SMR (Standardized Mortality Ratio), or possibly the SRU (Standardized Resource Use), accordind to \code{y.type} . It's also called "indicator".
#'
#' @param n A numeric vector representing the case volume, or number of admissions, for each unit.
#'
#' @param n1,n2 If one picks \code{option = "ratioRates"} or \code{option = "diffProp"} or \code{option = "ratioProp"}, then \code{n1} and \code{n2} are numeric vectors representing the total of admissions at 1st and 2nd periods, respectively.
#'
#' @param o A numeric vector representing the observed death. Acceptable values are 0 (absence) or 1 (presence).
#'
#' @param o1,o2 If one picks \code{option = "ratioRates"} or \code{option = "diffProp"} or \code{option = "ratioProp"}, then \code{o1} and \code{o2} are numeric vectors representing the observed deaths at 1st and 2nd periods, respectively.
#'
#' @param e Used only when \code{option = "rate"} and \code{direct = FALSE}. This is a numeric vector representing the expected number of deaths.
#'
#' @param e1,e2 If one picks \code{option = "ratioRates"}, \code{e1} and \code{e2} are numeric vectors representing the expected number of deaths at 1st and 2nd periods, respectively.
#'
#' @param lambda1,lambda2 Values correponding to the rate at which a death occurs in the institutions at the 1st and 2nd periods, respectively. It is assumed that the parameters \code{o1} and \code{o2} are distributed as \code{oi} ~ Poisson(\code{lambdai}) when \code{option = "ratioRates"}. The default value for \code{lambdai} is \code{lambdai = sum(oi)/sum(ni)}, where ni is the value of the parameter \code{n1} or \code{n2} when i equals 1 or 2. \code{lambdai} is the estimate for the mean of the poisson distribution.
#'
#' @param pi1,pi2 Values correponding to the probability for the occurrence of a death in the institutions at the 1st and 2nd periods, respectively. Its assumed that the paramenters \code{o1} and \code{o2} are distributed as \code{oi} ~ Bin(\code{pii,ni}) when \code{option = "diffProp"} or \code{option = "ratioProp"}. \code{ni} is the value of the parameter n1 or n2 when i equals 1 or 2. The default value for \code{pii} is \code{sum(oi)/sum(ni)}, the estimate for the mean of the poisson distribution.
#'
#' @param y.type A character vector representing the indicator type. It is used to name the vertical axis if \code{option = "rate"} or \code{option = "ratioRate"} and ignored otherwise. It usually is 'SMR' or 'SRU'.
#'
#' @param p A confidence level numeric vector. The function will return a confidence interval for each value in p. The default is 2 and 3 standard deviations (\code{p = c(.95, 998)}).
#'
#' @param theta The target value which specifies the desired expectation for institutions considered "in control". Used when \code{option = "prop"} or \code{option = "rate"}. Usually, this function internally estimates a theta to represent a central tendency of the group. However, one may want to set a pre-specified value for theta to indicate a "baseline" parameter for comparison (e.g 1 for \code{option = "rate"} or .20 for \code{option = "prop"}). If this is the case, the horizontal line representing theta may not be centralized in the funnel or may be even outside the funnel, making the plot look unusual.
#'
#' @param direct Logical (default = \code{FALSE}); Used when \code{option = "rate"}. If \code{TRUE}, we assume the vector of rates "y" is being reported as a rate per (say) 1000 individuals, and that it has been transformed to a proportion between 0 and 1. The associated error (horizontal axis) will be measured accordingly to the size of the populations \code{n}. The CI - confidence interval - is calculated by a binomial distribution. If \code{FALSE}, the associated error will be measured accordingly to the expected number of deaths \code{e}. The CI is calculated by a poisson distribution instead. See details.
#'
#' @param method There are two kinds of approximations for the CI, as mentioned in \code{direct} parameter. The one from the exact distribuition (binomial or poisson) and the one from the normal distribution. So, \code{method} is a character vector representing the kind of approximation desired, being \code{"exact"} (default) or \code{"normal"} the two options. It is used when \code{option = "rate"} or \code{option = "prop"}. The original report makes no formal comparison of which method is best, however it is mentioned that the funnels from different methods should look identical or very similar if all units have 100 or more observations. If any unit has less, the funnel from the normal approximation may mislead the interpretation. See details.
#'
#' @param myunits A numeric vector coded with 0 and 1 indicating which units one would like to benchmark among all units. These will be highlighted with dots of different collors in the plot.
#'
#' @param option A character specifying the type of funnel plot one wants to produce. It can assume \code{"rate"}, \code{"ratioRates"}, \code{"prop"}, \code{"diffProp"} or \code{"ratioProp"}. If \code{option = "rate"}, \code{funnel} plots a standardized rate y versus the expected number of deaths or case volume (number of unit admissions) for all units. If \code{option = "ratioRate"}, \code{funnel} can be used to compare units at two diferent periods. It plots a ratio of rates y versus a precision parameter rho. If \code{option = "prop"}, \code{funnel} plots a proportion y versus its case volume (number of admissions). If \code{option = "ratioProp"} or \code{option = "diffProp"}, \code{funnel} can be used to compare units at two diferent periods. It plots a ratio (or difference) of proportions y versus a precision parameter rho. See details.
#'
#'
#' @param printUnits Logical (default = \code{TRUE}); If \code{TRUE}, the units are identified in the plot and printed in de console. The numbers plotted correspond to the row numbers printed in the console.
#'
#' @param plot Logical; If \code{TRUE} (default), the correspondent graphic is plotted with the standard options.
#'
#' @param x An object of class 'funnel'.
#'
#' @param xlim,ylim Limits of horizontal and vertical axis. These limits are defined in the funnel plot and passed to \code{plot.funnel}. The user may redefine the limits in \code{plot.funnel}. Ultimately, these arguments are passed to \code{\link[graphics]{plot.default}}.
#'
#' @param col A character vector representing the colors for the CI funnel lines. Must have same length of \code{p} + 1 with the target line color in the last position.
#'
#' @param lwd A positive number specifying the lines width. It's the same for all lines in the plot. See \code{\link[graphics]{par}}.
#'
#' @param lty A numeric vector representing the CI lines types. See \code{\link[graphics]{par}}.
#'
#' @param bty A character string which represents the type of \code{\link[graphics]{box}} which is drawn around plots. See \code{\link[graphics]{par}}.
#'
#' @param pch Either an integer or a single character specifying a symbol to be used as the default in plotting points. See \code{\link[graphics]{points}} for possible values and their interpretation. Note that only integers and single-character strings can be set as a graphics parameter (and not NA nor NULL).
#'
#' @param pt.col A character specifying the points colors.
#'
#' @param bg A character specifying the color to be used for the points background when \code{pch = 21} (default). See \code{\link[graphics]{par}}.
#'
#' @param pt.cex A numerical value giving the amount by which plotting points should be magnified relative to the default.  See \code{\link[graphics]{par}}.
#'
#' @param auto.legend Logical; If \code{TRUE} (default), prints a legend with default arguments.
#'
#' @param text.cex A numerical value giving the amount by which plotting text should be magnified relative to the default.  See \code{\link[graphics]{par}}.
#'
#' @param text.pos A position specifier for numbers that correspond to the units in the plot. Values of 1, 2, 3 and 4, respectively indicate positions below, to the left of, above and to the right of the points.
#'
#' @param mypts.col A character representing the color used to benchmark the units specified in \code{myunits}.
#'
#' @param xlab,ylab A title for the x and y axis. See \code{\link[graphics]{title}}
#'
#' @param digits Integer indicating the number of decimals to be used in the output.
#'
#' @param ... Further arguments passed to \code{\link[graphics]{plot}}.
#'
#' @param overdispersion Logical (default = FALSE); If TRUE, introduces an multiplicative over-dispersion factor phi that will inflate the CI null variance. See details.
#'
#' @param auto.xlab,auto.ylab Logical. If \code{TRUE}, one is not able to change x and y axis labels, respectively.
#'
#' @details
#' \itemize{
#' \item For every possible value of \code{option}, if \code{overdispersion = TRUE}, the CI can be inflated by a overdispersion parameter phi. There is a test for overdispersion which inflates the funnel if it's necessary. An "Winsorized" over-dispersion parameter is estimated and is used to inflate the funnel limits if it is significantly greater than 1. The parameter phi is returned as an funnel object.
#'
#' \item If \code{option = "rate"}, \code{funnel} plots a standardized rate y versus the expected number of deaths or volume value for several units.
#'
#' To choose the \code{direct} argument, one should pay attention if one wants to use a Direct or Indirect Standardized Rate. If direct, we assume the rate is reported as a rate per (say) 1000 individuals, then it is treated as a proportion. If indirect, we assume it is a cross-sectional data that leads to a standardized event ratio.
#'
#' In many circumstances we can assume an exact or approximate normal distribution for the data. Using the \code{method} argument, one could choose between \code{"exact"} or  \code{"normal"}. For direct standardized rates, the exact distribuition is binomial and for indirect standardized rates, the exact distribuition is poisson. Assume rho is the precision parameter (volume, for direct rates; expected value, for indirect rates). The original report claims that, for rho > 100, the normal and exact curves almost coincide. So, one could perfectly use  normal approximation if ones data parameter precision is greater than 100, in general.
#'
#' The console warns if there are units with volume/expected value less than 100.
#'
#' phi = (1/total) * sum((y - theta) ^ 2 * rho)/g(theta)
#'
#' var(y|theta,rho) = (phi * g(theta))/rho
#'
#' \item If \code{option = "ratioRate"}, \code{funnel} can be used to compare units at two diferent periods. It plots a ratio of rates y versus a precision parameter rho.
#'
#' Suppose we have two measures for each institution: O1; E1 in a baseline period and O2; E2 in a subsequent period, and we wish to assess the change in the underlying rate (SMR or SRU). We shall only consider the ratio of rates option. The exact method will automatically be applied if E1 = E2, and the indirect method, of normal approximations, otherwise. On this second method, for low (especially zero) counts the \code{funnel} function adds 0.5 to all parameters O and E in order to stabilize the estimates.
#'
#' Y = (O1/E1)/(O2/E2) and the target theta =	lambda2/lambda1.
#'
#'
#' When E1 = E2, y is plotted versus the average observed count (rho).
#'
#' When E1 is different of E2, i.e., it is used normal approximation. It is convenient to work on a logarithmic scale so that log(theta) is a target for log(Y). Y is plotted versus a different rho depending on the chosen rate.
#'
#' \item If \code{option = "prop"}, \code{funnel} plots a proportion y versus its volume.
#' It is used for cross-sectional data. Suppose in each institution that O events are observed out of a sample size of N:
#'
#' The indicator is the observed proportion y = O/N
#'
#' Assume \code{N} is the precision parameter (volume). Similarly to when \code{option = "rate"}, for \code{N} > 100 the normal and exact curves almost coincide. So, one could perfectly use normal approximation on the parameter \code{method} if ones data parameter precision is greater than 100, in general.
#'
#' phi = (1/total) * sum((y - theta) ^ 2 * N)/g(theta)
#'
#' var(y|theta,N) = (phi * g(theta))/N
#'
#' \item If \code{option = "ratioProp"} or \code{option = "diffProp"}, \code{funnel} can be used to compare units at two diferent periods. It plots a ratio (or difference) of proportions y versus a precision parameter rho to assess the change in the underlying proportion from pi1 to pi2. Normal approximations are used throughout, and for low (especially zero) counts, the function adds 0.5 to all arguments r and 1 to all arguments n in order to stabilize the estimates.
#'
#' In the case \code{option = "diffProp"}, the indicator is Y = (O2/N2 - O1/N1) and theta = pi2 - pi1. If \code{option = "ratioProp"}, the indicator is Y = (O2/N2)/(O1/N1) and theta = pi2/pi1. It is convenient to work on a logarithmic scale, so that log(theta) is a target for log(Y) in this case as well.
#'
#' For these two parameter options, the precision parameter (plotted at horizontal axis) can be interpreted as approximately the sample size per period.
#'}
#'
#' @return A table with unit names, y, observed (Obs), expected (Exp) and admissions (N) for each unit, a binary column showing which units one would like to highlight in the plot (myunits) and final columns show which units are out of control.
#'
#' @seealso \code{\link{SMR}}, \code{\link{SRU}}, \code{\link{reclass}}
#'
#' @references Spiegelhalter, David J. "Funnel plots for comparing institutional performance." Statistics in medicine 24.8 (2005): 1185-1202.
#'
#' @examples
#' # Loading data
#'data(icu)
#'
#'# Some edition
#'icu$Saps3DeathProbabilityStandardEquation <- icu$Saps3DeathProbabilityStandardEquation / 100
#'icu <- icu[-which(icu$Unit == "F"),]
#'icu$myunits <- ifelse(icu$Unit == "A",1,0) #my units
#'icu <- droplevels(icu)
#'
#'# Getting the cross-sectional arguments to use in funnel
#'x <- SMR.table(data = icu, group.var = "Unit",
#'               obs.var = "UnitDischargeName", pred.var = "Saps3DeathProbabilityStandardEquation")
#'myunit_names <- unique(icu$Unit[which(icu$myunits == 1)])
#'x$myunits <- ifelse(x$Levels %in% myunit_names, 1,0)
#'
#'# Analysis of proportions
#'f1 <- funnel(unit = x$Levels[-1], o = x[-1,]$Observed, theta = x$Observed[1] / x$N[1],
#'             n = x[-1,]$N, method = "exact", myunits = x$myunits[-1], option = "prop", plot = FALSE)
#'f1
#'plot(f1, main = "Cross-sectional proportions")
#'
#'# To analyze rates (SMR)
#'f2 <- funnel(unit = x$Levels[-1], y = x[-1,]$SMR, method = "exact", direct = TRUE,
#'             theta = x$SMR[1], e = x[-1,]$Expected, n = x[-1,]$N, o = x[-1,]$Observed,
#'             option = "rate", plot = FALSE)
#'f2
#'plot(f2, main = "Cross-sectional rate (SMR)")
#'
#'# Creating a variable containing month information about each admission
#'icu$month <- as.numeric(format(as.Date(icu$UnitAdmissionDateTime),"%m"))
#'
#'# First quarter
#'dt1 <- icu[which(icu$month %in% c(1,2,3)),]
#'
#'# Second quarter
#'dt2 <- icu[which(icu$month %in% c(4,5,6)),]
#'
#'# Getting the two period arguments to use in funnel
#'z <- SMR.table(data = dt1, group.var = "Unit", obs.var = "UnitDischargeName",
#'               pred.var = "Saps3DeathProbabilityStandardEquation")
#'w <- SMR.table(data = dt2, group.var = "Unit", obs.var = "UnitDischargeName",
#'               pred.var = "Saps3DeathProbabilityStandardEquation")
#'
#'z$myunits <- ifelse(z$Levels %in% myunit_names, 1,0)
#'w$myunits <- ifelse(w$Levels %in% myunit_names, 1,0)
#'# To analyze periods using ratio rates with e1 = e1
#'f3 <- funnel(unit = z$Levels[-1], n1 = z$N[-1], o1 = z$Observed[-1],
#'             e1 = z$Expected[-1],
#'             n2 = w$N[-1], o2 = w$Observed[-1], e2 = z$Expected[-1],
#'             myunits = z$myunits[-1], option = "ratioRates", plot = FALSE)
#'f3
#'plot(f3, main = "Ratio of SMRs of periods with same expectation of death")
#'
#'
#'# To analyze periods using ratio rates with e1 =! e1
#'f4 <- funnel(unit <- z$Levels[-1], n1 = z$N[-1], o1 = z$Observed[-1],
#'             e1 = z$Expected[-1], n2 = w$N[-1], o2 = w$Observed[-1], e2 = w$Expected[-1],
#'             option = "ratioRates", plot = FALSE)
#'f4
#'plot(f4, main = "Ratio of SMRs of periods with different expectation of death",
#'     ylim = c(-1.5,1.5), xlim = c(0,200))
#'
#'# To analyze periods by difference in proportions
#'f5 <- funnel(unit <- z$Levels[-1], n1 = z$N[-1], o1 = z$Observed[-1],
#'             n2 = w$N[-1], o2 = w$Observed[-1], option = "diffProp", plot = FALSE)
#'f5
#'plot(f5, main = "Difference in proportions of death for two periods")
#'
#'# To analyze periods by ratio of proportions
#'f6 <- funnel(unit <- z$Levels[-1], n1 = z$N[-1], o1 = z$Observed[-1],
#'             n2 = w$N[-1], o2 = w$Observed[-1], option = "ratioProp", plot = FALSE)
#'f6
#'plot(f6, main = "Ratio of proportions of death for two periods")
#'
#'rm(icu, x, z, w, dt1, dt2, unit, f1, f2, f3, f4, f5, f6)
#'
#' @import stats
#' @import graphics
#' @export


funnel <- function(unit, y, n, n1, n2, o, o1, o2, e, e1, e2, lambda1 = sum(o1)/sum(n1), lambda2 = sum(o2)/sum(n2), pi1 = sum(o1)/sum(n1), pi2 = sum(o2)/sum(n2), y.type = c("SMR","SRU"), p = c(.95,.998), theta, method = c("exact","normal"), direct = FALSE, myunits = rep(0,length(unit)), option = c("rate", "ratioRates", "prop", "diffProp", "ratioProp"), printUnits = TRUE, plot = TRUE, digits = 5, overdispersion = FALSE, ...){

  if (option[1] != "rate" && option[1] != "ratioRates" && option[1] != "prop" && option[1] != "diffProp" && option[1] != "ratioProp"){stop("option must be either 'rate', 'ratioRates', 'prop', 'diffprop' or 'ratioProp'.")}
  if (!is.logical(plot)){stop("plot must be TRUE or FALSE.")}

  if (option[1] == "rate"){
    output <- rateFunnel(unit = unit, y = y, n = n, o = o, e = e, y.type = y.type, p = p, theta = theta, method = method, direct = direct, myunits = myunits, printUnits = printUnits, digits = digits, overdispersion = overdispersion)
  }
  if (option[1] == "ratioRates"){
    output <- changeRateFunnel(unit = unit, n1 = n1, n2 = n2, o1 = o1, e1 = e1, o2 = o2, e2 = e2, lambda1 = lambda1, lambda2 = lambda2, y.type = y.type, p = p, myunits = myunits, printUnits = printUnits, digits = digits, overdispersion = overdispersion)
  }
  if (option[1] == "prop"){
    output <- propFunnel(unit = unit, o = o, n = n, theta = theta, p = p, method = method, myunits = myunits, printUnits = printUnits, digits = digits, overdispersion = overdispersion)
  }
  if (option[1] == "diffProp"){
    output <- changePropFunnel(unit = unit, o1 = o1, o2 = o2, n1 = n1, n2 = n2, p = p, pi1 = pi1, pi2 = pi2, method = "diff", myunits = myunits, printUnits = printUnits, digits = digits, overdispersion = overdispersion)
  }
  if (option[1] == "ratioProp"){
    output <- changePropFunnel(unit = unit, o1 = o1, o2 = o2, n1 = n1, n2 = n2, p = p, pi1 = pi1, pi2 = pi2, method = "ratio", myunits = myunits, printUnits = printUnits, digits = digits, overdispersion = overdispersion)
  }

  class(output) <- "funnel"
  if (plot) {plot(output, ...)}
  if (printUnits) {output}

}

#' @rdname funnel
#' @export
print.funnel <- function(x,...){
  print(x$tab, ...)
}

#' @rdname funnel
#' @export
plot.funnel <- function(x, ...,col = c("darkblue","paleturquoise3","gray26"), lwd = 2, lty = c(2,6,1), bty = "n", pch = 21, pt.col = "white", bg = "orange", pt.cex = 1.5, auto.legend = TRUE, text.cex = 0.7, text.pos = NULL, mypts.col = "darkblue", printUnits = x$printUnits, xlab = x$xlab, ylab = x$ylab, xlim = x$xlim, ylim = x$ylim){

  if (length(col) != length(x$p)+1){stop("col must have same length of p + 1 for the target line color in the last position")}
  if (!is.logical(auto.legend)){stop("auto.legend must be TRUE or FALSE.")}

    plot(x$x, x$y, ..., type = "n", xlim = xlim, ylim = ylim, bty = bty, xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    grid()
    axis(side = 1, tick = T, pos = NA, lwd = 0, lwd.ticks = 1, col.axis = "gray49", col.ticks = "lightgray", tcl = -1, lty = 3)
    axis(side = 2, tick = T, pos = NA, lwd = 0, lwd.ticks = 1, col.axis = "gray49", col.ticks = "lightgray", tcl = -1, lty = 3)

    mtext(text = xlab, side = 1, line = 2.5, font = 2)

    mtext(text = ylab, side = 2, line = 2.5, font = 2)



    points(x$x, x$y, pch = pch, col = pt.col, bg = bg, cex = pt.cex)

    for (i in 1:length(x$p)){
      lines(x$range, x$upperCI[[i]], col = col[i], lwd = lwd, lty = lty[i])
      lines(x$range, x$lowerCI[[i]], col = col[i], lwd = lwd, lty = lty[i])
    }

    abline(h = x$theta, col = col[length(x$p)+1], lwd = lwd)

    if (any(x$myunits == 1)){
      points(x$x[which(x$myunits == 1)], x$y[which(x$myunits == 1)], pch = pch, col = pt.col, bg = mypts.col, cex = pt.cex)
    }
    if (printUnits) {

      if (any(x$myunits == 1)){
        par(font = 2)
        text(x$x[-which(x$myunits == 1)], x$y[-which(x$myunits == 1)], labels = rownames(x$unitnames)[-which(x$myunits == 1)], cex = text.cex, pos = text.pos)

        text(x$x[which(x$myunits == 1)], x$y[which(x$myunits == 1)], labels = rownames(x$unitnames)[which(x$myunits == 1)], cex = text.cex, pos = length(text.pos) + 1)
        par(font = 1)
      } else {
        par(font = 2)
        text(x$x, x$y, labels = rownames(x$unitnames), cex = text.cex, pos = text.pos)
        par(font = 1)
      }
    }
    if (auto.legend){
      legend.arg <- c()
      for (i in 1:length(x$p)){
        legend.arg <- append(legend.arg, paste0(x$p[i]*100,"% limits"))
      }
      legend.arg <- append(legend.arg, paste0("Theta = ",format(round(x$theta,2), nsmall = 2)))
      legend(x = "topright", legend = as.expression(legend.arg), lwd = lwd, lty = lty, bty = "n", col = col[0:length(x$p)+1], xpd = NA, inset = c(0,-.15))
    }
    on.exit(par(mfrow = c(1,1)))
}


#' @import stats
#' @import graphics

#' @rdname funnel
#' @export
rateFunnel <- function(unit, y, n, o, e, y.type, p, theta = 1, method = c("exact","normal"), direct, ..., printUnits, auto.xlab = TRUE, xlab = c("Volume of cases","Expected values"), ylab = y.type[1], xlim = c(0, max(rho)), ylim = c(min(lowerCI[[which(p == max(p))]]), max(upperCI[[which(p == max(p))]])), myunits, digits, overdispersion){

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
  # if (any(! myunits %in% unit)){
  #   warning(paste0("There is no unit called ", myunits[which(! myunits %in% unit)], "\n"))
  #   myunits <- myunits[-which(! myunits %in% unit)]
  # }
  if(any(myunits != 0 & myunits != 1)){stop("myunits must be coded as 0 and 1.")}
  if (direct){
    rates.table <- data.frame(unit, Rate = y, Admissions = n, Observed = o, myunits)
    thirdcolname <-  "Admissions"
  } else {
    rates.table <- data.frame(unit, Rate = y, Expected = e, Observed = o, myunits)
    thirdcolname <- "Expected"
  }

  if (length(exc) > 0){
    rates.table <- rates.table[-which(rates.table$unit %in% exc),]
    # if (any(myunits %in% exc)){
    #   myunits <- myunits[-which(myunits %in% exc)]
    # }
  }

  upperCI <- list()
  lowerCI <- list()
  ylowCI <- list()
  yuppCI <- list()
  lowOUT <- list()
  uppOUT <- list()
  outofcontrol <- list()
  outcolname <- c()
  # phi <- NULL # will be calculated only if overdispersion = TRUE and method = "normal"

  totalAdmissions <- sum(n)
  totalObserved <- sum(o)

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
      gdetheta <- theta * (1 - theta)
      # prob <- observedRange/admissionsRange
      # prob <- max(observed)/max(admissions)
      for (i in 1:length(p)){
        # rp <- qbinom(p[i], size = admissionsRange, prob)
        # alpha <- (pbinom(rp, size = admissionsRange, prob) - p[i]) / ((pbinom(rp, size = admissionsRange, prob)) - pbinom(rp - 1, size = admissionsRange, prob))
        # upperCI[[i]] <- theta + (rp - alpha) / admissionsRange
        # lowerCI[[i]] <- theta - (rp - alpha) / admissionsRange

        CI <- funnelEstimate(y = rates.table$Rate, range = admissionsRange, u = length(unique(rates.table$unit)), totalAdmissions = totalAdmissions, totalObserved = totalObserved, p = p[i], theta = theta, overdispersion = overdispersion, dist = 'binomial', rho = rho, gdetheta = gdetheta)
        upperCI[[i]] <- CI$upperCI
        lowerCI[[i]] <- CI$lowerCI
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

      gdetheta <- theta
      # Calculate the z-score
      # z_score <- (y - theta) * sqrt( rho / theta)
      #
      # phi <- winsorising(z_score = z_score, n = rates.table$Admissions, u = length(unique(rates.table$unit)))

      # # Calculate the 10% and 90% percentiles.
      # q90 <- quantile(z_score,probs=c(0.9))
      # q10 <- quantile(z_score,probs=c(0.1))
      #
      # # Set z-scores larger than the 90% percentile to the 90% percentile.
      # z_score <- ifelse(z_score>q90,q90,z_score)
      #
      # # Set z-scores smaller than the 10% percentile to the 10% percentile.
      # z_score <- ifelse(z_score<q10,q10,z_score)
      #
      # # Calculate the Winsorised estimate
      # # Used when overdispersion of the indicator
      # phi <- (1 / nrow(rates.table)) * sum(z_score ^ 2)

#
#       if (overdispersion){
#         if (phi > (1 + 2 * sqrt( 2 / nrow(rates.table) ))){ # overdispersion = TRUE
#           # phi <- (1/nrow(rates.table)) * sum(((y - theta) ^ 2 * admissions)/(theta))
#           warning("The funnel limits were inflated due overdispersion presence.")
#           for (i in 1:length(p)){
#             zp <- qnorm(1 - (1 - p[i]) / 2)
#             upperCI[[i]] <- theta + zp * sqrt(theta * phi / admissionsRange)
#             lowerCI[[i]] <- theta - zp * sqrt(theta * phi / admissionsRange)
#             ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% admissions == TRUE)]
#             yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% admissions == TRUE)]
#             lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
#             uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
#             outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
#           }
#         }
#       } else {
        for (i in 1:length(p)){
          # zp <- qnorm(1 - (1 - p[i]) / 2)
          # upperCI[[i]] <- theta + zp * sqrt(theta / admissionsRange)
          # lowerCI[[i]] <- theta - zp * sqrt(theta / admissionsRange)

          CI <- funnelEstimate(y = rates.table$Rate, range = admissionsRange, u = length(unique(rates.table$unit)), totalAdmissions = totalAdmissions, totalObserved = totalObserved, p = p[i], theta = theta, overdispersion = overdispersion, dist = "normal", rho = rho, gdetheta = gdetheta)
          upperCI[[i]] <- CI$upperCI
          lowerCI[[i]] <- CI$lowerCI
           ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% admissions == TRUE)]
          yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% admissions == TRUE)]
          lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
          uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
          outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
          outcolname[i] <- paste0(p[i]*100,"%CI")
        }
    # }

      rates.table <- cbind(rates.table, outofcontrol)
      x <- admissions; y <- smr; range <- admissionsRange
    }


  } else { # indirect method
    rates.table <-  rates.table[order(rates.table$Expected),] # ordering by expected death
    smr <- rates.table$Rate
    e <- rates.table$Expected
    expectedRange <- seq(1, max(e)) #using expected death as precision parameter
    rho <- e
    unitnames <- data.frame(Unit = rates.table$unit)
    if (auto.xlab){ xlab = xlab[2]}
    # "exact" formula using poisson approximation
    if (method[1] == "exact"){
      # lambda <- theta*expectedRange
     lambda <- theta
     gdetheta <- theta

       for (i in 1:length(p)){
        # rp <- qpois(p[i], lambda)
        # alpha <- (ppois(rp, lambda) - p[i]) / (ppois(rp, lambda) - ppois(rp - 1, lambda))
        # upperCI[[i]] <- theta + (rp - alpha)/expectedRange
        # lowerCI[[i]] <- theta - (rp - alpha)/expectedRange

        CI <- funnelEstimate(y = rates.table$Rate, range = expectedRange, u = length(unique(rates.table$unit)), totalAdmissions = totalAdmissions, totalObserved = totalObserved, p = p[i], theta = theta, overdispersion = overdispersion, dist = "poisson", rho = rho, gdetheta = gdetheta)
        upperCI[[i]] <- CI$upperCI
        lowerCI[[i]] <- CI$lowerCI

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

      gdetheta <- theta

      # # Calculate the z-score
      # z_score <- (y - theta) * sqrt( rho / theta)
      #
      # # Calculate the 10% and 90% percentiles.
      # q90 <- quantile(z_score,probs=c(0.9))
      # q10 <- quantile(z_score,probs=c(0.1))
      #
      # # Set z-scores larger than the 90% percentile to the 90% percentile.
      # z_score <- ifelse(z_score>q90,q90,z_score)
      #
      # # Set z-scores smaller than the 10% percentile to the 10% percentile.
      # z_score <- ifelse(z_score<q10,q10,z_score)
      #
      # # Calculate the Winsorised estimate
      # # Used when overdispersion of the indicator
      # phi <- (1 / nrow(rates.table)) * sum(z_score ^ 2)

      # if (overdispersion){
      #   if (phi > (1 + 2 * sqrt( 2 / nrow(rates.table) ))) { # overdispersion = TRUE
      #     # phi <- (1/nrow(rates.table)) * sum(((y - theta) ^ 2 * e)/(theta))
      #     warning("The funnel limits were inflated due overdispersion presence.")
      #     for (i in 1:length(p)){
      #       zp <- qnorm(1 - (1 - p[i]) / 2)
      #       upperCI[[i]] <- theta + zp * sqrt(theta * phi / expectedRange)
      #       lowerCI[[i]] <- theta - zp * sqrt(theta * phi / expectedRange)
      #       ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
      #       yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
      #       lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
      #       uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
      #       outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      #       outcolname[i] <- paste0(p[i]*100,"%CI")
      #     }
      #   }
      # } else {
        for (i in 1:length(p)){
          # zp <- qnorm(1 - (1 - p[i]) / 2)
          # upperCI[[i]] <- theta + zp * sqrt(theta/expectedRange)
          # lowerCI[[i]] <- theta - zp * sqrt(theta/expectedRange)

          CI <- funnelEstimate(y = rates.table$Rate, range = expectedRange, u = length(unique(rates.table$unit)), totalAdmissions = totalAdmissions, totalObserved = totalObserved, p = p[i], theta = theta, overdispersion = overdispersion, dist = "normal", rho = rho, gdetheta = gdetheta)
          upperCI[[i]] <- CI$upperCI
          lowerCI[[i]] <- CI$lowerCI

          ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
          yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% trunc(e) == TRUE)]
          lowOUT[[i]] <- ifelse(smr < ylowCI[[i]],TRUE, FALSE)
          uppOUT[[i]] <- ifelse(smr > yuppCI[[i]], TRUE, FALSE)
          outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
          outcolname[i] <- paste0(p[i]*100,"%CI")
        }
      # }
      rates.table <- cbind(rates.table, outofcontrol)
      x <- e; y <- smr; range <- expectedRange
    }
  }

  output <- list(x = x, y = y, theta = theta, range = range, tab = rates.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = rates.table$myunits, p = p, unitnames = unitnames, printUnits = printUnits)

  colnames(output$tab) <- c("Unit", y.type[1], thirdcolname, "Observed", "Myunits", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2] <- round(output$tab[,2], digits)
  output
}

#' @rdname funnel
#' @export
changeRateFunnel <- function(unit, n1, n2, o1, e1, o2, e2, lambda1, lambda2, y.type, p, ..., printUnits, auto.xlab = TRUE, xlab = c("Average observed count","Expectation per period"), auto.ylab = TRUE, ylab = c(paste0(y.type[1],"'s Ratio"),paste0("Log(", y.type[1],"'s Ratio)")), ylim = c(max(lowerCI[[which(p == max(p))]]) - 1.5*theta, min(upperCI[[which(p == max(p))]]) + 1.5*theta), xlim = c(0,max(rho)), myunits, digits, overdispersion){

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

  if(any(myunits != 0 & myunits != 1)){stop("myunits must be coded as 0 and 1.")}

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

  totalAdmissions <- sum(n1,n2)
  totalObserved <- sum(o1,o2)
  if (all.equal(e1, e2) == TRUE){
    warning("The expected values from 1st and 2nd periods are the same.")
    # then y = o2/o1
    # exact methods
    secondcolname <- "y"
    rho <- (o1 + o2)/2  # precision parameter: average observed count
    gdetheta <- (((o1 + o2) ^ 2) * theta) / (2 * (1 + theta) ^ 2)
    change.table <- data.frame(unit,y,o1,e1,n1,o2,e2,n2,rho,myunits)
    change.table <- change.table[order(change.table$rho),]
    if (length(exc) > 0){
      change.table <- change.table[-which(change.table$unit %in% exc),]
    }
    unitnames <- data.frame(Unit = change.table$unit)
    expectedRange <- seq(1, max(change.table$rho)*1.2)
    lambda <- theta

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow repeted values in xCI
      if (ceiling(change.table$rho)[i] == ceiling(change.table$rho)[i+1]){
        change.table$rho[i+1] <- change.table$rho[i+1] + 1
      }
    }
    for (i in 1:length(p)){

      CI <- funnelEstimate(y = change.table$y, range = expectedRange, u = length(unique(change.table$unit)), totalAdmissions = totalAdmissions, totalObserved = totalObserved, p = p[i], theta = theta, overdispersion = overdispersion, dist = "binomial", rho = rho, gdetheta = gdetheta)
      upperCI[[i]] <- CI$upperCI
      lowerCI[[i]] <- CI$lowerCI

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
    warning("The expected values from 1st and 2nd periods differ. Will be used logarithmic scale to plot values.")
    # normal approximation
    # logarithmic scale!!
    secondcolname <- "log(y)"
    lambdaghat <- (o1 + o2) / (theta ^ (1/2) * e2 + theta ^ (-1/2) * e1)
    lambdagbarra <- (sum(o1) + sum(o2)) / (sum(e1) + sum(e2)) #assuming e1 = e2 = e, then varlogy|lambdagbarra = gdethetha / e
    varlogy <- ((theta ^ (-1/2)) / (e2* lambdaghat)) + ((theta ^ (1/2)) / (e1* lambdaghat))
    gdetheta <- (theta ^ (-1/2) + theta ^ (1/2)) / lambdagbarra
    rho <- gdetheta / varlogy # precision parameter
    change.table <- data.frame(unit,"y" = log(y),o1,e1,n1,o2,e2,n2,rho,myunits)
    change.table <- change.table[order(change.table$rho),]
    if (length(exc) > 0){
      change.table <- change.table[-which(change.table$unit %in% exc),]

    }
    unitnames <- data.frame(Unit = change.table$unit)
    expectedRange <- seq(1, max(change.table$rho)+5)

   for (i  in 1:(length(change.table$rho)-1)){ # do not allow repeted values in xCI
      if (ceiling(change.table$rho)[i] == ceiling(change.table$rho)[i+1]){
        change.table$rho[i+1] <- change.table$rho[i+1] + 1
      }
    }

      for (i in 1:length(p)){

          CI <- funnelEstimate(y = change.table$y, range = expectedRange, u = length(unique(change.table$unit)), totalAdmissions = totalAdmissions, totalObserved = totalObserved, p = p[i], theta = log(theta), overdispersion = overdispersion, dist = "normal", rho = change.table$rho, gdetheta = gdetheta)
        upperCI[[i]] <- CI$upperCI
        lowerCI[[i]] <- CI$lowerCI

        ylowCI[[i]] <- lowerCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
        yuppCI[[i]] <- upperCI[[i]][which(expectedRange %in% ceiling(change.table$rho) == TRUE)]
        lowOUT[[i]] <- ifelse(change.table$y < ylowCI[[i]],TRUE, FALSE)
        uppOUT[[i]]<- ifelse(change.table$y > yuppCI[[i]], TRUE, FALSE)
        outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        outcolname[i] <- paste0(p[i]*100,"%CI")
        change.table <- cbind(change.table, outofcontrol[[i]])
      }


    if (auto.xlab){xlab = xlab[2]}
    if (auto.ylab){ylab = ylab[2]}

    theta <- log(theta)
  }

  x <- change.table$rho;y <- change.table$y; range <- expectedRange
  output <- list(x = x, y = y, theta = theta, range = range, tab = change.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = change.table$myunits, p = p, unitnames = unitnames, printUnits = printUnits)
  colnames(output$tab) <- c("Unit",secondcolname, "Obs1", "Exp1","N1", "Obs2","Exp2","N2","rho", "Myunits",outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2:9] <- round(output$tab[,2:9], digits)
  output
}

#' @rdname funnel
#' @export
propFunnel <- function(unit, o, n, theta, p, method = c("exact","normal"), ..., printUnits, ylab = "%", xlab = "Volume", ylim = c(0, min(upperCI[[which(p == max(p))]]) + 2.5*theta), xlim = c(0, max(n)), myunits, digits, overdispersion){

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

  if(any(myunits != 0 & myunits != 1)){stop("myunits must be coded as 0 and 1.")}

  rho <- n # precision parameter

  y <- (o / n)
  theta <- theta
  gdetheta <- theta * (1 - theta)

  upperCI <- list()
  lowerCI <- list()
  ylowCI <- list()
  yuppCI <- list()
  lowOUT <- list()
  uppOUT <- list()
  outofcontrol <- list()
  outcolname <- c()

  prop.table <- data.frame(unit, y, o, n, myunits)
  prop.table <- prop.table[order(prop.table$n),]

  if (length(exc) > 0){
    prop.table <- prop.table[-which(prop.table$unit %in% exc),]
  }
  unitnames <- data.frame(Unit = prop.table$unit)
  admissionsRange <- seq(1,max(n))
  observedRange <- seq(1, max(o), length.out = length(admissionsRange))
  prob <- max(o) / max(n)

  totalAdmissions <- sum(n)
  totalObserved <- sum(o)

  # using exact binomial approximation
  if (method[1] == "exact"){
    for (i in 1:length(p)){

      CI <- funnelEstimate(y = prop.table$y, range = admissionsRange, u = length(unique(prop.table$unit)), totalAdmissions = totalAdmissions, totalObserved = totalObserved, p = p[i], theta = theta, overdispersion = overdispersion, dist = "binomial", rho = rho, gdetheta = gdetheta)
      upperCI[[i]] <- CI$upperCI
      lowerCI[[i]] <- CI$lowerCI


      ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
      yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
      lowOUT[[i]] <- ifelse(prop.table$y < ylowCI[[i]],TRUE,FALSE)
      uppOUT[[i]]<- ifelse(prop.table$y > yuppCI[[i]], TRUE, FALSE)
      outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
      outcolname[i] <- paste0(p[i]*100,"%CI")
      prop.table <- cbind(prop.table, outofcontrol[[i]])

      upperCI[[i]] <- CI$upperCI * 100
      lowerCI[[i]] <- CI$lowerCI * 100
    }

  } else { # using normal approximation

    for (i in 1:length(p)){

        CI <- funnelEstimate(y = prop.table$y, range = admissionsRange, u = length(unique(prop.table$unit)), totalAdmissions = totalAdmissions, totalObserved = totalObserved, p = p[i], theta = theta, overdispersion = overdispersion, dist = "normal", rho = rho, gdetheta = gdetheta)
        upperCI[[i]] <- CI$upperCI
        lowerCI[[i]] <- CI$lowerCI

        ylowCI[[i]] <- lowerCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
        yuppCI[[i]] <- upperCI[[i]][which(admissionsRange %in% prop.table$n == TRUE)]
        lowOUT[[i]] <- ifelse(prop.table$y < ylowCI[[i]],TRUE,FALSE)
        uppOUT[[i]]<- ifelse(prop.table$y > yuppCI[[i]], TRUE, FALSE)
        outofcontrol[[i]] <- ifelse(lowOUT[[i]] == TRUE | uppOUT[[i]] == TRUE, "OUT","-")
        outcolname[i] <- paste0(p[i]*100,"%CI")
        prop.table <- cbind(prop.table, outofcontrol[[i]])

        upperCI[[i]] <- CI$upperCI * 100
        lowerCI[[i]] <- CI$lowerCI * 100

      }
    }

  theta <- theta * 100
  prop.table$y <- prop.table$y * 100

  x <- prop.table$n; y <- prop.table$y; range <- admissionsRange
  output <- list(x = x, y = y, theta = theta, range = range, tab = prop.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = prop.table$myunits, p = p, unitnames = unitnames, printUnits = printUnits)
  colnames(output$tab) <- c("Unit","y(%)", "Observed","Admissions", "Myunits", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2] <- round(output$tab[,2], digits)
  output
}


#' @rdname funnel
#' @export
changePropFunnel <- function(unit, o1, o2, n1, n2, p, pi1, pi2, method = c("diff","ratio"), ..., printUnits, xlab = "Sample size per period", auto.ylab = TRUE, ylab = c("Proportions difference","Proportions ratio log"), ylim = c(max(lowerCI[[which(p == max(p))]]) - 6*theta, min(upperCI[[which(p == max(p))]]) + 6*theta), xlim = c(0,max(rho)), myunits, digits, overdispersion){

  if(!is.factor(unit)){stop("Unit must be a factor.")}
  if(!is.numeric(n1)){stop("n1 must be numeric.")}
  if(!is.numeric(n2)){stop("n2 must be numeric.")}
  if (!is.numeric(o1)){stop("o1 must be numeric.")}
  if (!is.numeric(o2)){stop("o2 must be numeric.")}
  if (!is.logical(printUnits)){stop("printUnits must be TRUE or FALSE.")}
  if (!is.logical(overdispersion)){stop("overdispersion must be TRUE or FALSE.")}
  if (!is.logical(auto.ylab)){stop("auto.ylab must be TRUE or FALSE.")}
  if (method[1] != "diff" && method[1] != "ratio"){stop("method must be either 'diff' or 'ratio'.")}
  exc <- NULL
  if (any(n1 == 0 | n2 == 0)){
    exc <- unit[which(n1 == 0 | n2 == 0)]
    warning(paste0("The following units were excluded due to absence of observations: ", exc, "\n"))
  }

  if(any(myunits != 0 & myunits != 1)){stop("myunits must be coded as 0 and 1.")}

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

  totalAdmissions <- sum(n1,n2)
  totalObserved <- sum(o1,o2)

  if (method[1] == "diff"){
    secondcolname <- "y"
    y <- (o2/n2) - (o1/n1)
    theta <- pi2 - pi1
    pim <- (pi2 + pi1) / 2 # mean proportion
    vary <- ((pim + (theta / 2)) * (1 - pim - (theta / 2))) / n2 + ((pim - (theta / 2)) * (1 - pim + (theta / 2))) / n1
    gdetheta <- ((pim + (theta / 2)) * (1 - pim - (theta / 2))) + ((pim - (theta / 2)) * (1 - pim + (theta / 2)))
    rho <- gdetheta / vary
    change.table <- data.frame(unit,y,o1,n1,o2,n2,rho, myunits)
    change.table <- change.table[order(change.table$rho),]


    if (length(exc) > 0){
      change.table <- change.table[-which(change.table$unit %in% exc),]
       }
    unitnames <- data.frame(Unit = change.table$unit)
    expectedRange <- seq(0, max(change.table$rho)+5)

    for (i  in 1:(length(change.table$rho)-1)){ # do not allow repeted values in xCI
      if (ceiling(change.table$rho)[i] == ceiling(change.table$rho)[i+1]){
        change.table$rho[i+1] <- change.table$rho[i+1] + 1
      }
    }

    for (i in 1:length(p)){

        CI <- funnelEstimate(y = change.table$y, range = expectedRange, u = length(unique(change.table$unit)), totalAdmissions = totalAdmissions, totalObserved = totalObserved, p = p[i], theta = theta, overdispersion = overdispersion, dist = "normal", rho = rho, gdetheta = gdetheta)
        upperCI[[i]] <- CI$upperCI
        lowerCI[[i]] <- CI$lowerCI

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
    change.table <- data.frame(unit,"y" = log(y),o1,n1,o2,n2,rho, myunits)
    change.table <- change.table[order(change.table$rho),]


    if (length(exc) > 0){
      change.table <- change.table[-which(change.table$unit %in% exc),]
      # if (any(myunits %in% exc)){
      #   myunits <- myunits[-which(myunits %in% exc)]
      # }
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

        CI <- funnelEstimate(y = change.table$y, range = expectedRange, u = length(unique(change.table$unit)), totalAdmissions = totalAdmissions, totalObserved = totalObserved, p = p[i], theta = log(theta), overdispersion = overdispersion, dist = "normal", rho = rho, gdetheta = gdetheta)
        upperCI[[i]] <- CI$upperCI
        lowerCI[[i]] <- CI$lowerCI

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
  # }

  x <- change.table$rho; y <- change.table$y; range <- expectedRange
  output <- list(x = x, y = y, theta = theta, range = range, tab = change.table, upperCI = upperCI, lowerCI = lowerCI, xlab = xlab, ylab = ylab, xlim = xlim, ylim = ylim, myunits = change.table$myunits, p = p, unitnames = unitnames, printUnits = printUnits)
  colnames(output$tab) <- c("Unit",secondcolname, "Obs1", "N1", "Obs2","N2","rho","Myunits", outcolname)
  rownames(output$tab) <- seq(1,nrow(output$tab))
  output$tab[,2:7] <- round(output$tab[,2:7], digits)
  output
}

