#' Standardized Mortality Ratio (SMR)
#'
#' @name SMR
#'
#' @description Calculates the standardized mortality ratio and its confidence interval. SMR, for a group, is defined as the ratio of the observed deaths in this group and the sum of the predicted individual probabilities of death by any model (expected deaths).
#'
#' \code{SMR.table} estimates at once the overall SMR and the SMR across several groups, e.g. ICU units or clinical characteristics. The \code{SMR.table} can be ordered by the SMR estimate or its confidence intervals, facilitating the comparison of the units ranks.
#'
#' \code{forest.SMR} shows the \code{SMR.table} output as a forest plot. The plot opens two windows and plot at the left side the values from the \code{SMR.table} and at the right side the points and lines graphically representing each SMR and its confidence interval.
#'
#' @param obs.var Observed death. Accepted values are 0 (absence) or 1 (presence) in a vector. For \code{SMR.table} it must be a character indicating the name of the variable in the data.
#'
#' @param pred.var Death individual predictions (ranging from 0 to 1) in a vector. For \code{SMR.table} it must be a character indicating the name of the variable in the data.
#'
#' @param digits Number of digits for rounding the output.
#'
#' @param ci.method Method to estimate the confidence interval. "Hosmer" (default) or "Byar" are acceptable values.
#'
#' @param ci.level Level of the confidence interval. Default is 0.95.
#'
#' @param data For \code{SMR.table}, a dataset where pred.var, obs.var and group.var are in.
#'
#' @param group.var For \code{SMR.table}, this is a character vector indicating the name(s) of the variable(s) in the data that will form the groups where SMR will be calculated. The variables must be factors.
#'
#' @param use.label Logical. Default is FALSE. For \code{SMR.table} this option will replace the variables names by its labels in var.labels argument.
#'
#' @param var.labels A character vector with variables labels. The default is to replace the variable name by the label stored at attr(data, "var.labels"). But one may specify labels directly.
#'
#' @param reorder Default is "no". Possible values are: "no", "SMR","lower.Cl", and "upper.Cl". It will make the \code{SMR.table} to be ordered within each varibale by its original order, or by SMR order, or by lower.Cl order, or by upper.Cl.
#'
#' @param decreasing Logical. When 'reorderd' is TRUE, should the order be decreasing or incresing? See \code{\link[base]{order}}
#'
#' @param x For the \code{forest.SMR} this is the output of \code{SMR.table}.
#'
#' @param mar1,mar.SMR Values to set the margins (mar parameter) of left and right windows. See \code{\link[graphics]{par}}
#'
#' @param overall.arg A list of arguments passed to \code{\link[graphics]{text}} for plotting the overall label. Internally, 'y' coordinate is replaced.
#'
#' @param NOE.overall.args A list of arguments passed to \code{\link[graphics]{text}} for plotting the overall N (number of observations), O (observed deaths) and E (expected deaths). Internally, 'labels' and 'y' arguments are replaced.
#'
#' @param var.labels.arg A list of arguments passed to \code{\link[graphics]{text}} for plotting the variables labels. Internally, 'y' coordinate is replaced.
#'
#' @param cat.labels.arg A list of arguments passed to \code{\link[graphics]{text}} for plotting the categories labels. Internally, 'y' coordinate is replaced.
#'
#' @param N.values.arg A list of arguments passed to \code{\link[graphics]{text}} for plotting the values of N (number of observations) of each subgroup. Internally, the arguments 'label' and 'y' coordinate are replaced.
#'
#' @param O.values.arg A list of arguments passed to \code{\link[graphics]{text}} for plotting the values of Observed deaths of each subgroup. Internally, the arguments 'label'  and 'y' coordinate are replaced.
#'
#' @param E.values.arg A list of arguments passed to \code{\link[graphics]{text}} for plotting the values of Expected deaths of each subgroup. Internally, the arguments 'label' and 'y' coordinate are replaced.
#'
#' @param NOE.head.arg A list of arguments passed to \code{\link[graphics]{text}} for plotting the labels of the columns N, E and O on the top of the graph. Internally, the 'x' and 'y' coordinates are replaced. The x coordinates are taken from the x in \code{N.values.arg}, \code{O.values.arg} and \code{E.values.arg}.
#'
#' @param Overall.seg.arg A list of arguments passed to \code{\link[graphics]{segments}} for plotting the lines corresponding to overall SMR confidence intervals. Internally, 'x' and 'y' coordinates are replaced.
#'
#' @param Overall.p.arg A list of arguments passed to \code{\link[graphics]{points}} for plotting the points corresponding to overall SMR. Internally, 'x' and 'y' coordinates are replaced.
#'
#' @param Overall.est.arg A list of arguments passed to \code{\link[graphics]{text}} for plotting the overall SMR beside the graph. Internally, 'y' coordinate and 'label' argument are replaced.
#'
#' @param cat.seg.arg A list of arguments passed to \code{\link[graphics]{segments}} for plotting the lines corresponding to SMR confidence intervals for all groups. Internally, 'x' and 'y' coordinates are replaced.
#'
#' @param cat.p.arg A list of arguments passed to \code{\link[graphics]{points}} for plotting the points corresponding to all categoreis SMR. Internally, 'x' and 'y' coordinates are replaced.
#'
#' @param cat.est.arg A list of arguments passed to \code{\link[graphics]{text}} for plotting the categories SMR beside the graph. Internally, 'y' coordinate and 'label' arguments are replaced.
#'
#' @param SMR.head.arg  A list of arguments passed to \code{\link[graphics]{text}} for plotting the label of the SMR column on the top of the graph. Internally, the 'y' coordinate is replaced.
#'
#' @param smr.xlab Label of the x axis. Default is "Standardized Mortality Ratio".
#'
#' @param smr.xlim Limits of x axis of the \code{forest.SMR} plot. Default is "auto", which internally will pick the highest values of all upper.Cl and the lowest lower.Cl. Besides "auto", only a vector of 2 numbers is valid, and will be passed to \code{\link[graphics]{plot.default}}.
#'
#' @param grid Logical. If TRUE (default), it will draw a grid with the \code{\link[graphics]{grid}} default arguments.
#'
#' @return
#' If SMR, then:
#' \itemize{
#' \item \code{N} Number of subjects analyzed.
#' \item\code{O} Observed number of deaths.
#' \item \code{E} Expected number of deaths.
#' \item \code{SMR} Standardized mortality ratio.
#' \item \code{lower.Cl} lower confidence limit.
#' \item \code{upper.Cl} upper confidence limit.
#' }
#'
#' If SMR.table, then a data.frame with the same information as above, and the
#'  additional information is returned: "Variables" (variables names), "Levels" (variables levels).
#'
#' If forest.SMR, then a plot is returned.
#'
#' @author Lunna Borges and Pedro Brasil
#'
#' @seealso \code{\link{SRU}}, \code{\link{reclass}}, \code{\link{funnel}}
#'
#' @examples
#' # Loading a example data
#' data(icu)
#'
#' # Setting variable labels to data
#' attr(icu, "var.labels")[match(c("Unit", "IsMechanicalVentilation1h",
#'           "AdmissionTypeName_pri","Vasopressors_D1"), names(icu))] <-
#'   c("ICU unit","Mechanichal ventilation","Admission type","Vasopressors at admission")
#'
#' # Some editing
#' icu$Saps3DeathProbabilityStandardEquation <- icu$Saps3DeathProbabilityStandardEquation /100
#' icu$IsMechanicalVentilation1h <- as.factor(ifelse(icu$IsMechanicalVentilation1h == 1, "Yes", "No"))
#' icu$AdmissionTypeName_pri <- as.factor(icu$AdmissionTypeName_pri)
#' levels(icu$AdmissionTypeName_pri) <- c("Clinical","Elective surgery", "Urgent surgery")
#' icu$Vasopressors_D1 <- as.factor(ifelse(icu$Vasopressors_D1 == 1, "Yes", "No"))
#'
#' # The overall SMR for the whole sample
#' SMR(icu$UnitDischargeName, icu$Saps3DeathProbabilityStandardEquation)
#'
#' # The overall SMR and for some subgroups
#' x <- SMR.table(data = icu, obs.var = "UnitDischargeName",
#'                pred.var = "Saps3DeathProbabilityStandardEquation",
#'                group.var = c( "IsMechanicalVentilation1h",
#'                "AdmissionTypeName_pri","Vasopressors_D1"),
#'                reorder = "no",
#'                decreasing = TRUE,
#'                use.label = TRUE)
#' x
#'
#' # A forest plot for all groups SMR (resize the window may be required)
#' forest.SMR(x, digits = 2)
#'
#' # The same thing but reordering the categories
#' x <- SMR.table(data = icu, obs.var = "UnitDischargeName",
#'                pred.var = "Saps3DeathProbabilityStandardEquation",
#'                group.var = c( "IsMechanicalVentilation1h",
#'                "AdmissionTypeName_pri", "Vasopressors_D1"),
#'                reorder = "SMR",
#'                decreasing = TRUE,
#'                use.label = TRUE)
#' forest.SMR(x, digits = 2)
#'
#' # The overall SMR and for all Units
#' x <- SMR.table(data = icu, obs.var = "UnitDischargeName",
#'                pred.var = "Saps3DeathProbabilityStandardEquation",
#'                group.var = "Unit",
#'                reorder = "no",
#'                decreasing = TRUE,
#'                use.label = TRUE)
#' x
#'
#' # A forest plot for all Units
#' forest.SMR(x, digits = 2)
#'
#' # The same thing but reordering the categories
#' x <- SMR.table(data = icu, obs.var = "UnitDischargeName",
#'                pred.var = "Saps3DeathProbabilityStandardEquation",
#'                group.var = "Unit",
#'                reorder = "SMR",
#'                decreasing = TRUE,
#'                use.label = TRUE)
#' forest.SMR(x, digits = 2)
#'
#' rm(x, icu)
#' @references
#'
#' David W. Hosmer and Stanley Lemeshow. Confidence intervals estimates of an index of quality performance basend on logistic regression models. Statistics in Medicine , vol. 14, 2161-2172 (1995)
#'
#' @import stats
#' @export
SMR <- function(obs.var, pred.var, digits = 5, ci.method = c("Hosmer", "Byar"), ci.level = 0.95) {
  if (length(obs.var) != length(pred.var)){
    stop("Length of pred.var and obs.var differ.")
  }
  if (any(is.na(obs.var) | is.na(pred.var))) {
    stop("There are NAs either at pred.var or obs.var. Either remove the NAs or impute!")
  }
  if (!is.numeric(pred.var)) {
    stop("Predicted death variable must be numeric.")
  }
  if (any(min(pred.var) < 0 | max(pred.var) > 1)) {
    stop("The individual predicted death must range from 0 to 1.")
  }
  if (!is.numeric(obs.var)) {
    stop("Observed death variable must be numeric.")
  }
  if (any(obs.var != 0 & obs.var != 1)) {
    stop("Observed death variable must be coded as 0 and 1.")
  }
  if (ci.method[1] != "Byar" && ci.method[1] != "Hosmer") {
    stop("ci.method must be either 'Byar' or 'Hosmer'.")
  }
  if (!is.numeric(ci.level) || ci.level > 1 || ci.level < 0) {
    stop("ci.level must be numeric between 0 and 1.")
  }
  N <- length(pred.var)
  O <- length(obs.var[which(obs.var == 1)])
  E <- sum(pred.var)
  SMR <- O / E
  if (ci.method[1] == "Byar") {
    lowerCL <- O / E * (1 - 1 / (9 * O) - qnorm(1 - (1 - ci.level) / 2) / (3 * sqrt(O))) ^ 3
    upperCL <- (O + 1) / E * (1 - (1 / (9 * (O + 1))) + qnorm(1 - (1 - ci.level) / 2) /
                                (3 * sqrt(O + 1))) ^ 3
  }
  if (ci.method[1] == "Hosmer") {
    V2 <- sum(pred.var * (1 - pred.var))
    lowerCL <- exp(log(SMR) - qnorm(1 - (1 - ci.level) / 2) * sqrt(V2 / O ^ 2))
    upperCL <- exp(log(SMR) + qnorm(1 - (1 - ci.level) / 2) * sqrt(V2 / O ^ 2))
  }
  output <- c(N = N, Observed = O, Expected = E, SMR = SMR, lower.Cl = lowerCL,
              upper.Cl = upperCL)
  output <- round(output, digits = digits)
  output
}

#' @rdname SMR
#' @export
SMR.table <- function(data, group.var, obs.var, pred.var, digits = 5, use.label = FALSE, var.labels = attr(data, "var.labels")[match(group.var, names(data))], ci.method = c("Hosmer", "Byar"), ci.level = 0.95, reorder = c("no","SMR","lower.Cl","upper.Cl"), decreasing = FALSE) {
  if (any(is.na(match(group.var, names(data))))) {
    stop("One or more variables in group var is not a variable of data.")
  }
  if (!all(sapply(data[ , group.var], is.factor))) {
    stop("All variables in group.var must be factors.")
  }
  if (any(is.na(match(c(pred.var, obs.var), names(data))))) {
    stop("Either 'pred.var' or 'obs.var' is not a variable in data.")
  }
  if (use.label) {
    if (is.null(var.labels)) {
      stop("Either there is no label in attr(data,'var.labels') or 'var.labels argument was not set.")
    }
    if (length(group.var) != length(var.labels)){
      stop("The number of variables and labels are different.")
    }
    if (!is.character(var.labels)) {
      stop("'var.labels ' must be a character vector.")
    }
  }
  if (reorder !=  "no" && reorder !=  "SMR" && reorder !=  "lower.Cl" && reorder !=  "upper.Cl") {
    stop("'reorder' must be one of 'no','SMR','lower.Cl' or 'upper.Cl'")
  }
  cont.table <- sapply(1:length(group.var), function(i) table(data[, group.var[i]]))
  if (any(unlist(cont.table) == 0)) {
    warning("In SMR analysis, levels were deleted before analysis due to zero observations.")
    data[ , group.var] <- droplevels(data[ , group.var])
  }
  Variables <- c("Overall", unlist(sapply(1:length(group.var), function(i) rep(group.var[i], nlevels(data[ , group.var[i]])))))
  if (length(group.var) == 1) {
    Levels <- c(NA, levels(data[ , group.var]))
  } else {
    Levels <- c(NA, unname(unlist(sapply(data[ , group.var], levels))))
  }
  x <- data.frame(Variables, Levels)
  x$N <- NA
  x$Observed <- NA
  x$Expected <- NA
  x$SMR <- NA
  x$lower.Cl <- NA
  x$upper.Cl <- NA
  x[1,3:8] <- SMR(data[ , obs.var], data[, pred.var], digits = digits, ci.method = ci.method, ci.level = ci.level)
  for(i in 2:nrow(x)){
    cond <- which(data[ , as.character(x[i , "Variables"])] == as.character(x[i , 2]))
    x[i,3:8] <- SMR(data[cond , obs.var], data[cond , pred.var], digits = digits, ci.method = ci.method, ci.level = ci.level)
  }
  x$Variables <- as.character(x$Variables)
  if (reorder[1] == "SMR") {
    for (i in group.var) {
      # i = 1
      cond <- grep(i, x$Variables)
      y <- x[ cond, ]
      y <- y[ order(y$SMR, decreasing = decreasing), ]
      x[ cond, ] <- y
    }
  }
  if (reorder[1] == "lower.Cl") {
    # i = 2
    for (i in group.var) {
      cond <- grep(i, x$Variables)
      y <- x[ cond, ]
      y <- y[ order(y$lower.Cl, decreasing = decreasing), ]
      x[ cond, ] <- y
    }
  }
  if (reorder[1] == "upper.Cl") {
    for (i in group.var) {
      cond <- grep(i, x$Variables)
      y <- x[ cond, ]
      y <- y[ order(y$upper.Cl, decreasing = decreasing), ]
      x[ cond, ] <- y
    }
  }
  if (use.label) {
    names(group.var) <- var.labels
    for (i in 2:length(x$Variables)) {
           x$Variables[i] <- names(group.var)[which(x$Variables[i] == group.var)]
    }
  }
  for (i in length(x$Variables):2) {
    if (x$Variables[i - 1] == x$Variables[i]) {x$Variables[i] <- NA}
  }
  x
}

#' @rdname SMR
#' @import graphics
#' @import grDevices
#' @export
forest.SMR <- function(x,
                       mar1 = c(5.1, 1, 4.1, 1),
                       mar.SMR = c(5.1, 7, 4.1, 1),
                       overall.arg = list(x = .01, font = 2, las = 1, labels = var.labels[1], xpd = NA, adj = 0),
                       NOE.overall.args = list(x = c(N.values.arg$x, O.values.arg$x, E.values.arg$x), font = 2, las = 1, xpd = NA),
                       var.labels.arg = list(x = .01, font = 2, las = 1, cex = 1, xpd = NA, adj = 0),
                       cat.labels.arg = list(x = .1, font = 3, las = 1, cex = .95,  col = gray(.4), xpd = NA , adj = 0),
                       N.values.arg = list(x = .5, col = gray(.4), xpd = NA),
                       O.values.arg = list(x = .675, col = gray(.4), xpd = NA),
                       E.values.arg = list(x = .85, col = gray(.4), xpd = NA),
                       NOE.head.arg= list(font = 2, labels = c("N","O","E"), xpd = NA),
                       Overall.seg.arg = list(col = "navyblue", xpd = NA, lwd = 2),
                       Overall.p.arg = list(pch = 23, cex = 2, col = "black", bg = gray(.4), xpd = NA),
                       Overall.est.arg = list(x = smr.xlim[1] - .06, las = 1, font = 2, xpd = NA, adj = 1),
                       cat.seg.arg = list(col = "navyblue", xpd = NA, lwd = 2),
                       cat.p.arg = list(pch = 22 ,cex= 1, col = "black", bg = gray(.4), xpd = NA),
                       cat.est.arg = list(x = smr.xlim[1] - .06, las = 1, col = gray(.4), xpd = NA, adj = 1),
                       SMR.head.arg = list(smr.xlim[1] - .06, font = 2, labels = "SMR [95% CIs]", xpd = NA, adj = 1),
                       smr.xlab = "Standardized Mortality Ratio",
                       smr.xlim = "auto",
                       grid = TRUE,
                       digits = 3){
  opar <- par(mfrow = c(1,1), mar = c(5.1, 4.1, 4.1, 2.1))
  on.exit(par(opar))

  # Separando os valores da medida preincipal
  main.pos <- 1
  main.smr <- unlist(x[which(x$Variables == "Overall"), c("SMR", "lower.Cl", "upper.Cl")])
  main.n <- unlist(x[which(x$Variables == "Overall"), "N"])
  main.observed <- round(unlist(x[which(x$Variables == "Overall"), "Observed"]), digits = digits)
  main.expected <- round(unlist(x[which(x$Variables == "Overall"), "Expected"]), digits = digits)

  # Separando as medidas do SMR para montar o grafico
  smr.estimates <- unlist(x$SMR[-1])
  smr.ll <- unlist(x$lower.Cl[-1])
  smr.ul <- unlist(x$upper.Cl[-1])

  # Separando os valores para serem impressos na direita do grafico
  N <- unlist(x$N[-1])
  observed <- round(unlist(x$Observed[-1]), digits = digits)
  expected <- round(unlist(x$Expected[-1]), digits = digits)

  # Achandos os limites do grafico no eixo horizontal
  if (smr.xlim[1] == "auto"){
    smr.xlim <- c(min(unlist(c(main.smr[2], smr.ll)), na.rm = TRUE), max(unlist(c(main.smr[3], smr.ul)), na.rm = TRUE))
  }

  # Separando os valores das vari?veis e categorias
  var.labels <- x$Variables[-which(is.na(x$Variables))]
  cat.labels <- x$Levels[-which(is.na(x$Levels))]

  # Achando as posi??es no eixo vertical
  nLevels <- c(which(!is.na(x$Variables))[-1],(nrow(x) + 1))
  for(i in 1:length(nLevels)){
    nLevels[i] <- nLevels[i+1] - nLevels[i]
  }; rm(i)
  nLevels <- nLevels[which(!is.na(nLevels))]
  # x$Levels
  # i = 3
  cat.pos <- seq(1:nLevels[1])

  if (length(nLevels) == 1) {
    cat.pos <- cat.pos + 2
  } else {
    for(i in 2:length(nLevels)){
      ini <- cat.pos[length(cat.pos)]
      cat.pos <- c(cat.pos, (ini + 3):(ini + 2 + nLevels[i]))
    }
    cat.pos <- cat.pos + 2
  }
  if (length(nLevels) == 1) {
    var.pos <- cat.pos[length(cat.pos)] + 1
  } else {
    var.pos = nLevels[1] + 1
    # i = 2
    for(i in 2:length(nLevels)){
      var.pos <- c(var.pos, var.pos[i-1] + nLevels[i] + 2)
    }
    var.pos <- var.pos + 2
  }
  ylim = c(1, var.pos[length(var.pos)])
  # 5.1 4.1 4.1 2.1

  par(mfrow = c(1,2))

  # Janela com textos com variaveis e categorias
  par(mar = mar1)
  plot(NA, NA, xlim = c(0,1), ylim = ylim , xlab = "", ylab = "",yaxt = "n", xaxt = "n", xaxs = "i", bty = "n", xpd = NA)

  # Efeito principal
  overall.arg$y <- main.pos
  do.call(text, overall.arg)

  # N O E do overall
  NOE.overall.args$labels <- c(main.n, main.observed, main.expected)
  NOE.overall.args$y <- main.pos
  do.call(text, NOE.overall.args)

  # Demais vari?veis
  var.labels.arg$labels <- var.labels[2:length(var.labels)]
  var.labels.arg$y <- var.pos
  do.call(text, var.labels.arg)

  # Categorias
  cat.labels.arg$labels <- cat.labels
  cat.labels.arg$y <- cat.pos
  do.call(text, cat.labels.arg)

  # Colunas com os valores de N O e E
  N.values.arg$labels <- N
  N.values.arg$y <- cat.pos
  do.call(text, N.values.arg)

  O.values.arg$labels <- observed
  O.values.arg$y <- cat.pos
  do.call(text, O.values.arg)

  E.values.arg$labels <- expected
  E.values.arg$y <- cat.pos
  do.call(text, E.values.arg)

  # Cabecalho das colunas N O E
  NOE.head.arg$y <- ylim[2] + 1
  NOE.head.arg$x <- c(N.values.arg$x, O.values.arg$x, E.values.arg$x)
  do.call(text, NOE.head.arg)

  # SMR
  par(mar = mar.SMR)
  plot(NA, NA, xlim = smr.xlim, ylim = ylim , xlab = smr.xlab, ylab = "", yaxt = "n", xaxt = "n", xaxs = "i", bty = "n", xpd = NA); axis(1)
  if(grid) grid()

  # Segmento do IC Efeito principal
  Overall.seg.arg$x0 <- main.smr[2]
  Overall.seg.arg$y0 <- main.pos
  Overall.seg.arg$x1 <- main.smr[3]
  Overall.seg.arg$y1 <- main.pos
  do.call(segments, Overall.seg.arg)
  # Ponto do Efeito principal
  Overall.p.arg$x <- main.smr[1]
  Overall.p.arg$y <- main.pos
  Overall.p.arg$type <- "p"
  do.call(points, Overall.p.arg)
  # Estimativa do efeito principal
  Overall.est.arg$y <- main.pos
  Overall.est.arg$labels <- sprintf(paste0("%.",digits,"f [%.",digits,"f ; %.",digits,"f]"),main.smr[1],main.smr[2],main.smr[3])
  do.call(text, Overall.est.arg)

  # Segmentos das categorias
  cat.seg.arg$x0 <- smr.ll
  cat.seg.arg$y0 <- cat.pos
  cat.seg.arg$x1 <- smr.ul
  cat.seg.arg$y1 <- cat.pos
  do.call(segments, cat.seg.arg)
  # Ponto das estimativas das categorias
  cat.p.arg$x <- smr.estimates
  cat.p.arg$y <- cat.pos
  cat.p.arg$type <- "p"
  do.call(points, cat.p.arg)
  # Estimativas das categorias
  cat.est.arg$y <- cat.pos
  cat.est.arg$labels <- sprintf(paste0("%.",digits,"f [%.",digits,"f ; %.",digits,"f]"),smr.estimates,smr.ll,smr.ul)
  do.call(text, cat.est.arg)

  # Cabecalho dos SMRs
  SMR.head.arg$y <- ylim[2] + 1
  do.call(text, SMR.head.arg)
}

