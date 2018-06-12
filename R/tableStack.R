#' Tabulation of variables in a stack form
#'
#' There are two functionalities: Tabulation of variables with the same possible range of distribution and stack into a new table with or without other descriptive statistics or to breakdown distribution of more than one row variables against a column variable
#'
#' @param vars	a vector of variables in the data frame. The imput may be given with or without quotes.
#' @param dataFrame	source data frame of the variables
#' @param minlevel	possible minimum value of items specified by user
#' @param maxlevel	possible maximum value of items specified by user
#' @param count	whether number of valid records for each item will be displayed
#' @param na.rm	whether missing value would be removed during calculation mean score of each person
#' @param means	whether means of all selected items will be displayed
#' @param medians	whether medians of all selected items will be displayed
#' @param sds	whether standard deviations of all selected items will be displayed
#' @param decimal	number of decimals displayed
#' @param total	display of means and standard deviations of total and average scores
#' @param var.labels	presence of descriptions of variables on the last column of output
#' @param var.labels.trunc	number of characters used for variable description
#' @param reverse	whether item(s) negatively correlated with other majority will be reversed
#' @param vars.to.reverse	variable(s) to reverse
#' @param by	a variable for column breakdown. If NONE is given, only the 'total column' will be displayed. More on Details.
#' @param vars.to.factor	variable(s) to be converted to factor for tabulaton
#' @param iqr	variable(s) to display median and inter-quartile range
#' @param prevalence	for logical or dichotomous variables, whether prevalence of the dichotomous row variable in each column subgroup will be displayed
#' @param percent	type of percentage displayed when the variable is categorical and for NArow when activated. Default is column
#' @param frequency	whether to display frequency in the cells when the variable is categorical and for NArow when activated
#' @param test	whether statistical test(s) will be computed
#' @param name.test	display name of the test and relevant degrees of freedom
#' @param total.column	whether to add 'total column' to the output or not
#' @param simulate.p.value	simulate P value for Fisher's exact test
#' @param sample.size	whether to display non-missing sample size of each column
#' @param assumption.p.value	level of Bartlett's test P value to judge whether the comparison and the test should be parametric
#' @param NAcol whether to add 'NA column' to the output or not
#' @param NArow whether to add 'NA rows' for each variable to the output or not
#' @param drplvls whether to hide non used levels on factor and character variables or not
#'
#' @details  This function is a clone of \code{tableStack} from the \code{epiDisplay} package. Comparing to the original, tt adds options to show the NA in the variables as categories, similar to the option \code{useNA} in the \code{table} function, and it also fix few bugs, such as showing the \code{total.column} without the need to \code{test = TRUE}, and to show or hide levels with zero counts without returning error.
#'
#' This function simultaneously explores several variables with a fixed integer rating scale. For non-factor variables, the default values for tabulation are the minimum and the maximum of all variables but can be specified by the user.
#'
#'When 'by' is omitted, all variables must be of the same class, and must be 'integer', 'factor' or 'logical. Some parameters are only used if by is omitted, others are only used if by is available. The by-omitted dependents are minlevel, maxlevel, count, na.rm, means, medians, sds, total, reverse, vars.to.reverse. The by-available dependents are iqr, prevalence, percent, frequency, test, name.test, total.column, simulate.p.value, sample.size, assumption.p.value, NArow, NAcol, drplvls.
#'Unlike function 'alpha', the argument 'reverse' has a default value of FALSE. This argument is ignored if 'vars.to.reverse' is specified.
#'
#'Options for 'reverse', 'vars.to.reverse' and statistics of 'means', 'medians', 'sds' and 'total' are available only if the items are not factor. To obtain statistics of factor items, users need to use 'unclassDataframe' to convert them into integer.
#'
#'When the 'by' argument is given, 'reverse' and 'vars.to.reverse' do not apply, as mentioned before. Instead, columns of the 'by' variable will be formed. A table will be created against each selected variable. If the variable is a factor or coerced to factor with 'vars.to.factor', cross-tabulation will result with percents as specified, ie. "column", "row", or "none" (FALSE). For a dichotomous row variable, if set to 'TRUE', the prevalence of row variable in the form of a fraction is displayed in each subgroup column. For objects of class 'numeric' or 'integer', means with standard deviations will be displayed. For variables with residuals that are not normally distributed or where the variance of subgroups are significantly not normally distributed (using a significance level of 0.01), medians and inter-quartile ranges will be presented if the argument 'iqr' is set to "auto" (by default). Users may specify a subset of the selected variables (from the 'vars' argument) to be presented in such a form. Otherwise, the argument could be set as any other character string such as "none", to insist to present means and standard deviations.
#'
#'When 'test = TRUE' (default), Pearson's chi-squared test (or a two-sided Fisher's exact test, if the sample size is small) will be carried out for a categorical variable or a factor. Parametric or non-parametric comparison and test will be carried out for a object of class 'numeric' or 'integer' (See 'iqr' and 'assumption.p.value' below). If the sample size of the numeric variable is too small in any group, the test is omitted and the problem reported.
#'
#' For Fisher's exact test, the default method employs 'simulate.p.value = FALSE'. See further explanation in 'fisher.test' procedure. If the dataset is extraordinarily large, the option may be manually set to TRUE.
#'
#' When 'by' is specified as a single character object (such as 'by="none"') or when 'by = NONE' there will be no column breakdown and all tests will be omitted. Only the total column is displayed. Only the 'total' column is shown.
#'
#' If this 'total column' is to accompany the 'by' breakdown, the argument 'total.column=TRUE' should be specified. The 'sample.size' is TRUE by default. The total number of records for each group is displayed in the first row of the output. However, the variable in each row may have some missing records, the information on which is reported by NArow for each variable on 'vars' and by NAcol for the variable on 'by'.
#'
#' By default, Epicalc sets 'var.labels=TRUE' in order to give nice output. However, 'var.labels=FALSE' can sometimes be more useful during data exploration. Variable numbers as well as variable names are displayed instead of variable labels. Names and numbers of abnormally distributed variables, especially factors with too many levels, can be easily identified for further relevelling or recoding.
#'
#' The argument 'iqr' has a default value being "auto". Non-parametric comparison and test will be automatically chosen if Bartlett's test P value is below the 'assumption.p.value'.
#'
#' The test can be forced to parametric by setting 'iqr=NULL' and to non-parametric by if iqr is set to the variable number of cont.var (See examples.).
#'
#' @return  an object of class 'tableStack' and 'list' when by=NULL
#' \tabular{ll}{
#' \code{results} \tab an object of class 'noquote' which is used for print out\cr
#' \code{items.reversed}  \tab name(s) of variable(s) reversed\cr
#' \code{total.score} \tab a vector from 'rowSums' of the columns of variables specified in 'vars'\cr
#' \code{mean.score}  \tab a vector from 'rowMeans' of the columns of variables specified in 'vars'\cr
#' \code{mean.of.total.scores}  \tab mean of total scores\cr
#' \code{sd.of.total.scores}  \tab standard deviation of total scores\cr
#' \code{mean.of.average.scores}  \tab mean of mean scores\cr
#' \code{sd.of.average.scores}  \tab standard deviation of mean scores
#' }
#' When 'by' is specified, an object of class 'tableStack' and 'table is returned.
#' @author Virasakdi Chongsuvivatwong <cvirasak@medicine.psu.ac.th>
#' Caio Ferreira <caio.ferreira@epimedsolutions.com>
#' Lunna Borges <caio.ferreira@epimedsolutions.com>
#'Pedro Brasil <pedro.brasil@epimedsolutions.com>
#' @references 'table', 'tab1', 'summ', 'alpha', 'unclassDataframe'
#' @examples
#' \dontrun{
#' data(Oswego)
#' tableStack(bakedham:fruitsalad, dataFrame=Oswego)
#' .data <- Oswego
#' attach(.data)
#' tableStack(bakedham:fruitsalad, .data) # Default data frame is .data
#' tableStack(bakedham:fruitsalad, .data, by= ill)
#' tableStack(bakedham:fruitsalad, .data, by= ill, prevalence=TRUE)
#' tableStack(bakedham:fruitsalad, .data, by= ill, percent=FALSE)
#' tableStack(bakedham:fruitsalad, .data, by= ill, percent=FALSE, name.test=FALSE)
#' tableStack(bakedham:fruitsalad, .data, by= ill, NAcol = TRUE)
#' tableStack(bakedham:fruitsalad, .data, by= ill, NAcol = TRUE, NArow = TRUE)
#' detach(.data)
#'
#'
#' data(Cars93, package = "MASS")
#' .data <- Cars93
#' tableStack(vars=4:25, .data, by=Origin)
#' tableStack(vars=4:25, .data, by="none")
#' tableStack(vars=4:25, .data, by=Origin, total.column=TRUE)
#' tableStack(vars=4:25, .data, by = NONE)
#'
#'
#' data(Attitudes)
#' .data <- Attitudes
#' attach(.data)
#' tableStack(qa1:qa18, .data)  # May need full screen of Rconsole
#' tableStack(qa1:qa18, .data, var.labels.trunc=35)
#' # Fits in with default R console screen
#' tableStack(qa1:qa18, .data, reverse=TRUE) -> a
#' a
#' ## Components of 'a' have appropriate items reversed
#' a$mean.score -> mean.score
#' a$total.score -> total.score
#' .data$mean.score <- mean.score
#' .data$total.score <- total.score
#' rm(total.score, mean.score)
#' detach(.data)
#' attach(.data)
#' tableStack(c(qa1,qa13:qa18,mean.score,total.score), .data, by=sex, test=FALSE)
#' tableStack(c(qa15, qa17, mean.score:total.score), .data, by=sex, iqr=c(qa17,total.score))
#' tableStack(c(qa15, qa17, mean.score:total.score), .data, by=dep, iqr=c(qa17,total.score))
#' ## 'vars' can be mixture of different classes of variables
#' .data$highscore <- mean.score > 4
#' tableStack(mean.score:highscore, .data, by=sex, iqr=total.score)
#' detach(.data)
#' rm(list=ls())
#'
#' data(Ectopic)
#' .data <- Ectopic
#' tableStack(vars=3:4, .data, by=outc)
#' tableStack(vars=3:4, .data, by=outc, percent="none")
#' tableStack(vars=3:4, .data, by=outc, prevalence = TRUE)
#' tableStack(vars=3:4, .data, by=outc, name.test = FALSE)
#'
#' ## Variable in numeric or factor
#' data(Outbreak)
#' .data <- Outbreak
#' # Comparison of exposure to food items between the two gender
#' tableStack(vars=5:8, .data, by=sex) # as continuous varaibles
#' tableStack(vars=5:8, .data, by=sex, vars.to.factor = 5:8) # as factors
#'
#' ## Using drplvls
#' # a dataframe was created containing a factor with an unused level
#' data <- data.frame(
#'   sample(1:10,2000, replace = T),
#'   factor(sample(c("Car","Bus"), 2000, replace = T),
#'      levels = c("Car","Bus","Truck")),
#'   sample(c("US","CA"), 2000, replace = T))
#'  names(data) <- c("Age","Type","Origin")
#'
#'  # by using drplvls the row of the unused fator is hidden
#'  tableStack(vars = c(Age, Type), data, by = Origin, drplvls = TRUE)
#' }
#' @export

tableStack <- function (vars, dataFrame, minlevel = "auto", maxlevel = "auto", count = TRUE, na.rm = FALSE, means = TRUE, medians = FALSE, sds = TRUE, decimal = 2, total = TRUE, var.labels = TRUE, var.labels.trunc = 150, reverse = FALSE, vars.to.reverse = NULL, by = NULL, vars.to.factor = NULL, iqr = "auto", prevalence = FALSE, percent = c("column", "row", "none"), frequency = TRUE, test = TRUE, name.test = TRUE, total.column = FALSE, simulate.p.value = FALSE, sample.size = TRUE, assumption.p.value = 0.01, NAcol = FALSE, NArow = FALSE, drplvls = FALSE){
  if(percent != "column" && percent != "row" && percent != "none" && !is.logical(percent)){
    stop("'percent' must be 'column', 'row', 'none' or logical.")
  }
  if(!is.data.frame(dataFrame)){
    stop("'dataFrame' must be a data frame.")
  }
  if(!(assumption.p.value<1 && assumption.p.value>0)){
    stop("'assumption.p.value' must be lower than 1 and higher than 0.")
  }
  if(!is.numeric(var.labels.trunc)){
    stop("'var.labels.trunc' must be numeric.")
  }
  if(!is.numeric(decimal)){
    stop("'decimal' must be numeric.")
  }
  if(!is.logical(count)){
    stop("'count' must be logic.")
  }
  if(!is.logical(na.rm)){
    stop("'na.rm' must be logic.")
  }
  if(!is.logical(means)){
    stop("'means' must be logic.")
  }
  if(!is.logical(medians)){
    stop("'medians' must be logic.")
  }
  if(!is.logical(sds)){
    stop("'sds' must be logic.")
  }
  if(!is.logical(total)){
    stop("'total' must be logic.")
  }
  if(!is.logical(var.labels)){
    stop("'var.labels' must be logic.")
  }
  if(!is.logical(reverse)){
    stop("'reverse' must be logic.")
  }
  if(!is.logical(prevalence)){
    stop("'prevalence' must be logic.")
  }
  if(!is.logical(frequency)){
    stop("'frequency' must be logic.")
  }
  if(!is.logical(test)){
    stop("'test' must be logic.")
  }
  if(!is.logical(name.test)){
    stop("'name.test' must be logic.")
  }
  if(!is.logical(total.column)){
    stop("'total.column' must be logic.")
  }
  if(!is.logical(simulate.p.value)){
    stop("'simulate.p.value' must be logic.")
  }
  if(!is.logical(sample.size)){
    stop("'sample.size' must be logic.")
  }
  if(!is.logical(NAcol)){
    stop("'NAcol' must be logic.")
  }
  if(!is.logical(NArow)){
    stop("'NArow' must be logic.")
  }
  if(!is.logical(drplvls)){
    stop("'drplvls' must be logic.")
  }
  nl <- as.list(1:ncol(dataFrame))
  names(nl) <- names(dataFrame)
  selected <- eval(substitute(vars), nl, parent.frame())
  if (is.character(selected)){
    selected <- unname(unlist(nl[selected]))
  }
  nllen <- length(nl)
  nl[nllen+1] <- "NONE"
  names(nl)[nllen+1] <- "NONE"
  by.var <- eval(substitute(by), nl, parent.frame())
  if (is.character(by.var)){
    by.var1 <- by.var
    by.var <- unname(unlist(nl[by.var]))
    if (is.null(by.var)){
      by.var <- by.var1
    }
  }
  if (!is.null(by.var) && is.character(by.var) && NAcol){
    NAcol <- FALSE
    warning("NAcol counts the amount of NAs on the variable used on by. Since by is set to NONE, NAcol was set to FALSE.")
  }
  if (!is.null(by.var) && is.character(by.var)){
    total.column <- F
  }
  selected.iqr <- eval(substitute(iqr), nl, parent.frame())
  if (is.character(selected.iqr)){
    selected.iqr <- unname(unlist(nl[selected.iqr]))
  }
  if (is.numeric(by.var)) {
    by <- dataFrame[, by.var]
  }
  if (is.character(by.var)) {
    by1 <- as.factor(rep("Total", nrow(dataFrame)))
  }
  if (is.null(by.var)) {
    selected.class <- NULL
    for (i in selected) {
      selected.class <- c(selected.class, class(dataFrame[, i]))
      if (is.character(dataFrame[, i])){
        stop("Without 'by', selected variables can't be of charater class.")
      }
    }
    if (length(table(table(selected.class))) > 1)
      warning("Without 'by', classes of all selected variables should be the same.")
  }
  selected.to.factor <- eval(substitute(vars.to.factor), nl, parent.frame())
  if (is.character(selected.to.factor)){
    selected.to.factor <- unname(unlist(nl[selected.to.factor]))
  }
  if (!is.character(selected.iqr)) {
    intersect.selected <- intersect(selected.iqr, selected.to.factor)
    if (length(intersect.selected) != 0) {
      stop(paste(names(dataFrame)[intersect.selected], "cannot simultaneously describe IQR and be coerced factor"))
    }
    for (i in selected.iqr) {
      if (!is.integer(dataFrame[, i]) & !is.numeric(dataFrame[, i])) {
        stop(paste(names(dataFrame)[i], "is neither integer nor numeric, not possible to compute IQR"))
      }
    }
  }
  for (i in selected) {
    if ((class(dataFrame[, i]) == "integer" || class(dataFrame[,   i]) == "numeric") && !is.null(by.var)) {
      if (any(selected.to.factor == i)) {
        dataFrame[, i] <- factor(dataFrame[, i])
      }
      else {
        dataFrame[, i] <- as.numeric(dataFrame[, i])
      }
    }
  }
  if ((reverse || suppressWarnings(!is.null(vars.to.reverse))) &&
      is.factor(dataFrame[, selected][, 1])) {
    stop("Variables must be in 'integer' class before reversing. \n        Try 'unclassDataframe' first'")
  }
  selected.dataFrame <- dataFrame[, selected, drop = FALSE]
  if (is.null(by.var)) {
    selected.matrix <- NULL
    for (i in selected) {
      selected.matrix <- cbind(selected.matrix, unclass(dataFrame[,i]))
    }
    colnames(selected.matrix) <- names(selected.dataFrame)
    if (minlevel == "auto") {
      minlevel <- min(selected.matrix, na.rm = TRUE)
    }
    if (maxlevel == "auto") {
      maxlevel <- max(selected.matrix, na.rm = TRUE)
    }
    nlevel <- as.list(minlevel:maxlevel)
    names(nlevel) <- eval(substitute(minlevel:maxlevel), nlevel, parent.frame())

    if (suppressWarnings(!is.null(vars.to.reverse))) {
      nl1 <- as.list(1:ncol(dataFrame))
      names(nl1) <- names(dataFrame[, selected])
      which.neg <- eval(substitute(vars.to.reverse), nl1, parent.frame())
      if (is.character(which.neg)){
        which.neg <- unname(unlist(nl1[which.neg]))
      }
      for (i in which.neg) {
        dataFrame[, selected][, i] <- maxlevel + 1 - dataFrame[, selected][, i]
        selected.matrix[, i] <- maxlevel + 1 - selected.matrix[, i]
      }
      reverse <- FALSE
      sign1 <- rep(1, ncol(selected.matrix))
      sign1[which.neg] <- -1
    }
    if (reverse) {
      matR1 <- cor(selected.matrix, use = "pairwise.complete.obs")
      diag(matR1) <- 0
      if (any(matR1 > 0.98)) {
        reverse <- FALSE
        temp.mat <- which(matR1 > 0.98, arr.ind = TRUE)
        warning(paste(paste(rownames(temp.mat), collapse = " and ")), " are extremely correlated.", "\n", "  The command has been excuted without 'reverse'.", "\n", "  Remove one of them from 'vars' if 'reverse' is required.")
      }
      else {
        score <- factanal(na.omit(selected.matrix), factors = 1, scores = "regression")$score
        sign1 <- NULL
        for (i in 1:length(selected)) {
          sign1 <- c(sign1, sign(cor(score, na.omit(selected.matrix)[, i], use = "pairwise")))
        }
        which.neg <- which(sign1 < 0)
        for (i in which.neg) {
          dataFrame[, selected][, i] <- maxlevel + minlevel - dataFrame[, selected][, i]
          selected.matrix[, i] <- maxlevel + minlevel - selected.matrix[, i]
        }
      }
    }
    table1 <- NULL
    for (i in as.integer(selected)) {
      if (!is.factor(dataFrame[, i]) & !is.logical(dataFrame[, i, drop = TRUE])) {
        x <- factor(dataFrame[, i])
        levels(x) <- nlevel
        tablei <- table(x)
      }
      else {
        if (is.logical(dataFrame[, i, drop = TRUE])) {
          tablei <- table(factor(dataFrame[, i, drop = TRUE], levels = c("FALSE", "TRUE")))
        }
        else {
          tablei <- table(dataFrame[, i])
        }
      }
      if (count) {
        tablei <- c(tablei, length(na.omit(dataFrame[, i])))
        names(tablei)[length(tablei)] <- "count"
      }
      if (is.numeric(selected.dataFrame[, 1, drop = TRUE]) | is.logical(selected.dataFrame[, 1, drop = TRUE])) {
        if (means) {
          tablei <- c(tablei, round(mean(as.numeric(dataFrame[, i]), na.rm = TRUE), digits = decimal))
          names(tablei)[length(tablei)] <- "mean"
        }
        if (medians) {
          tablei <- c(tablei, round(median(as.numeric(dataFrame[, i]), na.rm = TRUE), digits = decimal))
          names(tablei)[length(tablei)] <- "median"
        }
        if (sds) {
          tablei <- c(tablei, round(sd(as.numeric(dataFrame[, i]), na.rm = TRUE), digits = decimal))
          names(tablei)[length(tablei)] <- "sd"
        }
      }
      table1 <- rbind(table1, tablei)
    }
    results <- as.table(table1)
    if (var.labels) {
      rownames(results) <- names(selected.dataFrame)
    }
    else {
      rownames(results) <- paste(selected, ":", names(selected.dataFrame))
    }
    if (is.integer(selected.dataFrame[, 1])) {
      rownames(results) <- names(nl)[selected]
      if (is.factor(dataFrame[, selected][, 1])) {
        colnames(results)[1:(ncol(results) - (count + means + medians + sds))] <- levels(dataFrame[,  selected][, 1])
      }
      else {
        colnames(results)[1:(ncol(results) - (count + means + medians + sds))] <- names(nlevel)
      }
    }
    result0 <- results
    if (var.labels) {
      if (!is.null(attributes(dataFrame)$var.labels)) {
        results <- as.table(cbind(results, substr(attributes(dataFrame)$var.labels[selected], 1, var.labels.trunc)))
      }
      if (!is.null(attributes(dataFrame)$var.labels))
        colnames(results)[ncol(results)] <- "description"
    }
    if (is.integer(selected.dataFrame[, 1]) | is.numeric(selected.dataFrame[, 1]) | is.logical(selected.dataFrame[, 1])) {
      if (reverse || (!is.null(vars.to.reverse))) {
        Reversed <- ifelse(sign1 < 0, "    x   ", "    .   ")
        results <- cbind(Reversed, results)
      }
      sumMeans <- 0
      sumN <- 0
      for (i in selected) {
        sumMeans <- sumMeans + mean(as.numeric(dataFrame[, i]), na.rm = TRUE) * length(na.omit(dataFrame[, i]))
        sumN <- sumN + length(na.omit(dataFrame[, i]))
      }
      mean.of.total.scores <- weighted.mean(rowSums(selected.matrix), w = rowSums(!is.na(selected.matrix)), na.rm = TRUE)
      sd.of.total.scores <- sd(rowSums(selected.matrix), na.rm = TRUE)
      mean.of.average.scores <- weighted.mean(rowMeans(selected.matrix), w = rowSums(!is.na(selected.matrix)), na.rm = TRUE)
      sd.of.average.scores <- sd(rowMeans(selected.matrix), na.rm = TRUE)
      countCol <- which(colnames(results) == "count")
      meanCol <- which(colnames(results) == "mean")
      sdCol <- which(colnames(results) == "sd")
      if (total) {
        results <- rbind(results, rep("", reverse || suppressWarnings(!is.null(vars.to.reverse)) + (maxlevel + 1 - minlevel) + (count + means + medians + sds + var.labels)))
        results[nrow(results), countCol] <- length((rowSums(selected.dataFrame))[!is.na(rowSums(selected.dataFrame))])
        results[nrow(results), meanCol] <- round(mean.of.total.scores, digits = decimal)
        results[nrow(results), sdCol] <- round(sd.of.total.scores, digits = decimal)
        rownames(results)[nrow(results)] <- " Total score"
        results <- rbind(results, rep("", reverse || suppressWarnings(!is.null(vars.to.reverse)) + (maxlevel + 1 - minlevel) + (count + means + medians + sds + var.labels)))
        results[nrow(results), countCol] <- length(rowSums(selected.dataFrame)[!is.na(rowSums(selected.dataFrame))])
        results[nrow(results), meanCol] <- round(mean.of.average.scores, digits = decimal)
        results[nrow(results), sdCol] <- round(sd.of.average.scores, digits = decimal)
        rownames(results)[nrow(results)] <- " Average score"
      }
    }
    results <- list(results = noquote(results))
    if (reverse || suppressWarnings(!is.null(vars.to.reverse)))
      results <- c(results, list(items.reversed = names(selected.dataFrame)[sign1 < 0]))
    if (var.labels && !is.null(attributes(dataFrame)$var.labels)) {
      results <- c(results, list(item.labels = attributes(dataFrame)$var.labels[selected]))
    }
    if (total) {
      if (is.integer(selected.dataFrame[, 1]) | is.numeric(selected.dataFrame[, 1])) {
        results <- c(results,
                     list(total.score = rowSums(selected.matrix)),
                     list(mean.score = rowMeans(selected.matrix, na.rm = na.rm)),
                     list(mean.of.total.scores = mean.of.total.scores, sd.of.total.scores = sd.of.total.scores, mean.of.average.scores = mean.of.average.scores,  sd.of.average.scores = sd.of.average.scores))
      }
    }
    class(results) <- c("tableStack", "list")
    results
  }
   else {
    if (is.character(by.var)) {
      by1 <- as.factor(rep("Total", nrow(dataFrame)))
    }
    else {
      by1 <- factor(dataFrame[, by.var])
    }
    if (is.logical(dataFrame[, i])) {
      dataFrame[, i] <- as.factor(dataFrame[, i])
      levels(dataFrame[, i]) <- c("No", "Yes")
    }
    if (length(table(by1)) == 1)
      test <- FALSE
    name.test <- ifelse(test, name.test, FALSE)
    if (is.character(selected.iqr)) {
      if (selected.iqr == "auto") {
        selected.iqr <- NULL
        for (i in 1:length(selected)) {
          if (class(dataFrame[, selected[i]]) == "difftime") {
            dataFrame[, selected[i]] <- as.numeric(dataFrame[, selected[i]])
          }
          if (is.integer(dataFrame[, selected[i]]) |
              is.numeric(dataFrame[, selected[i]])) {
            if (length(table(by1)) > 1) {
              if (nrow(dataFrame) < 5000) {
                if (nrow(dataFrame) < 3) {
                  selected.iqr <- c(selected.iqr, selected[i])
                }
                else if (shapiro.test(lm(dataFrame[,  selected[i]] ~ by1)$residuals)$p.value < assumption.p.value | bartlett.test(dataFrame[, selected[i]] ~ by1)$p.value < assumption.p.value) {
                  selected.iqr <- c(selected.iqr, selected[i])
                }
              }
              else {
                sampled.shapiro <- sample(lm(dataFrame[, selected[i]] ~ by1)$residuals, 250)
                if (shapiro.test(sampled.shapiro)$p.value <
                    assumption.p.value | bartlett.test(dataFrame[, selected[i]] ~ by1)$p.value < assumption.p.value) {
                  selected.iqr <- c(selected.iqr, selected[i])
                }
              }
            }
          }
        }
      }
      else {
        selected.iqr <- NULL
      }
    }
    table2 <- NULL
    if (sample.size) {
      if (test) {
        if (name.test) {
          if (total.column) {
            if (NAcol){
              table2 <- rbind(c(table(by1), sum(is.na(by1)), length(by1), "", ""), c(rep("", length(table(by1)) + 1), "", "", ""))
              colnames(table2)[ncol(table2) - (3:0)] <- c("NA","Total", "Test stat.", "P.Value")
            }
            else{
            table2 <- rbind(c(table(by1), sum(!is.na(by1)), "", ""), c(rep("", length(table(by1)) + 1), "", ""))
            colnames(table2)[ncol(table2) - (2:0)] <- c("Total", "Test stat.", "P.Value")
            }
          }
          else {
            if (NAcol){
              table2 <- rbind(c(table(by1), sum(is.na(by1)), "", ""), c(rep("", length(table(by1))), "", "", ""))
              colnames(table2)[ncol(table2) - (2:0)] <- c("NA", "Test stat.", "P.Value")
            }
            else{
            table2 <- rbind(c(table(by1), "", ""), c(rep("", length(table(by1))), "", ""))
            colnames(table2)[ncol(table2) - (1:0)] <- c("Test stat.", "P.Value")
            }
          }
        }
        else {
          if (total.column) {
            if(NAcol){
              table2 <- rbind(c(table(by1), sum(is.na(by1)), length(by1), ""), c(rep("", length(table(by1)) + 1), "", ""))
              colnames(table2)[ncol(table2) - (2:0)] <- c("NA", "Total", "P.Value")
            }
            else{
            table2 <- rbind(c(table(by1), sum(!is.na(by1)), ""), c(rep("", length(table(by1)) + 1), ""))
            colnames(table2)[ncol(table2) - (1:0)] <- c("Total", "P.Value")
            }
          }
          else {
            if(NAcol){
              table2 <- rbind(c(table(by1),sum(is.na(by1)), ""), c(rep("", length(table(by1))), "", ""))
              colnames(table2)[ncol(table2) - (1:0)] <- c("NA", "P.Value")
            }
            else{
            table2 <- rbind(c(table(by1), ""), c(rep("", length(table(by1))), ""))
            colnames(table2)[ncol(table2)] <- "P.Value"
            }
          }
        }
      }
      else {
        if (total.column && NAcol) {
            table2 <- rbind(c(table(by1), sum(is.na(by1)), length(by1)), c(rep("", length(table(by1)) + 1), ""))
            colnames(table2)[ncol(table2) - (1:0)] <- c("NA", "Total")
          }
          else if (total.column && NAcol == F){
          table2 <- rbind(c(table(by1), sum(!is.na(by1))), c(rep("", length(table(by1)) + 1)))
          colnames(table2)[ncol(table2)] <- c("Total")
          }
        else if (total.column == F && NAcol){
          table2 <- rbind(c(table(by1), sum(is.na(by1))), c(rep("", length(table(by1)) + 1)))
          colnames(table2)[ncol(table2)] <- c("NA")
        }
        else {
          table2 <- rbind(table(by1), "")
        }
      }
    }
    for (i in 1:length(selected)) {
      if (is.factor(dataFrame[, selected[i]]) | is.logical(dataFrame[, selected[i]]) | is.character(dataFrame[, selected[i]])) {
          if (drplvls && is.factor(dataFrame[, selected[i]])){
            drp <- dataFrame[,selected[i]]
            drp <- droplevels(drp)
            x0 <- table(drp, by1)
          }
          else {
            x0 <- table(dataFrame[, selected[i]], by1)
          }
          if (NArow || NAcol){
            if (drplvls && is.factor(dataFrame[, selected[i]])){
              natab <- table(drp, by1, useNA = "always" )
            }
            else{
            natab <- table(dataFrame[, selected[i]], by1, useNA = "always" )
            }
          }
          if (NArow && NAcol){
            a <- 0
          }
          else if (!NArow && NAcol){
            a <- 1
          }
          if (NArow){
            x1 <- x0
            x0 <- rbind(x0,natab[nrow(natab),1:ncol(natab)-1])
            rownames(x0)[nrow(x0)] <- "NA"
          }
          x2 <- x0
          if (NAcol && total.column){
            x2 <- cbind(x0,natab[1:(nrow(natab)-a),ncol(natab)])
            colnames(x2)[ncol(x2)] <- "NA"
            x <- addmargins(x2, margin = 2)
          }
        else if (!NAcol && total.column) {
          x <- addmargins(x0, margin = 2)
        }
          else if (NAcol && !total.column){
            x <- cbind(x0,natab[1:(nrow(natab)-a),ncol(natab)])
            colnames(x)[ncol(x)] <- "NA"
            x2 <- x
          }
        else {
          x <- x0
        }
        sr <- rowSums(x0)
        sc <- colSums(x0)
        table0 <- x
        x <- unclass(x)
        x.row.percent <- formatC(round(x/rowSums(x0) * 100, decimal), format = 'f', digits = decimal)
        if ((nrow(x) == 2 & prevalence & !NArow)||(nrow(x) == 3 & prevalence & NArow)) {
          if (NArow){
            x <- x[1:2,]
          }
          table00 <- addmargins(x, margin = 1)
          table0 <- paste(table00[2, ], "/", table00[3, ], " (", formatC(round(table00[2, ]/table00[3, ] * 100, decimal), format = 'f', digits = decimal), "%)", sep = "")
          table0 <- t(table0)
          rownames(table0) <- "  prevalence"
        }
        else {
          if (percent[1] == "column") {
            x.col.percent <- formatC(round(t(t(x)/colSums(x)) * 100, decimal), format = 'f', digits = decimal)
            x.col.percent1 <- matrix(paste(x, " (", x.col.percent, ")", sep = ""), nrow(x),  ncol(x))
            if (!frequency) {
              x.col.percent1 <- x.col.percent
            }
            table0 <- x.col.percent1
          }
          else {
            if (percent[1] == "row") {
              x.row.percent <- formatC(round(x/rowSums(x2) * 100, decimal), format = 'f', digits = decimal)
              x.row.percent1 <- matrix(paste(x, " (", x.row.percent, ")", sep = ""), nrow(x), ncol(x))
              if (!frequency) {
                x.row.percent1 <- x.row.percent
              }
              table0 <- x.row.percent1
            }
          }
          rownames(table0) <- paste("  ", rownames(x))
            colnames(table0) <- colnames(x)
        }
        if (NArow){
          x0 <- x1
        }
        if (test) {
          E <- outer(sr, sc, "*")/sum(x0)
          dim(E) <- NULL
          if ((sum(E < 5))/length(E) > 0.2 & nrow(dataFrame) < 1000) {
            test.method <- "Fisher's exact test"
            p.value <- fisher.test(x0, simulate.p.value = simulate.p.value)$p.value
          }
          else {
            test.method <- paste("Chisq. (", suppressWarnings(chisq.test(x0)$parameter), " df) = ", suppressWarnings(round(chisq.test(x0)$statistic, decimal + 1)), sep = "")
            p.value <- suppressWarnings(chisq.test(x0)$p.value)
          }
        }
        }
      if (is.numeric(dataFrame[, selected[i]])) {
        if (any(selected.iqr == selected[i])) {
          term1 <- NULL
          term2 <- NULL
          term3 <- NULL
          for (j in 1:(length(levels(by1)))) {
            term1 <- c(term1, quantile(dataFrame[by1 == levels(by1)[j], selected[i]], na.rm = TRUE)[3])
            term2 <- c(term2, quantile(dataFrame[by1 == levels(by1)[j], selected[i]], na.rm = TRUE)[2])
            term3 <- c(term3, quantile(dataFrame[by1 == levels(by1)[j], selected[i]], na.rm = TRUE)[4])
          }
          if (NAcol){
            term1 <- c(term1, quantile(dataFrame[is.na(by1), selected[i]], na.rm = TRUE)[3])
            term2 <- c(term2, quantile(dataFrame[is.na(by1), selected[i]], na.rm = TRUE)[2])
            term3 <- c(term3, quantile(dataFrame[is.na(by1), selected[i]], na.rm = TRUE)[4])
          }
          if (total.column) {
            if (NAcol){
            term1 <- c(term1, quantile(dataFrame[, selected[i]], na.rm = TRUE)[3])
            term2 <- c(term2, quantile(dataFrame[, selected[i]], na.rm = TRUE)[2])
            term3 <- c(term3, quantile(dataFrame[, selected[i]], na.rm = TRUE)[4])
            }
            else {
              term1 <- c(term1, quantile(dataFrame[!is.na(by1), selected[i]], na.rm = TRUE)[3])
              term2 <- c(term2, quantile(dataFrame[!is.na(by1), selected[i]], na.rm = TRUE)[2])
              term3 <- c(term3, quantile(dataFrame[!is.na(by1), selected[i]], na.rm = TRUE)[4])
            }
          }
          term.numeric <- paste(formatC(round(term1, decimal), digits = decimal, format = 'f'), " (", formatC(round(term2, decimal), digits = decimal, format = 'f'), " - ", formatC(round(term3, decimal), digits = decimal, format = 'f'), ")", sep = "")
          term.numeric <- gsub(" NA","NA",term.numeric)
          term.numeric <- t(term.numeric)
          rownames(term.numeric) <- "  median (IQR)"
        }
        else {
          b <- 0
          term1 <- as.vector(tapply(X = dataFrame[, selected[i]], INDEX = list(by1), FUN = "mean", na.rm = TRUE))
          term2 <- as.vector(tapply(X = dataFrame[, selected[i]], INDEX = list(by1), FUN = "sd", na.rm = TRUE))
          if (total.column) {
            b <- 1
            if (NAcol){
            term1 <- c(term1, mean(dataFrame[, selected[i]], na.rm = TRUE))
            term2 <- c(term2, sd(dataFrame[, selected[i]], na.rm = TRUE))
            }
            else {
              term1 <- c(term1, mean(dataFrame[!is.na(by1), selected[i]], na.rm = TRUE))
              term2 <- c(term2, sd(dataFrame[!is.na(by1), selected[i]], na.rm = TRUE))
            }
          }
          term.numeric <- paste(formatC(round(term1, decimal), digits = decimal, format = 'f'), " (", formatC(round(term2, decimal), digits = decimal, format = 'f'), ")", sep = "")
          if(NAcol){
            term.numeric <- append(term.numeric,term.numeric[length(term.numeric)])
            term.numeric[length(term.numeric)-b] <- paste(formatC(round(mean(dataFrame[is.na(by1), selected[i]], na.rm = TRUE),decimal),digits = decimal, format = 'f')," (", formatC(round(sd(dataFrame[is.na(by1), selected[i]], na.rm = TRUE),decimal), digits = decimal, format = 'f'),")",sep = "")
          }
          term.numeric <- t(term.numeric)
          rownames(term.numeric) <- "  mean (SD)"
        }
        if (NArow){
          narow <- NULL
          sumnarow <- 0
          sumnapercent <- 0
          napercentnacol <- 0
          naxna <- 0
          for (j in 1:length(levels(by1))){
            natot <- sum(is.na(dataFrame[which(by1 == levels(by1)[j]),selected[i]]))
            sumnarow <- sumnarow + natot
            if (percent[1] == "column"){
              napercent <- formatC(round(natot / nrow(dataFrame[which(by1 == levels(by1)[j]),]) * 100, decimal), format = 'f', digits = decimal)
              navalue <- paste(natot," (",napercent, ")", sep ="")
              if (!frequency) {
                navalue <- napercent
              }
            }
            else if (percent[1] == "row"){
              if (NAcol){
                naxna <- sum(is.na(dataFrame[is.na(by1),selected[i]]))
              }
              napercent <- formatC(round(natot / (sum(is.na(dataFrame[!is.na(by1),selected[i]]))+naxna) * 100, decimal), format = 'f', digits = decimal)
              navalue <- paste(natot," (",napercent, ")", sep ="")
              sumnapercent <- sumnapercent + as.numeric(napercent)
              if (!frequency) {
                navalue <- napercent
              }
            }
            else{
              navalue <- formatC(natot, format = 'f', digits = decimal)
            }
            narow <- c(narow,navalue)
          }
          if (NAcol){
            natotnacol <- sum(is.na(dataFrame[is.na(by1),selected[i]]))
            sumnarow <- sumnarow + natotnacol
            if (percent[1] == "column"){
              napercentnacol <- formatC(round(natotnacol / length(dataFrame[is.na(by1),selected[i]]) * 100, decimal), format = 'f', digits = decimal)
              navaluenacol <- paste(natotnacol," (",napercentnacol, ")", sep ="")
              if (!frequency) {
                navaluenacol <- napercentnacol
              }
            }
            else if (percent[1] == "row"){
              napercentnacol <- formatC(round(natotnacol / sum(is.na(dataFrame[,selected[i]])) * 100, decimal), format = 'f', digits = decimal)
              navaluenacol <- paste(natotnacol," (",napercentnacol, ")", sep ="")
              if (!frequency) {
                navaluenacol <- napercentnacol
              }
            }
            else{
              navaluenacol <- formatC(natotnacol, format = 'f', digits = decimal)
            }
            narow <- c(narow,navaluenacol)
          }
          if (total.column){
            if (percent[1] == "column"){
              napercenttot <- formatC(round(sumnarow / nrow(dataFrame) * 100, decimal), format = 'f', digits = decimal)
              navaluetot <- paste(sumnarow," (",napercenttot, ")", sep ="")
              if (!frequency) {
                navaluetot <- napercenttot
              }
            }
            else if (percent[1] == "row"){
              napercenttot <- formatC((sumnapercent+as.numeric(napercentnacol)), format = 'f', digits = decimal)
              navaluetot <- paste(sumnarow," (",napercenttot, ")", sep ="")
              if (!frequency) {
                navaluetot <- napercenttot
              }
            }
            else{
              navaluetot <- formatC(sumnarow, format = 'f', digits = decimal)
            }
            narow <- c(narow,navaluetot)
          }
          term.numeric <- rbind(term.numeric,narow)
          rownames(term.numeric)[2] <- "  NA"
        }
        table0 <- term.numeric
        if (test) {
          if (any(as.integer(table(by1[!is.na(dataFrame[, selected[i]])])) < 3) | length(table(by1)) > length(table(by1[!is.na(dataFrame[, selected[i]])]))) {
            test.method <- paste("Sample too small: group", paste(which(as.integer(table(factor(by)[!is.na(dataFrame[, selected[i]])])) < 3), collapse = " "))
            p.value <- NA
          }
          else {
            if (any(selected.iqr == selected[i])) {
              if (length(levels(by1)) > 2) {
                test.method <- "Kruskal-Wallis test"
                p.value <- kruskal.test(dataFrame[, selected[i]] ~ by1)$p.value
              }
              else {
                test.method <- "Ranksum test"
                p.value <- wilcox.test(dataFrame[, selected[i]] ~ by1, exact = FALSE)$p.value
              }
            }
            else {
              if (length(levels(by1)) > 2) {
                test.method <- paste("ANOVA F-test (", anova(lm(dataFrame[, selected[i]] ~ by1))[1, 1], ", ", anova(lm(dataFrame[, selected[i]] ~ by1))[2, 1], " df) = ", round(anova(lm(dataFrame[, selected[i]] ~ by1))[1, 4], decimal + 1), sep = "")
                p.value <- anova(lm(dataFrame[, selected[i]] ~ by1))[1, 5]
              }
              else {
                test.method <- paste("t-test", paste(" (", t.test(dataFrame[, selected[i]] ~  by1, var.equal = TRUE)$parameter, " df)", sep = ""), "=", round(abs(t.test(dataFrame[, selected[i]] ~ by1, var.equal = TRUE)$statistic), decimal + 1))
                p.value <- t.test(dataFrame[, selected[i]] ~ by1, var.equal = TRUE)$p.value
              }
            }
          }
        }
      }
      if (test) {
        if (name.test) {
          label.row <- c(rep("", length(levels(by1)) + total.column + NAcol), test.method, ifelse(p.value < 0.001, "< 0.001", round(p.value, decimal + 2)))
          label.row <- t(label.row)
                    if (total.column) {
                      if(NAcol){
                        colnames(label.row) <- c(levels(by1), "NA", "Total",
                                                 "Test stat.", "P value")
                      }
                      else{
          colnames(label.row) <- c(levels(by1), "Total",
                                   "Test stat.", "P value")
                      }
        }
          else {
            if(NAcol){
              colnames(label.row) <- c(levels(by1), "NA", "Test stat.", "P value")
            }
            else{
            colnames(label.row) <- c(levels(by1), "Test stat.", "P value")
            }
          }
          table0 <- cbind(table0, "", "")
          blank.row <- rep("", length(levels(by1)) + total.column + NAcol + 2)
        }
        else {
          label.row <- c(rep("", length(levels(by1)) +
                               total.column + NAcol), ifelse(p.value < 0.001, "< 0.001",
                                                     round(p.value, decimal + 2)))
          label.row <- t(label.row)
          if (total.column && NAcol){
            colnames(label.row) <- c(levels(by1), "NA", "Total", "P value")
          }
          else if (total.column && NAcol==F){
            colnames(label.row) <- c(levels(by1), "Total", "P value")
          }
          else if (total.column==F && NAcol){
            colnames(label.row) <- c(levels(by1), "NA", "P value")
          }
          else {
            colnames(label.row) <- c(levels(by1), "P value")
          }
          table0 <- cbind(table0, "")
          blank.row <- rep("", length(levels(by1)) +
                             total.column + NAcol + 1)
        }
      }
      else {
        label.row <- c(rep("", length(levels(by1)) +
                             total.column + NAcol))
        label.row <- t(label.row)
        if (total.column && NAcol==F) {
          colnames(label.row) <- c(levels(by1), "Total")
        }
        else if (total.column && NAcol){
          colnames(label.row) <- c(levels(by1), "NA", "Total")
        }
        else if (total.column == F && NAcol) {
          colnames(label.row) <- c(levels(by1), "NA")
        }
        else {
          colnames(label.row) <- c(levels(by1))
        }
        blank.row <- rep("", length(levels(by1)) + total.column + NAcol)
      }
      if (var.labels) {
        rownames(label.row) <- ifelse(!is.null(attributes(dataFrame)$var.labels[selected][i]), attributes(dataFrame)$var.labels[selected[i]], names(dataFrame)[selected][i])
        rownames(label.row) <- ifelse(rownames(label.row) == "", names(dataFrame[selected[i]]), rownames(label.row))
      }
      else {
        rownames(label.row) <- paste(selected[i], ":", names(dataFrame[selected[i]]))
      }
      if (!is.logical(dataFrame[, selected[i]])) {
        if (prevalence & length(levels(dataFrame[, selected[i]])) ==
            2) {
          rownames(label.row) <- paste(rownames(label.row), "=", levels(dataFrame[, selected[i]])[2])
        }
      }
      blank.row <- t(blank.row)
      rownames(blank.row) <- ""
      table2 <- rbind(table2, label.row, table0, blank.row)
    }
    if (sample.size) {
      rownames(table2)[1:2] <- c("Total", "")
    }
    class(table2) <- c("tableStack", "table")
    table2
  }
}
