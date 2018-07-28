#' Miscellaneous functions for data editing
#'
#' @name miscellaneous
#'
#' @description Collection of functions for data editing, usually used as lower levels for other functions.
#'
#' \code{f.num} is a wrapper to format numeric variables that are stored as character or factor, simultaneously it will try to detect comma spearated and replace it by dots before formating the variable as numeric. Any non-numeric encoding will be coerced to NA.
#'
#'\code{f.date} is a wrapper either to \code{\link[base]{as.Date}} or \code{\link[base]{strptime}} to format character or factor variables into dates. In Epimed Solutions database there are a few pre-specified formats that \code{f.date} will try to detect and return a formated date. \code{f.date} will try to dected if more than half of the elements in a vector have a pre-specified format. If so, the remaining will be coerced to NA if they have different format from the detected. See example.
#'
#' \code{remove.na} identifies all the empty spaces, i.e. the " " cells, of character or factor variables in a data.frame and returns the same data.frame with these empty cells replaced, by default, by NAs. It does not matter the length of the empty spaces. Also, \code{remove.na} trims the leading and trailing empty spaces from all character and factor variables. It does not format the numeric variables. It may also return at the console a few information about the " " fields.
#'
#' \code{tab2tex} removes the empty rows, and also tunrs the rownames of a table epiDisplay::tableStack into the first column, to make it easier to paste the table into a rtf or latex document without empty rows or rownames conflicts.
#'
#' \code{trunc_num} truncates a numeric vector by replacing the values below the min value or above the max values by the min and max values respectively or optionall to NA. See example.
#'
#' \code{dummy.columns} takes a \code{data.frame} with one column with concatatenated levels of a factor (or character) variable and return a \code{data.frame} with additional columns with zeros and ones (dummy values), which names are the factor levels of the original column. See example below. \code{rm.dummy.columns} is an internal function of \code{dummy.columns} that deletes the new dummy columns which have less then a specified minimum events.
#'
#' @param num.var A character, or factor variable to be formated as numeric.
#'
#' @param date A character or factor variable to be formated as date.
#'
#' @param data A \code{data.frame}.
#'
#' @param replace By default, NA. But could be any vector of length 1.
#'
#' @param console.output Logical. Print at the console a few informations about the " " fields?
#'
#' @param x,nc For \code{tab2tex} x is a object from epiDisplay::tableStack. nc is the number of the last column to keep in the table. If the table has 5 columns and nc = 3, then columns 4 and 5 are removed. For \code{trunc_num}, x is a numeric vector.
#'
#' @param min,max For \code{trunc_num}, min and max are the minimal and maximal numeric values where the numeric vector will be truncated.
#'
#' @param toNA For \code{trunc_num}, if FALSE any min and max are the minimal and maximal numeric values where the numeric vector will be truncated.
#'
#' @param original.column A character vector representing the name of the column the be transformed in dummy variables.
#'
#' @param factors A character vector to make new dummy columns and to match values in \code{original.column}. This is interesting if the user desires to make dummy only from a few factors in the originlal column. Ignored if scan.oc = TRUE
#'
#' @param scan.oc Default = FALSE, if TRUE, \code{dummy.columns} scans the specified \code{original.column} and uses all factors to generate dummy variables. It overrides the \code{factor} argument.
#'
#' @param sep A character of legth one that systematically split the factors in the original columns. It wil be passed to the \code{sep} argument in the \code{\link[base]{scan}} function.
#'
#' @param colnames.add The default is '= "Dummy_"'. This is a character vector of length one to stick in the \code{colnames} of the dummy variables. For example, if the orginal column has A;B;C factor levels, the new dummy variables \code{colnames} would be "Dummy_A", "Dummy_B", and "Dummy_C"
#'
#' @param event A character string to be detected as a event. In \code{rm.dummy.columns}, if the columns are coded as '0' and '1', the event is '1', if it is coded as logical, the events is 'TRUE'.
#'
#' @param min.events Either \code{NULL} (default), or a numeric scalar. If any of the new variables have less events then specified in \code{min.events}, they will be deleted before returning the output data.
#'
#' @param warn Default is \code{FALSE}. If \code{TRUE}, \code{dummy.columns} will print at the console the deleted columns names.
#'
#' @param rm.oc Default is \code{FALSE}. If \code{TRUE}, \code{dummy.columns} will delete the original column before returnig the final \code{data.frame}.
#'
#' @param return.factor Default is \code{TRUE}. If \code{TRUE}, \code{dummy.columns} return factor columns with "0" and "1" levels, or numeric otherwise.
#'
#' @param colnames For \code{rm.dummy.columns} this is the names of the columns to be tested and deleted inside \code{dummy.columns}.
#'
#' @param u A number indicating the total amount of ICUs in the data.
#'
#' @param z_score A numeric vector indicating the standardized Pearson residual or the "naive" Z-Score for each unit.
#'
#' @param y A numeric vector representing the "Standardized rate" for each unit, usually the SMR or possibly the SRU , accordind to \code{y.type}.
#' @param totalObserved The quantity of observed death in all units.
#' @param totalAdmissions The quantity of admissions in all units.
#' @param p A number between 0 and 1 indicating the confidence interval level for the funnel.
#' @param theta Target value which specifies the desired expectation for institutions considered "in control".
#' @param dist A character specifying the distribution about the funnel control limits will be estimated. It can be "binomial" (default), "normal" or "poisson".
#' @param rho A numeric vector representing the funnel precision parameter. It is calculated inside \code{funnel} and used to calculer \code{z_score}.
#' @param gdetheta A numeric auxiliary numeric vector used to calculate \code{z_score} to be used to calculate estimate funnel control limits.
#' @param range A numeric range representing for which values the funnel will be estimated. Usually the same variable in x axis (the precision parameter).
#' @param overdispersion Logical (default = FALSE); If TRUE, introduces an multiplicative over-dispersion factor phi that will inflate the CI null variance. See \code{funnel} details.
#'
#' @author Lunna Borges & Pedro Brasil
#'
#' @seealso \code{\link{dataquality}}
#'
#' @examples
#' # Formating character or factor variable that should be numeric variables
#' f.num(c("2,4000","10,0000","5.0400"))
#'
#' # Simulating a dataset
#' y <- data.frame(v1 = sample(c(" F","M  ","   "), 10, replace = TRUE),
#'                 v2 = sample(c(1:3,"     "), 10, replace = TRUE),
#'                 v3 = sample(c("Alive","Dead",""), 10, replace = TRUE))
#' y
#'
#' # Replacing the "" cells by NA
#' y <- remove.na(y)
#' y
#'
#' rm(y)
#'
#' # Formating dates
#' x <- f.date(c("28/02/2013","16/07/1998","31/03/2010"))
#' x
#' class(x)
#'
#' # The first element (i.e., the different one) is coerced to NA
#' x <- f.date(c("2013-02-28 12:40","16/07/1998","31/03/2010"))
#' x
#' class(x)
#'
#' # The last element (i.e. the different one) is coerced to NA
#' x <- f.date(c("2013-02-28 12:40","1998-07-16 18:50","31/03/2010"))
#' x
#' class(x)
#'
#' # Truncating numeric vectors
#' trunc_num(1:12, min = 3, max = 10)
#'
#' # Truncating numeric vectors but returning NAs instead
#' trunc_num(1:12, min = 3, max = 10, toNA = TRUE)
#'
#'# Simulating a dataset for dummy.columns example
#'
#'y <- data.frame(v1 = 1:20,
#'                v2 = sapply(1:20, function(i) toString(sample(c("Code1","Code2","Code3","Code4"),
#'                      size = sample(2:4, 1), replace = FALSE))))
#'y
#'
#' # For a few of the codes in the original column
#'y <- dummy.columns(y, original.column = "v2", factor = c("Code2","Code3"))
#'y
#'
#' # For all codes in the original column
#'y <- dummy.columns(y[, 1:2], original.column = "v2", scan.oc = TRUE)
#'y
#'
#'rm(y)
#' @rdname miscellaneous
#' @export
f.num <- function(num.var){
  if(!any(class(num.var) %in% c("numeric","integer","double"))){
    if(any(is.character(num.var))){
      if(any(grepl(",",num.var))){num.var <- sub(",", ".", num.var)}
      output <- suppressWarnings(as.numeric(num.var))
    }
    if(any(is.factor(num.var))){
      if(any(grepl(",",levels(num.var)))){levels(num.var) <- sub(",", ".",levels(num.var))}
      output <- suppressWarnings(as.numeric(as.character(num.var)))
    }
  } else {output <- num.var}
  output
}

#' @rdname miscellaneous
#' @export
f.date <- function(date){
  l.date.2 <- length(date) / 2
  if (class(date) != "Date" && any(class(date) != c("POSIXlt", "POSIXt"))) {
    date <- as.character(date)
    if ((sum(substr(date, 5, 5) == "-", na.rm = TRUE) > l.date.2) &
        (sum(substr(date, 14, 14) == ":", na.rm = TRUE) > l.date.2)) {
      date <- strptime(date, "%Y-%m-%d %H:%M")
    } else {
      if (sum(substr(date, 5, 5) == "-", na.rm = TRUE) > l.date.2) {
        date <- as.Date(date)
      }
      if(sum(substr(date, 3, 3) == "/", na.rm = TRUE) > l.date.2) {
        date <- as.Date(date, "%d/%m/%Y")
      }
    }
  }
  date
}

#' @rdname miscellaneous
#' @export
remove.na <- function(data, replace = NA, console.output = TRUE){
  if (!is.data.frame(data)) {
    stop("'data' is not a data.frame.")
  }
  if (length(replace) != 1) {
    stop("'replace' must have length 1.")
  }
  if (!is.logical(console.output)) {
    stop("'console.output' must be either TRUE or FALSE.")
  }
  nc <- dim(data)
  na.sum <- vector(mode = "numeric", length = nc[2])
  for (i in seq_along(names(data))) {
    if (is.character(data[, i])) {
      data[, i] <- trimws(data[, i])
      if (any(data[, i] == "")) {
        na.sum[i] <- sum(data[, i] == "")
        data[which(data[, i] == ""), i] <- replace
      }
    }
    if (is.factor(data[, i])) {
      levels(data[, i]) <- trimws(levels(data[, i]))
      if (any(levels(data[, i]) == "")) {
        na.sum[i] <- sum(data[, i] == "")
        levels(data[, i])[which(levels(data[, i]) == "")] <- replace
      }
    } else {
      na.sum[i] <- -1
    }
  }
  if (console.output) {
    if (any(na.sum == -1)) { na.sum <- na.sum[-which(na.sum == -1)] }
    s <- sum(na.sum, na.rm = TRUE)
    sf <- round( s / (length(na.sum) * nc[1]), 3)
    cat(paste0("Data has ", nc[2] , " columns."),"\n")
    cat(paste0("Data has ", length(na.sum) , " factor variables."),"\n")
    cat(paste0("Data has ", s , " or " , sf, " of the factor fileds withouth data."),"\n")
  }
  data
}

#' @rdname miscellaneous
#' @export
tab2tex <- function(x, nc = ncol(x)){
  x <- x[, 1:nc]
  x  <- cbind(rownames(x), x)
  colnames(x)[1] <- "Variables"
  x <- x[-which(rownames(x) == ""), ]
  rownames(x) <- 1:nrow(x)
  output <- x
  output
}

#' @rdname miscellaneous
#' @export
trunc_num <- function(x, min, max, toNA = FALSE) {
  if (!is.numeric(x)) { stop("'x' must be numeric.")}
  if (!is.logical(toNA)) { stop("'toNA' must be logic.")}
  if (toNA) {
    ifelse(x > max, NA, ifelse(x < min, NA, x))
  } else {
      ifelse(x > max, max, ifelse(x < min, min, x))
  }
}

#' @rdname miscellaneous
#' @export
dummy.columns <- function(data, original.column, factors, scan.oc = FALSE, sep = ",", colnames.add = "Dummy.", min.events = NULL, rm.oc = FALSE, warn = FALSE, return.factor = TRUE) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  if (!is.character(original.column) && !is.numeric(original.column)) {
    stop("'original.colum' must be either a character or a numeric vector.")
  }
  if (length(original.column) != 1) {
    stop("'original.colum' must have length one.")
  }
  if (!any(grepl(original.column, colnames(data)))) {
    stop("'original.colum' is not a variable in 'data'.")
  }
  if (!is.character(sep) || length(sep) != 1) {
    stop("'sep' must be a character vector of length 1.")
  }
  if (!is.logical(rm.oc)) {
    stop("'rm.oc' must be logical.")
  }
  if (!is.logical(scan.oc)) {
    stop("'scan.oc' must be logical.")
  }
  if (scan.oc) {
    factors <- as.character(sort(unique(trimws(scan(text = as.character(data[, original.column]), sep = sep, what = "character", quiet = TRUE)))))
  }
  if (!is.character(factors)) {
    stop("'factors' must be a character vector.")
  }
  if (!is.null(min.events)) {
    if (length(min.events) != 1 || !is.numeric(min.events) || min.events < 1)
    stop("'min.events' must be either null or a positive number.")
  }
  if (!is.logical(return.factor)) {
    stop("'return.factor' must be logical.")
  }

  nr <- nrow(data)
  lf <- length(factors)
  b <- as.data.frame(matrix(NA, nrow = nr, ncol = lf))
  colnames(b) <- paste0(colnames.add, factors)
  if ( return.factor ){
    for (i in 1:lf) {
      b[, i] <- as.factor(ifelse(grepl(factors[i], data[, original.column]), 1, 0))
    }
  } else {
      for (i in 1:lf) {
        b[, i] <- ifelse(grepl(factors[i], data[, original.column]), 1, 0)
      }
  }


  output <- cbind(data, b)
  # Remover as colunas que possuem poucos eventos
  if ( !is.null(min.events) ) {
    output <- rm.dummy.columns(data = output, colnames = colnames.add, min.events = min.events, warn = warn)
  }
  # Remover as colunas que possuem poucos eventos
  if ( rm.oc ) {
    output[, original.column] <- NULL
  }
  output
}

#' @rdname miscellaneous
#' @export
rm.dummy.columns <- function (data, colnames, event = "1", min.events = 50, warn = FALSE) {
  col.index <- grep(colnames, colnames(data))
  event.table <- lapply(data[, col.index], table)
  cond.table <- sapply(seq_along(event.table), function(i) event.table[[i]][event] < min.events)
  cond.table[is.na(cond.table)] <- TRUE
  if (warn) {
    warning(paste("The following columns were deleted:", toString(colnames(data)[col.index[cond.table]])))
  }
  data[, col.index[cond.table]] <- list(NULL)
  data
}

#' @rdname miscellaneous
#' @export
funnelEstimate <- function(y, range, u, totalAdmissions, totalObserved, p = .95, theta = 1, overdispersion = TRUE, dist = c("binomial","normal","poisson"), rho, gdetheta){

  if (!is.numeric(y)){stop("y must be numeric.")}
  if (!is.numeric(u)){stop("u must be numeric.")}
  if (!is.numeric(range)){stop("range must be numeric.")}
  if (!is.numeric(p)){stop("p must be numeric.")}
  if (!is.numeric(theta)){stop("theta must be numeric.")}
  if (!is.logical(overdispersion)){stop("overdispersion must be TRUE or FALSE.")}
  if (dist[1] != "binomial" && dist[1] != "normal" && dist[1] != "poisson"){stop("dist must be either 'binomial', 'normal' or 'poisson'.")}

  #MISSING VERIFICATION FOR RHO AND GDETHETA


  # Calculate the z-score
  z_score <- (y - theta) * sqrt( rho / gdetheta)

  # Calculate the Winsorised estimate
  # Used when overdispersion of the indicator
  phi <- winsorising(z_score, u = u)


  if(dist[1] == "binomial"){
    warning("It is being used exact (binomial) distribuition to draw the funnel plot.")

    if (!is.numeric(totalAdmissions)){stop("totalAdmissions must be numeric.")}
    if (!is.numeric(totalObserved)){stop("totalObserved must be numeric.")}

   # estimativa da probabilidade de ocorrer um evento da binomial
    prob <- totalObserved/totalAdmissions
    # creating binomial quantiles
    rp <- qbinom(p, size = range, prob)
    # correction parameter
    alpha <- (pbinom(rp, size = range, prob) - p) / ((pbinom(rp, size = range, prob)) - pbinom(rp - 1, size = range, prob))

    if( overdispersion & phi > (1 + 2 * sqrt( 2 / u )) ){
      warning("The funnel limits were inflated due overdispersion presence.")
      # funnel confidence intervals
      upperCI <- theta + (rp - alpha) * sqrt(phi) / range
      lowerCI <- theta - (rp - alpha) * sqrt(phi) / range
    } else {
      # funnel confidence intervals
      upperCI <- theta + (rp - alpha) / range
      lowerCI <- theta - (rp - alpha) / range
    }
  }

  if(dist[1] == 'normal'){
    warning("It is being used normal approximation to draw the funnel plot.")

    zp <- qnorm(1 - (1 - p) / 2)

    if ( overdispersion & phi > (1 + 2 * sqrt( 2 / u )) ){
      warning("The funnel limits were inflated due overdispersion presence.")

      upperCI <- theta + zp * sqrt(theta * phi / range)
      lowerCI <- theta - zp * sqrt(theta * phi / range)

    }
    else {
        upperCI <- theta + zp * sqrt(gdetheta / range)
        lowerCI <- theta - zp * sqrt(gdetheta / range)
    }
  }

  if(dist[1] == "poisson"){
    warning("It is being used exact (poisson) distribuition to draw the funnel plot.")

    lambda <- theta

    rp <- qpois(p, lambda)
    alpha <- (ppois(rp, lambda) - p) / (ppois(rp, lambda) - ppois(rp - 1, lambda))

    if ( overdispersion & phi > (1 + 2 * sqrt( 2 / u )) ){
      warning("The funnel limits were inflated due overdispersion presence.")

      upperCI <- theta + (rp - alpha) * sqrt(phi) / range
      lowerCI <- theta - (rp - alpha) * sqrt(phi) / range

    }
    else {

      upperCI <- theta + (rp - alpha) / range
      lowerCI <- theta - (rp - alpha) / range

    }

  }

  output <- list("upperCI" = upperCI, "lowerCI" = lowerCI, "rho" = rho)

  return(output)

}
#' @rdname miscellaneous
#' @export
winsorising <- function(z_score, u){
  if (!is.numeric(z_score)){stop("z_score must be numeric.")}
  if (!is.numeric(u)){stop("u must be numeric.")}
  if (length(u) != 1){stop("u must be of length 1.")}
  #### observe deciles to see if there is NA in 90 percentile
  #### it works to avoid problem to calculate phi if a z_score doesn't have 90 quantile
  deciles <- quantile(z_score, probs = seq(0,1,.1), na.rm = T)
  if (any(is.na(deciles[10]))){
    highestQuantile <- as.numeric((which(is.na(deciles))[1]-2)/10)
  } else { highestQuantile <- 0.9 }

  # Calculate the 10% and 90% percentiles.
  q90 <- quantile(z_score,probs=c(highestQuantile), na.rm = T)
  q10 <- quantile(z_score,probs=c(0.1), na.rm = T)

  # Set z-scores larger than the 90% percentile to the 90% percentile.
  z_score <- ifelse(z_score>q90,q90,z_score)

  # Set z-scores smaller than the 10% percentile to the 10% percentile.
  z_score <- ifelse(z_score<q10,q10,z_score)

  # Calculate the Winsorised estimate
  # Used when overdispersion of the indicator
  phi <- (1 / u) * sum(z_score ^ 2, na.rm = T)

  return(phi)


}
