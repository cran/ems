#' Miscellaneous functions for data editing
#'
#' @name miscellaneous
#'
#' @description Collections of functions for data editing usually used as lower levels for other functions.
#'
#' \code{f.num} is a wrapper to format numeric variables that are stored as character or factor, simultaneously it will try to detect comma spearated and replace it by dots before formating the variable as numeric. Any non-numeric encoding will be coerced to NA.
#'
#'\code{f.date} is a wrapper either to \code{\link[base]{as.Date}} or \code{\link[base]{strptime}} to format character or factor variables into dates. In Epimed Solutions database there are a few pre-specified formats that \code{f.date} will try to detect and return a formated date. \code{f.date} will try to dected if more than half of the elements in a vector have a pre-specified format. If so, the remaining will be coerced to NA if they have different format from the detected. See example.
#'
#' \code{remove.na} identifies all the empties spaces, i.e. the " " cells, of character or factor variables in a data.frame and returns the same data.frame with these empty cells replaced, by default, by NAs. It does not matter the length of the empty spaces. Also, \code{remove.na} trims the leading and trailing empty spaces from all character and factor variables. It does not format the numeric variables. It may also returns at the console a few information about the " " fields.
#'
#' \code{tab2tex} removes the empty rows, and also tunrs the rownames of a table epiDisplay::tableStack into the first column, to make it easier to paste the table into a rtf or latex document without empty rows or rownames conflicts.
#'
#' \code{trunc_num} truncates a numeric vector by replacing the values below the min value or above the max values by the min and max values respectively. See example.
#'
#' @param num.var A character, or factor variable to be formated as numeric.
#'
#' @param date A character or factor variable to be formated as date.
#'
#' @param data A data.frame.
#'
#' @param replace By default, NA. But could be any vector of length 1.
#'
#' @param console.output Logical. Print at the console a few information about the "" fields?
#'
#' @param x,nc For \code{tab2tex} x is a object from epiDisplay::tableStack. nc is the number of the last column to keep in the table. If the table has 5 columns and nc = 3, then columns 4 and 5 are removed. For \code{trunc_num}, x is a numeric vector.
#'
#' @param min,max For \code{trunc_num}, min and max are the minimal and maximal numeric values where the numeric vector will be truncated.
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
  if (class(date) != "Date") {
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
trunc_num <- function(x, min, max) {
  if (!is.numeric(x)) { stop("'x' is not numeric.")}
  ifelse(x > max, max, ifelse(x < min, min, x))
}
