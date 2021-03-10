#' Collection of functions to check data quality in a dataset and remove not valid or extreme values.
#'
#' @name dataquality
#'
#' @description These functions return the counts and fractions of expected values, unexpected values, missing values and not valid values. They are able to do it with factor variables, numeric variables and date variables. \code{t_factor}, \code{t_num}, and \code{t_date} do the job for a single variable and have simpler arguments, while \code{factor.table}, \code{num.table}, and \code{date.table} do the job for several variables at once. \code{rm.unwanted} cheks the factor and numeric variables and remove the not valid or extreme values. This approach is attractive before data imputation. They all return a \code{data.frame}.
#'
#' \code{t_factor} and \code{factor.table} will try to get factor or character variables and check how much of its content match with the expectd. They will try to treat the levels or cells containing " " as \code{NAs}.
#'
#' \code{t_num} will try to get a numeric variable (even if it is currently formated as character or factor) and check how much of its content is expected (match a desired range), unexpected, non-numeric values and missing vlaues. \code{num.table} does the same, but for two or more variables at once.
#'
#' \code{t_date} will try to get a date variable (even if it is currently formated as character or factor) and check how much of its content is expected (match a desired range), unexpected, non-date values and missing vlaues. \code{date.table} does the same, but for two or more variables at once.
#'
#' \code{rm.unwanted} will chek in data the variables specified in the limits object according to the limits specified for each variable. If there are levels considered not valid in a factor variable, these levels are deleted. For example, if Sex is expected to be "M" and "F", and there is also an "I" level in data, every "I" is replaced by \code{NA}. Similarly, misspelled levels will be understood as non-valid levels and coercerd to \code{NA}, with the exception of leading or trailing empty spaces and lower and upper cases diferences if \code{try.keep = TRUE}. If there is a continuous numeric variable and it is expected to have values ranging from 30 to 700, the values outside this range, i.e. higher then 700 or lower then 30, are replaced by \code{NA}. Non-numeric elements, i.e. non-valid elements that should be numeric, will also be coerced to \code{NA}. If a varible is specified in \code{num.limits}, then it will be returned as a numeric variable, even if it was formated as factor or character. If a variable is specified in limits, the returnig format will depend on the \code{stringAsFactors} argument, unless it is formated as logical. In this case it is skipped. The arguments \code{limits} and \code{num.limits} may be \code{NULL}, meaning that the factor-character variables or the numeric variables , respectively, will not be edited.
#'
#' @param data A data.frame where variables will be tested.
#'
#' @param variable A character vector of length one, indicating the name of the variable in the dataset to be tested.
#'
#' @param legal A character vector representeing the expected levels of the tested variable.
#'
#' @param limits a list of two or more lists, each containing the arguments variable name and legal levels (in this order), to check on the factor variables. In the case of \code{rm.unwanted}, if left NULL, it means no numeric variable will be checked. See examples.
#'
#' @param var.labels Variables labels for a nice output. Must be informed in the same order as variable argument. By default, it captures the labels stored in attr(data, "var.labels"), if any. If not informed, the function returns the variables names.
#'
#' @param num.var A character vector indicating the name of a variable that should be numeric (although it can yet be formated as character or factor).
#'
#' @param num.max,num.min The maximal and minimal limits of acceptable range of a numeric variable.
#'
#' @param num.limits A data.frame with the following variables: num.var, num.max and num.min, representing the numeric variables names, maximal and minimal expected valid values. In the case of \code{rm.unwanted}, if left NULL, it means no numeric variable will be checked. See example.
#'
#' @param digits Number of decimal places for rounding.
#'
#' @param date.var A character vector indicating the name of a variable in data that should be a date (althoug it can yet be formated as character or factor).
#'
#' @param date.max,date.min The maximal and minimal limits of acceptable range of a date variable.
#'
#' @param format.date Default is "auto". If so, \code{t_date} will use \code{\link{f.date}} to detect the date format and format it as date. If not set to "auto", it should be a date format to be passed to \code{\link[base]{as.Date}} format argument. If \code{format.date} is misspecified, then \code{t_date} and \code{date.table} will identify all dates as non-dates. For \code{date.table}, if it is set to 'auto' , it will use \code{\link{f.date}} to detect the date format and format it as date. If different from 'auto', one should specify the desired date formats in the date.limits data.frame. See example.
#'
#' @param date.limits A \code{data.frame} with the following variables: date.var, date.max, date.min, and (optionaly) format.date. These represent values of the arguments above. See example.
#'
#' @param try.keep Default is \code{TRUE}. If \code{TRUE}, \code{remove.unwanted} will first trim all empty spaces and transform all levels to lower case characters before comparing the found levels and expected levels of a character/factor variable. Therefore, found levels such as "yes  " will be considered identical to the expected level "Yes", and will not be coerced to \code{NA}.
#'
#' @param stringAsFactors In \code{rm.unwanted}, if set to \code{TRUE}, the default value, variables in the limits argument that are character and numeric variables in data will be returned as factors. Logical variables are skipped. However, a variable will be returned as logical if it is originally a factor but its final levels are \code{TRUE} and \code{FALSE} and \code{stringAsFactors = FALSE}.
#'
#' @author Lunna Borges & Pedro Brasil
#'
#' @seealso \code{\link{miscellaneous}}
#'
#' @examples
#' # Simulating a dataset with 5 factor variables and assigning labels
#' y <- data.frame(Var1 = sample(c("Yes","No", "Ignored", "", "yes ", NA), 200, replace = TRUE),
#'                 Var2 = sample(c("Death","Discharge", "", NA), 200, replace = TRUE),
#'                 Var3 = sample(c(16:35, NA), 200, replace = TRUE),
#'                 Var4 = sample(c(12:300, "Female", "", NA), 200, replace = TRUE),
#'                 Var5 = sample(c(60:800), 200, replace = TRUE))
#' attr(y, "var.labels") <- c("Intervention use","Unit destination","BMI","Age","Cholesterol")
#' summary(y)
#'
#' # Cheking the quality only the first variable
#' t_factor(y, "Var1", c("Yes","No","Ignored"))
#'
#' # Checking two or more variables at once
#' factor.limits  = list(list("Var1",c("Yes","No")),
#'                       list("Var2",c("Death","Discharge")))
#' factor.table(y, limits = factor.limits)
#'
#' # Checking only one variable that shohuld be numeric
#' t_num(y,"Var3", num.min = 17, num.max = 32)
#'
#' # Making the limits data.frame
#' num.limits <- data.frame(num.var = c("Var3","Var4","Var5"),
#'               num.min = c(17,18,70), num.max = c(32,110,300))
#' num.limits
#'
#' # Checking two or more numeric variables (or the ones that
#' #          should be as numeric) at once
#' num.table(y, num.limits)
#'
#' # Removing the unwanted values (extremes or not valid).
#' y <- rm.unwanted(data = y, limits = factor.limits,
#'                            num.limits = num.limits)
#' summary(y)
#'
#' rm(y, num.limits, factor.limits)
#'#'
#' # Loading a dataset and assigning labels
#' data(icu)
#' attr(icu, "var.labels")[match(c("UnitAdmissionDateTime","UnitDischargeDateTime",
#'    "HospitalAdmissionDate", "HospitalDischargeDate"), names(icu))] <-
#'    c("Unit admission","Unit discharge","Hospital admission","Hospital discharge")
#'
#' # Checking only one variable that should be a date.
#' t_date(icu, "HospitalDischargeDate", date.max = as.Date("2013-10-30"),
#'                                      date.min = as.Date("2013-02-20"))
#'
#' # Checking a date variable misspecifying the date format
#' # will cause the variable dates to be identified as non-date values.
#' t_date(data = icu, date.var = "HospitalDischargeDate",
#'                    date.max = as.Date("2013-10-30"),
#'                    date.min = as.Date("2013-02-20"),
#'                    format.date = "%d/%m/%Y")
#'
#' # Making a limit data.frame assuming an 'auto' format.date
#' d.lim <- data.frame(date.var = c("UnitAdmissionDateTime","UnitDischargeDateTime",
#'                    "HospitalAdmissionDate","HospitalDischargeDate"),
#'                    date.min = rep(as.Date("2013-02-28"), 4),
#'                    date.max = rep(as.Date("2013-11-30"), 4))
#' d.lim
#'
#' # Checking two or more date variables (or the ones that should be as date) at once
#' date.table(data = icu, date.limits = d.lim)
#'
#' # Making a limit data.frame specifying format.date argument
#' # Here the the last 'format.date' is missspecified on purpose
#' # So, the last date will be identified as non-date values.
#' d.lim <- data.frame(date.var = c("UnitAdmissionDateTime","UnitDischargeDateTime",
#'          "HospitalAdmissionDate","HospitalDischargeDate"),
#'           date.min = rep(as.Date("2013-02-28"), 4),
#'           date.max = rep(as.Date("2013-11-30"), 4),
#'           format.date = c(rep("%Y/%m/%d",3), "%Y-%m-%d"))
#' d.lim
#'
#' # Checking the quality of date variable with new limits.
#' # The 'format.date = ""' is required to force the function to look the format
#' # into the date.limits data.frame
#' date.table(data = icu, date.limits = d.lim, format.date = "")
#'
#' rm(icu, d.lim)
#'
#' @export
t_factor <- function(data, variable, legal, var.labels = attr(data, "var.labels")[match(variable, names(data))], digits = 3){
  if (!is.data.frame(data)) {
    stop("Argument 'data' is not a data.frame.")
  }
  if (!is.character(variable)) {
    stop("Argument 'variable' is not a character vector.")
  }
  if (length(variable) != 1) {
    stop("'t_factor' accepts one 'variable' at the time.")
  }
  if (!(variable %in% names(data))) {
    stop(paste(variable, "is not a varaible in", substitute(data)))
  }
  if (any(is.null(var.labels))) {
    var.labels <- variable
  }
  var <- data[ , variable]
  var.dim <- length(var)
  na.sum <- table(is.na(var))[2]
  if (is.na(na.sum)) {na.sum <- 0}
  tmp <- !(var %in% legal)
  table.tmp <- table(tmp)
  if (any(names(table.tmp) == "FALSE")) {
    val.esp <- table.tmp[which(names(table.tmp) == "FALSE")]
  } else {val.esp <- 0}
  if (any(is.na(var))) {
    tmp[which(is.na(var))] <- FALSE
  }
  table.tmp <- table(tmp)
  if (any(names(table.tmp) == "TRUE")) {
    val.n.esp <- table.tmp[which(names(table.tmp) == "TRUE")]
  } else {val.n.esp <- 0}
  output <- c(var.labels , paste0(val.esp, " (", sprintf(paste0("%.",digits,"f"), val.esp / var.dim), ")"),
              paste0(val.n.esp, " (", sprintf(paste0("%.",digits,"f"), val.n.esp/var.dim), ")"),
              paste0(na.sum, " (", sprintf(paste0("%.",digits,"f"), na.sum/var.dim), ")"))
  names(output) <- c("Variable","Expected values","Unexpected values","Missing values")
  output
}

#' @rdname dataquality
#' @export
factor.table <- function(data, limits, var.labels = attr(data, "var.labels")[match(unlist(sapply(seq_along(limits), function(i) limits[[i]][1])), names(data))], digits = 3){
  if (!is.data.frame(data)) {
    stop("Argument 'data' is not a data.frame.")
  }
  if (!is.list(limits)) {
    stop("Argument 'limits' is not a list.")
  }
  var <- unlist(sapply(seq_along(limits), function(i) limits[[i]][1]))
  if (any(!(var %in% names(data)))) {
    stop(paste0("The following variables are not in the dataset: ", toString(var[-which(var %in% names(data))])))
  }
  if (any(duplicated(var))) { stop(paste0("The following variables in limits are not unique: ", var[duplicated(var)]), collapse = TRUE) }

  # cat("Factor variables analysed: \n")
  # cat(var, fill = length(var),"\n")
  output <- as.data.frame(t(sapply(seq_along(limits), function(i) t_factor(data, var[i], unlist(limits[[i]][2]), digits = digits, var.labels = var.labels[i]))))
  output
}

#' @rdname dataquality
#' @export
t_num <- function(data, num.var, num.max = 100, num.min = 0, var.labels = attr(data, "var.labels")[match(num.var, names(data))], digits = 3){
  if (!is.data.frame(data)) {
    stop("Argument 'data' is not a data.frame.")
  }
  if (num.max < num.min) { stop("num.max is lower than num.min.") }
  if (!is.character(num.var)) {
    stop("'t_num' accepts one 'num.var' at the time.")
  }
  if (length(num.var) != 1) {
    stop("'t_num' accepts one 'num.var' at the time.")
  }
  if (!(num.var %in% names(data))) {
    stop(paste(toString(num.var), "is not a varaible in", substitute(data)))
  }
  if (any(is.null(var.labels))) {
    var.labels <- num.var
  }
  num.var <- data[,num.var]
  num.dim <- length(num.var)
  na.sum <- table(is.na(num.var))[2]
  if(is.na(na.sum)){na.sum <- 0}
  fnum <- f.num(num.var)
  #### head(fnum);head(num.var)
  val.n.num <- sum(table(num.var[which(is.na(fnum))]))
  # a <- a[order(date),]
  val.n.esp <- table(ifelse(fnum > num.max | fnum < num.min,1,0))
  if(any(names(val.n.esp) == "0")){val.esp <- val.n.esp[which(names(val.n.esp) == "0")]} else {val.esp <- 0}
  if(any(names(val.n.esp) == "1")){val.n.esp <- val.n.esp[which(names(val.n.esp) == "1")]}  else {val.n.esp <- 0}
  output <- c(var.labels, paste0(val.esp," (",sprintf(paste0("%.",digits,"f"), val.esp/num.dim),")"),
              paste0(val.n.esp," (", sprintf(paste0("%.",digits,"f"),val.n.esp/num.dim),")"),
              paste0(val.n.num," (", sprintf(paste0("%.",digits,"f"), val.n.num/num.dim),")"),
              paste0(na.sum," (", sprintf(paste0("%.",digits,"f"), na.sum/num.dim),")"))
  names(output) <- c("Variable","Expected values","Unexpected values","Non-numeric values","Missing values")
  output
}

#' @rdname dataquality
#' @export
num.table <- function(data, num.limits, var.labels = attr(data, "var.labels")[match(num.limits$num.var, names(data))], digits = 3){
  if (!is.data.frame(data)) {
    stop("Argument 'data' is not a data.frame.")
  }
  if (!is.data.frame(num.limits)) {
    stop("Argument 'num.limits' is not a data.frame.")
  }
  if (!all(c("num.var","num.min","num.min") %in% names(num.limits))) {
    stop("'num.limits' must be a data.frame with the following columns: 'num.var', 'num.min' and 'num.min'.")
  }
  if (!is.character(num.limits$num.var) && !is.factor(num.limits$num.var)) {
    stop("'num.limits$num.var' must be a character or factor variable.")
  }
  if (!is.numeric(num.limits$num.min) || !is.numeric(num.limits$num.max)) {
    stop("'num.limits$num.min' and 'num.limits$num.max' must be numeric vectors.")
  }
  if (any(!(num.limits$num.var %in% names(data)))) {
    stop(paste0("The following variables are not in the dataset: ", toString(num.limits$num.var[-which(num.limits$num.var %in% names(data))])))
  }
  if (any(num.limits$num.max < num.limits$num.min)) {
    stop(paste0("num.max is lower than num.min in ", num.limits$num.var[which(num.limits$num.max < num.limits$num.min)], collapse = ", "))
  }
  num.limits$num.var <- as.character(num.limits$num.var)
  if (any(duplicated(num.limits$num.var))) {
    stop(paste0("The following variables in num.limits are not unique: ", num.limits$num.var[duplicated(num.limits$num.var)]), collapse = TRUE)
  }
  output <- as.data.frame(t(sapply(1:nrow(num.limits), function(i) t_num(data = data, num.var = num.limits$num.var[i], num.max = num.limits$num.max[i], num.min = num.limits$num.min[i], digits = digits, var.labels = var.labels[i]))))
  output
}

#' @rdname dataquality
#' @export
t_date <- function(data, date.var, date.max = as.Date("2010-11-30"), date.min = as.Date("2010-01-31"), format.date = "auto", digits = 3, var.labels = attr(data, "var.labels")[match(date.var, names(data))]){
  if (!is.data.frame(data)) {
    stop("Argument 'data' is not a data.frame.")
  }
  if (date.max < date.min) {
    stop("date.max is lower than date.min.")
  }
  if ( !is.character(date.var) ) {
    stop("'date.var' is not a character vector.")
  }
  if (length(date.var) != 1) {
    stop("'t_date' accepts one 'date.var' at the time.")
  }
  if (!(date.var %in% names(data))) {
    stop(paste(toString(date.var), "is not a varaible in", substitute(data)))
  }
  if (is.null(var.labels)) {
    var.labels <- date.var
  }
  date <- data[ , date.var]
  date.dim <- length(date)
  na.sum <- table(is.na(date))[2]
  if ( is.na(na.sum) ) { na.sum <- 0 }
  if (format.date == "auto") {
    fdate <- f.date(date) # class(fdate) ; head(fdate)
  } else {
    fdate <- as.Date(date, format = format.date) # class(fdate) ; head(fdate)
  }
  if (any(class(fdate) %in% c("POSIXlt", "POSIXt"))) {fdate <- as.Date(fdate)}
  date <- as.character(date) # class(date) ; head(date)
  val.n.date <- sum(table(date[which(is.na(fdate))]))
  val.n.esp <- table(ifelse(fdate > date.max | fdate < date.min, 1, 0))
  val.esp <- val.n.esp[which(names(val.n.esp) == "0")]
  if (is.na(val.esp) || is.null(val.esp) || length(val.esp) == 0) { val.esp <- 0 }
  val.n.esp <- val.n.esp[which(names(val.n.esp) == "1")]
  if(is.na(val.n.esp) || is.null(val.n.esp) || length(val.n.esp) == 0){val.n.esp <- 0}
  output <- c(var.labels,
              paste0(val.esp, "(", sprintf(paste0("%.", digits,"f"), val.esp / date.dim), ")"),
              paste0(val.n.esp, "(", sprintf(paste0("%.",digits,"f"), val.n.esp / date.dim), ")"),
              paste0(val.n.date, "(", sprintf(paste0("%.",digits,"f"), val.n.date / date.dim), ")"),
              paste0(na.sum, "(", sprintf(paste0("%.",digits,"f"), na.sum / date.dim), ")"))
  names(output) <- c("Variables","Expected values","Unexpected values","Non-dates values","Missing values")
  output
}

#' @rdname dataquality
#' @export
date.table <- function(data, date.limits, format.date = "auto", digits = 3, var.labels = attr(data, "var.labels")[match(date.limits$date.var, names(data))]){
  if (!is.data.frame(data)) {
    stop("Argument 'data' is not a data.frame.")
  }
  if (!is.data.frame(date.limits)) {
    stop("Argument 'date.limits' is not a data.frame.")
  }
  if (!all(c("date.var","date.max","date.min") %in% names(date.limits))) {
    stop("'date.limits' must be a data.frame having the following columns: 'date.var', 'date.min' and 'date.max'.")
  }
  if (!is.character(date.limits$date.var) && !is.factor(date.limits$date.var)) {
    stop("'date.limits$date.var' must be a character or factor variable.")
  }
  if (class(date.limits$date.min) != "Date" || class(date.limits$date.max) != "Date") {
    stop("'date.limits$date.min' and 'date.limits$date.max' must be Date vectors.")
  }
  if (any(!(date.limits$date.var %in% names(data)))) {
    stop(paste0("The following variables are not in the dataset: ", toString(date.limits$date.var[-which(date.limits$date.var %in% names(data))])))
  }
  if (any(date.limits$date.max < date.limits$date.min)) {
    stop(paste0("date.max is lower than date.min in ", date.limits$date.var[which(date.limits$date.max < date.limits$date.min)], collapse = " ,"))
  }
  date.limits$date.var <- as.character(date.limits$date.var)
  if (any(duplicated(date.limits$date.var))) {
    stop(paste0("The following variables in num.limits are not unique: ", date.limits$date.var[duplicated(date.limits$date.var)]), collapse = TRUE)
  }
  if ( format.date == "auto") {
    output <- as.data.frame(t(sapply(1:nrow(date.limits), function(i) t_date(data = data, date.var = date.limits$date.var[i], date.max = date.limits$date.max[i], date.min = date.limits$date.min[i], digits = digits, var.labels = var.labels[i]))))
  }
  if ( format.date != "auto") {
    if ( any(!("format.date" %in% names(date.limits))) ) {
      stop("'date.limits' must have a column 'format.date' with the desired date formats \n
           or 'format.date' argument must be set to 'auto'")
    }
    date.limits$format.date <- as.character(date.limits$format.date)
    output <- as.data.frame(t(sapply(1:nrow(date.limits), function(i) t_date(data = data, date.var = date.limits$date.var[i], date.max = date.limits$date.max[i], date.min = date.limits$date.min[i], digits = digits, var.labels = var.labels[i], format.date = date.limits$format.date[i]))))
  }
  output
}

#' @rdname dataquality
#' @export
rm.unwanted <- function(data, limits = NULL, num.limits = TRUE, try.keep = TRUE, stringAsFactors = TRUE) {
  if (!is.data.frame(data)) {
    stop("Argument 'data' is not a data.frame.")
  }
  if ( !is.logical(try.keep) ) {
    stop ("'try.keep' must be logical.")
  }
  if ( !is.null(num.limits) ) {
    if (!is.data.frame(num.limits)) {
      stop("Argument 'num.limits' is not a data.frame.")
    }
    if (!all(c("num.var","num.min","num.min") %in% names(num.limits))) {
      stop("'num.limits' must be a data.frame with the following columns: 'num.var', 'num.max' and 'num.min'.")
    }
    if (any(!is.character(num.limits$num.var)) && any(!is.factor(num.limits$num.var))) {
      stop("'num.limits$num.var' must be a character or factor variable.")
    }
    if (any(!is.numeric(num.limits$num.min)) || any(!is.numeric(num.limits$num.max))) {
      stop("'num.limits$num.min' and 'num.limits$num.max' must be numeric vectors.")
    }
    if (any(num.limits$num.max < num.limits$num.min)) {
      stop(paste0("num.max is lower than num.min in ", num.limits$num.var[which(num.limits$num.max < num.limits$num.min)], collapse = ", "))
    }
    if (any(!(num.limits$num.var %in% names(data)))) {
      warning(paste0("The following variables are not in the dataset, and will be ignored: ", toString(num.limits$num.var[-which(num.limits$num.var %in% names(data))])))
      # Removendo de num.limits variavies inexistentes nos dados
      num.limits <- subset(num.limits, subset = num.limits$num.var %in% names(data))
    }
    if (any(duplicated(num.limits$num.var))) {
      warning(paste0("The following variables are duplicated in num.limits, and will be ignored: ", toString(num.limits$num.var[duplicated(num.limits$num.var)])))
      # Removendo de num.limits variavies inexistentes nos dados
      num.limits <- subset(num.limits, subset = !duplicated(num.limits$num.var))
    }
  }
  if ( !is.null(limits) ) {
    if ( !is.list(limits) ) {
      stop("Argument 'limits' is not a list.")
    }
    var <- unlist(sapply(seq_along(limits), function(i) limits[[i]][1]))
    # var[4] <- "Var8"
    if ( any(!(var %in% names(data))) ) {
      warning(paste0("The following variables are not in the dataset, and will be ignored: ", toString(var[-which(var %in% names(data))])))
      nu <- sapply(seq_along(limits), function(i) limits[[i]][[1]]) %in%  names(data)
      # nu[3] <- FALSE
      limits[!nu] <- list(NULL)
    }
    if ( any(duplicated(var)) ) {
      warning(paste0("The following variables are duplicated in 'limits', and will be ignored: ", toString(var[duplicated(var)])))
      limits[duplicated(var)] <- list(NULL)
    }
  }

  if ( !is.null(limits) ) {
    for (i in seq_along(limits)) {
      # i = 2
      # i = 1
      CurrentVar <- limits[[i]][[1]]
      CurrentLimits <- limits[[i]][[2]]
      if ( is.character(data[, CurrentVar ]) ) {
        # Tranformando para factor para facilitar
        data[, CurrentVar ] <- as.factor(data[, CurrentVar ])
        if ( try.keep ) {
          DataLevels <- levels( data[ , CurrentVar ] )
          nValidLevels <-  DataLevels[!(DataLevels %in% CurrentLimits)]
          if ( length(nValidLevels) > 0 ) {
            WhichnValidLevels <- which(tolower(trimws(nValidLevels)) %in% tolower(CurrentLimits))
            if ( length(WhichnValidLevels) >= 1 ) {
              levels(data[, CurrentVar ])[sapply(nValidLevels[WhichnValidLevels], grep, x = DataLevels)] <- sapply(WhichnValidLevels, function(j) CurrentLimits[ tolower(CurrentLimits) == tolower(trimws(nValidLevels[j])) ])
            }
          }
        }
        levels(data[, CurrentVar ]) <- ifelse(!(levels(data[, CurrentVar ]) %in% CurrentLimits), NA, levels(data[, CurrentVar ]))
        if ( !stringAsFactors ) {
          data[, CurrentVar ] <- as.character(data[, CurrentVar ])
        }
      }
      if ( is.factor(data[, CurrentVar ]) ) {
        if ( try.keep ) {
          DataLevels <- levels( data[ , CurrentVar ] )
          nValidLevels <-  DataLevels[!(DataLevels %in% CurrentLimits)]
          if ( length(nValidLevels) > 0 ) {
            WhichnValidLevels <- which(tolower(trimws(nValidLevels)) %in% tolower(CurrentLimits))
            if ( length(WhichnValidLevels) >= 1 ) {
              levels(data[, CurrentVar ])[sapply(nValidLevels[WhichnValidLevels], grep, x = DataLevels)] <- sapply(WhichnValidLevels, function(j) CurrentLimits[ tolower(CurrentLimits) == tolower(trimws(nValidLevels[j])) ])
            }
          }
        }
        levels(data[, CurrentVar ]) <- ifelse(!(levels(data[, CurrentVar ]) %in% CurrentLimits), NA, levels(data[, CurrentVar ]))
        if (!stringAsFactors) {
          if (all(levels(data[, CurrentVar ]) %in% c("FALSE","TRUE"))) {
            data[, CurrentVar ] <- as.logical(data[, CurrentVar ])
          }
        }
      }
      if ( is.numeric(data[, CurrentVar ]) ) {
        # Tranformando para factor para facilitar
        data[, CurrentVar ] <- as.factor(data[, CurrentVar ])
        if ( try.keep ) {
          DataLevels <- levels( data[ , CurrentVar ] )
          nValidLevels <-  DataLevels[!(DataLevels %in% CurrentLimits)]
          if ( length(nValidLevels) > 0 ) {
            WhichnValidLevels <- which(tolower(trimws(nValidLevels)) %in% tolower(CurrentLimits))
            if ( length(WhichnValidLevels) >= 1 ) {
              levels(data[, CurrentVar ])[sapply(nValidLevels[WhichnValidLevels], grep, x = DataLevels)] <- sapply(WhichnValidLevels, function(j) CurrentLimits[ tolower(CurrentLimits) == tolower(trimws(nValidLevels[j])) ])
            }
          }
        }
        levels(data[, CurrentVar ]) <- ifelse(!(levels(data[, CurrentVar ]) %in% CurrentLimits), NA, levels(data[, CurrentVar ]))
        if ( !stringAsFactors ) {
          data[, CurrentVar ] <- as.numeric(as.character(data[, CurrentVar ]))
        }
      }
    } # Termina o seq limits
  }
  if ( !is.null(num.limits) ) {
    num.limits$num.var <- as.character(num.limits$num.var)
    for (i in 1:nrow(num.limits)) {
      # i = 2
      # i = grep("BUN", names(data))
      if ( !is.numeric(data[, num.limits[i, 1]]) ) {
        data[, num.limits[i, 1]] <- trunc_num(x = as.numeric(as.character(data[, num.limits[i, 1]])), max = num.limits[i, "num.max"], min = num.limits[i, "num.min"], toNA = TRUE)
      } else {
        data[, num.limits[i, 1]] <- trunc_num(x = data[, num.limits[i, 1]], max = num.limits[i, "num.max"], min = num.limits[i, "num.min"], toNA = TRUE)
      }
    }
  }
  data
}


