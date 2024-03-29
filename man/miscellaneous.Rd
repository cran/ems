% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.R
\name{miscellaneous}
\alias{miscellaneous}
\alias{f.num}
\alias{f.date}
\alias{remove.na}
\alias{tab2tex}
\alias{trunc_num}
\alias{dummy.columns}
\alias{rm.dummy.columns}
\alias{funnelEstimate}
\alias{winsorising}
\title{Miscellaneous functions for data editing}
\usage{
f.num(num.var)

f.date(date)

remove.na(data, replace = NA, console.output = TRUE)

tab2tex(x, nc = ncol(x))

trunc_num(x, min, max, toNA = FALSE)

dummy.columns(
  data,
  original.column,
  factors,
  scan.oc = FALSE,
  sep = ",",
  colnames.add = "Dummy.",
  min.events = NULL,
  rm.oc = FALSE,
  warn = FALSE,
  return.factor = TRUE
)

rm.dummy.columns(data, colnames, event = "1", min.events = 50, warn = FALSE)

funnelEstimate(
  y,
  range,
  u,
  totalAdmissions,
  totalObserved,
  p = 0.95,
  theta = 1,
  overdispersion = TRUE,
  dist = c("binomial", "normal", "poisson"),
  rho,
  gdetheta
)

winsorising(z_score, u)
}
\arguments{
\item{num.var}{A character, or factor variable to be formated as numeric.}

\item{date}{A character or factor variable to be formated as date.}

\item{data}{A \code{data.frame}.}

\item{replace}{By default, NA. But could be any vector of length 1.}

\item{console.output}{Logical. Print at the console a few informations about the " " fields?}

\item{x, nc}{For \code{tab2tex} x is a object from epiDisplay::tableStack. nc is the number of the last column to keep in the table. If the table has 5 columns and nc = 3, then columns 4 and 5 are removed. For \code{trunc_num}, x is a numeric vector.}

\item{min, max}{For \code{trunc_num}, min and max are the minimal and maximal numeric values where the numeric vector will be truncated.}

\item{toNA}{For \code{trunc_num}, if FALSE any min and max are the minimal and maximal numeric values where the numeric vector will be truncated.}

\item{original.column}{A character vector representing the name of the column the be transformed in dummy variables.}

\item{factors}{A character vector to make new dummy columns and to match values in \code{original.column}. This is interesting if the user desires to make dummy only from a few factors in the originlal column. Ignored if scan.oc = TRUE}

\item{scan.oc}{Default = FALSE, if TRUE, \code{dummy.columns} scans the specified \code{original.column} and uses all factors to generate dummy variables. It overrides the \code{factor} argument.}

\item{sep}{A character of legth one that systematically split the factors in the original columns. It wil be passed to the \code{sep} argument in the \code{\link[base]{scan}} function.}

\item{colnames.add}{The default is '= "Dummy_"'. This is a character vector of length one to stick in the \code{colnames} of the dummy variables. For example, if the orginal column has A;B;C factor levels, the new dummy variables \code{colnames} would be "Dummy_A", "Dummy_B", and "Dummy_C"}

\item{min.events}{Either \code{NULL} (default), or a numeric scalar. If any of the new variables have less events then specified in \code{min.events}, they will be deleted before returning the output data.}

\item{rm.oc}{Default is \code{FALSE}. If \code{TRUE}, \code{dummy.columns} will delete the original column before returnig the final \code{data.frame}.}

\item{warn}{Default is \code{FALSE}. If \code{TRUE}, \code{dummy.columns} will print at the console the deleted columns names.}

\item{return.factor}{Default is \code{TRUE}. If \code{TRUE}, \code{dummy.columns} return factor columns with "0" and "1" levels, or numeric otherwise.}

\item{colnames}{For \code{rm.dummy.columns} this is the names of the columns to be tested and deleted inside \code{dummy.columns}.}

\item{event}{A character string to be detected as a event. In \code{rm.dummy.columns}, if the columns are coded as '0' and '1', the event is '1', if it is coded as logical, the events is 'TRUE'.}

\item{y}{A numeric vector representing the "Standardized rate" for each unit, usually the SMR or possibly the SRU , accordind to \code{y.type}.}

\item{range}{A numeric range representing for which values the funnel will be estimated. Usually the same variable in x axis (the precision parameter).}

\item{u}{A number indicating the total amount of ICUs in the data.}

\item{totalAdmissions}{The quantity of admissions in all units.}

\item{totalObserved}{The quantity of observed death in all units.}

\item{p}{A number between 0 and 1 indicating the confidence interval level for the funnel.}

\item{theta}{Target value which specifies the desired expectation for institutions considered "in control".}

\item{overdispersion}{Logical (default = FALSE); If TRUE, introduces an multiplicative over-dispersion factor phi that will inflate the CI null variance. See \code{funnel} details.}

\item{dist}{A character specifying the distribution about the funnel control limits will be estimated. It can be "binomial" (default), "normal" or "poisson".}

\item{rho}{A numeric vector representing the funnel precision parameter. It is calculated inside \code{funnel} and used to calculer \code{z_score}.}

\item{gdetheta}{A numeric auxiliary numeric vector used to calculate \code{z_score} to be used to calculate estimate funnel control limits.}

\item{z_score}{A numeric vector indicating the standardized Pearson residual or the "naive" Z-Score for each unit.}
}
\description{
Collection of functions for data editing, usually used as lower levels for other functions.

\code{f.num} is a wrapper to format numeric variables that are stored as character or factor, simultaneously it will try to detect comma spearated and replace it by dots before formating the variable as numeric. Any non-numeric encoding will be coerced to NA.

\code{f.date} is a wrapper either to \code{\link[base]{as.Date}} or \code{\link[base]{strptime}} to format character or factor variables into dates. In Epimed Solutions database there are a few pre-specified formats that \code{f.date} will try to detect and return a formated date. \code{f.date} will try to dected if more than half of the elements in a vector have a pre-specified format. If so, the remaining will be coerced to NA if they have different format from the detected. See example.

\code{remove.na} identifies all the empty spaces, i.e. the " " cells, of character or factor variables in a data.frame and returns the same data.frame with these empty cells replaced, by default, by NAs. It does not matter the length of the empty spaces. Also, \code{remove.na} trims the leading and trailing empty spaces from all character and factor variables. It does not format the numeric variables. It may also return at the console a few information about the " " fields.

\code{tab2tex} removes the empty rows, and also tunrs the rownames of a table epiDisplay::tableStack into the first column, to make it easier to paste the table into a rtf or latex document without empty rows or rownames conflicts.

\code{trunc_num} truncates a numeric vector by replacing the values below the min value or above the max values by the min and max values respectively or optionall to NA. See example.

\code{dummy.columns} takes a \code{data.frame} with one column with concatatenated levels of a factor (or character) variable and return a \code{data.frame} with additional columns with zeros and ones (dummy values), which names are the factor levels of the original column. See example below. \code{rm.dummy.columns} is an internal function of \code{dummy.columns} that deletes the new dummy columns which have less then a specified minimum events.

\code{funnelEstimate}  estimates funnel confidence intervals (CI) for binomial, poisson or normal distribution. Used inside \code{\link[ems]{funnel}}.

\code{winsorising} is an internal function that estimates a phi parameter after shirinking extreme z-scores. This parameter is used to inflate funnel CIs due overdispersion presence.
}
\examples{
# Formating character or factor variable that should be numeric variables
f.num(c("2,4000","10,0000","5.0400"))

# Simulating a dataset
y <- data.frame(v1 = sample(c(" F","M  ","   "), 10, replace = TRUE),
                v2 = sample(c(1:3,"     "), 10, replace = TRUE),
                v3 = sample(c("Alive","Dead",""), 10, replace = TRUE))
y

# Replacing the "" cells by NA
y <- remove.na(y)
y

rm(y)

# Formating dates
x <- f.date(c("28/02/2013","16/07/1998","31/03/2010"))
x
class(x)

# The first element (i.e., the different one) is coerced to NA
x <- f.date(c("2013-02-28 12:40","16/07/1998","31/03/2010"))
x
class(x)

# The last element (i.e. the different one) is coerced to NA
x <- f.date(c("2013-02-28 12:40","1998-07-16 18:50","31/03/2010"))
x
class(x)

# Truncating numeric vectors
trunc_num(1:12, min = 3, max = 10)

# Truncating numeric vectors but returning NAs instead
trunc_num(1:12, min = 3, max = 10, toNA = TRUE)

# Simulating a dataset for dummy.columns example

y <- data.frame(v1 = 1:20,
               v2 = sapply(1:20, function(i) toString(sample(c("Code1","Code2","Code3","Code4"),
                     size = sample(2:4, 1), replace = FALSE))))
y


# For a few of the codes in the original column
y <- dummy.columns(y, original.column = "v2", factor = c("Code2","Code3"))
y

# For all codes in the original column
y <- dummy.columns(y[, 1:2], original.column = "v2", scan.oc = TRUE)
y

# Funnel Estimate
data(icu)
icu

funnelEstimate(y = icu$Saps3DeathProbabilityStandardEquation,
               range = 1, u = length(unique(icu$Unit)),
               totalAdmissions = nrow(icu),
               totalObserved = sum(icu$UnitDischargeName),
               theta = mean(icu$Saps3DeathProbabilityStandardEquation),
               dist = 'normal', rho = 1, gdetheta = 1)

rm(y, icu)

}
\seealso{
\code{\link{dataquality}}
}
\author{
Lunna Borges & Pedro Brasil
}
