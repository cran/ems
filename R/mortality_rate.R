#' Mortality Rate
#'
#' @name mortality_rate
#'
#' @description \code{mortality_rate} function returns a list with the mortality rate and the number of patients for each month or quarter of the year.
#'
#' @param deaths a numerical vector that only contains 0 and 1, indicating whether the patient was alive or dead, respectively.
#'
#' @param period a numerical vector that contains the order of months when the patients were admitted to the hospital unit. If period variable is NULL (the default), the function will return a single mortality rate.
#' 
#' @param isQuarter logical indicating whether the \code{period} refers to quarter or not. The default is \code{FALSE}.
#' 
#' @param isYear logical indicating whether the \code{period} refers to years or not. The default is \code{FALSE}.
#'
#' @param periodName a character vector that contains the name of months when the patients were admitted to de hospital unit. Used only if period is not NULL.
#' 
#' @param default_tapply argument to set the \code{default} in \code{tapply} function when evaluating the mortality rate for each period. Can be equal to \code{0} or \code{NA} (the default).
#'
#' @param option a character string which determines what the function mortality_rate returns. If the option is chosen to be 'both' (the default), the function will return a list containing monthly mortality rate, quarterly mortality rate, annual mortality rate and the number of patients in each month, quarter and year. If the option is 'monthly', only the monthly mortality rate and the number of patients in each month are returned. If the option is 'quarterly', only the quarterly mortality rate and the number of patients in each quarter are returned. If the option is 'annual', only the annual mortality rate and the number of patients in each year are returned.
#'
#' @author
#' Camila Cardoso <camila.cardoso@epimedsolutions.com>
#' Lunna Borges <lunna.borges@epimedsolutions.com>
#'
#' @examples
#'
#' # Loading the dataset
#' data(icu)
#'
#' # Creating a vector of months
#' date <- as.Date(icu$UnitDischargeDateTime, tryFormats = '%d/%m/%Y')
#' months <- as.numeric(format(date, '%m'))
#'
#' # Vector of deaths
#' deaths <- icu$UnitDischargeName
#'
#' # Calculating monthly and quarterly mortality rate
#' mortality_rate(deaths = deaths, period = months, option = 'both')
#'
#'
#' @export


mortality_rate <- function(deaths, period = NULL, isQuarter = FALSE, isYear = FALSE, 
                           option = c('both', 'monthly', 'quarterly', 'annual'),
                           periodName = NULL, default_tapply = NA){
  
  #### Convertions and tests ####
  
  # Convertions
  deaths <- as.numeric(deaths)
  
  # Tests
  if(!is.numeric(deaths)){
    stop("'deaths' must be a numeric vector.")
  }
  if(any(deaths != 0 & deaths != 1)){
    stop("'deaths' variable must be coded as 0 and 1.")
  }
  if(!is.numeric(period) & ! is.null(period)){
    stop('period variable must be numeric or NULL.')
  }
  if(!is.logical(isQuarter)){
    stop("'isQuarter' variable must be logical.")
  }
  if(!is.logical(isYear)){
    stop("'isYear' variable must be logical.")
  }
  if(isQuarter + isYear == 2){
    stop("'isQuarter' and 'isYear' cannot be TRUE at the same time.")
  }
  if(!default_tapply %in% c(NA, 0)){
    stop("'default_tapply' must be NA or 0.")
  }
  # periodName is not being used anymore
  
  
  ### Function ### 
  
  if(!is.null(period)){
    
    period     <- as.numeric(period)
    periodName <- as.character(periodName)
    
    
    if(isQuarter){
      
      # period refers to quarters and the function will only return quarterly mortality rate
      if(option[1] != 'quarterly'){
        warning(paste0("The mortality rates will be evaluated for quarters even though option == '", option[1], "'"))
      }
      quarters <- period
      
      # Number of observations in each quarter
      n_quarts <- as.numeric(table(quarters))
      
      # Quarterly mortality rate 
      quarterly_rate <- tapply(X = deaths, INDEX = quarters, FUN = mean, na.rm = TRUE, default = default_tapply)
      
      # Output of the function
      output <- list('quarterly_rate' = quarterly_rate, 'n_quarts' = n_quarts)
      
      
    } else if(isYear){
      
      # period refers to years and the function will only return annual mortality rate 
      if(option[1] != 'annual'){
        warning(paste0("The mortality rates will be evaluated for years even though option == '", option[1], "'"))
      } 
      years <- period
      
      # Number of observations in each year
      n_years <- as.numeric(table(years))
      
      # Annual mortality rate 
      annual_rate <- tapply(X = deaths, INDEX = years, FUN = mean, na.rm = TRUE, default = default_tapply)
      
      # Output of the function
      output <- list('annual_rate' = annual_rate, 'n_years' = n_years)
      
      
    } else{
      
      # period refers to months and other periods will be generated from it according to option[1]
      
      # Number of observations in each month
      n_months <- as.numeric(table(period))
      
      # Monthly mortality rate
      monthly_rate <- tapply(X = deaths, INDEX = period, FUN = mean, na.rm = TRUE, default = default_tapply)
      
      
      if(option[1] == 'monthly'){
        
        # The function will return only monthly mortality rate 
        output <- list('monthly_rate' = monthly_rate, 'n_months' = n_months)
        
        
      } else if(option[1] == 'quarterly'){
        
        # Creating a vector of quarters 
        quarters <- rep(NA, length(period))
        breaks   <- seq(1, max(period, na.rm = TRUE) - max(period, na.rm = TRUE) %% 3 + 2 * 3, 3)
        labels   <- 1:(length(breaks) - 1)
        quarters <- as.numeric(cut(period, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels))
        
        # Number of observations in each quarter
        n_quarts <- as.numeric(table(quarters))
        
        # Quarterly mortality rate 
        quarterly_rate <- tapply(X = deaths, INDEX = quarters, FUN = mean, na.rm = TRUE, default = default_tapply)
        
        # Output of the function
        output <- list('quarterly_rate' = quarterly_rate, 'n_quarts' = n_quarts)
        
        
      } else if(option[1] == 'annual'){
        
        if(length(unique(period)) <= 12){
          stop("Doesn't make sense to choose for 'annual' option because you only have 12 periods (1 year).")
        }
        
        # Creating a vector of years 
        years  <- rep(NA, length(period))
        breaks <- seq(1, max(period, na.rm = TRUE) - max(period, na.rm = TRUE) %% 12 + 2 * 12, 12)
        labels <- 1:(length(breaks) - 1)
        years  <- as.numeric(cut(period, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels))
        
        # Number of patients in each year 
        n_years <- as.numeric(table(years))
        
        # Annual mortality rate 
        annual_rate <- tapply(X = deaths, INDEX = years, FUN = mean, na.rm = TRUE, default = default_tapply)
        
        # Output of the function
        output <- list('annual_rate' = annual_rate, 'n_years' = n_years)
        
        
      } else{
        
        # Creating a vector of quarters 
        quarters <- rep(NA, length(period))
        breaks   <- seq(1, max(period, na.rm = TRUE) - max(period, na.rm = TRUE) %% 3 + 2 * 3, 3)
        labels   <- 1:(length(breaks) - 1)
        quarters <- as.numeric(cut(period, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels))
        
        # Number of observations in each quarter
        n_quarts <- as.numeric(table(quarters))
        
        # Quarterly mortality rate 
        quarterly_rate <- tapply(X = deaths, INDEX = quarters, FUN = mean, na.rm = TRUE, default = default_tapply)
        
        # Output of the function 
        output <- list('monthly_rate' = monthly_rate, 'n_months' = n_months, 
                       'quarterly_rate' = quarterly_rate, 'n_quarts' = n_quarts)
        
        
        if(length(unique(period)) > 12){
          
          # Creating a vector of years 
          years  <- rep(NA, length(period))
          breaks <- seq(1, max(period, na.rm = TRUE) - max(period, na.rm = TRUE) %% 12 + 2 * 12, 12)
          labels <- 1:(length(breaks) - 1)
          years  <- as.numeric(cut(period, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels))
          
          # Number of patients in each year 
          n_years <- as.numeric(table(years))
          
          # Annual mortality rate 
          annual_rate <- tapply(X = deaths, INDEX = years, FUN = mean, na.rm = TRUE, default = default_tapply)
          
          # Output of the function 
          output$annual_rate <- annual_rate
          output$n_years     <- n_years
          
          
        }
        
      }
      
    }
    
    
  } else{
    
    # The function will return an unique mortality rate 
    mortality_rate <- mean(deaths, na.rm = TRUE)
    output <- mortality_rate
    
  }
  
  return(output)
  
}



