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
#' @param periodName a character vector that contains the name of months when the patients were admitted to de hospital unit. Used only if period is not NULL.
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


mortality_rate <- function(deaths, period = NULL,
                           option = c('both', 'monthly', 'quarterly', 'annual'),
                           periodName = NULL){

  deaths <- as.numeric(deaths)



  ###### Conditions

  if(!is.numeric(deaths)){
    stop("'deaths' must be a numeric vector.")
  }
  if(any(deaths != 0 & deaths != 1)){
    stop("'deaths' variable must be coded as 0 and 1.")
  }
  if(!is.numeric(period) & ! is.null(period)){
    stop('period variable must be numeric or NULL.')
  }
  


  ##### Function

  if(!is.null(period)){

    period     <- as.numeric(period)
    periodName <- as.character(periodName)

    ### Monthly mortality rate
    monthly_rate <- tapply(X = deaths, INDEX = period, FUN = mean)

    ### Months names
    qtd_months   <- sort(unique(period))
    # months_names <- periodName[match(qtd_months,period)]


    ### Creating a new "column" of quarters
    quarters <- rep(NA, length(period))
   
    breaks   <- seq(1, max(period, na.rm = TRUE) - max(period, na.rm = TRUE) %% 3 + 2 * 3, 3)
    labels   <- 1:(length(breaks) - 1)
    quarters <- as.numeric(cut(period, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels))
    
    # n_intervals <- round(length(unique(period)) / 3)
    # quarters <- cut(period, breaks = n_intervals, labels = seq(1,n_intervals))
    
    
    ### Quarterly mortality rate
    quarterly_rate <- tapply(X = deaths, INDEX = quarters, FUN = mean)

    ### Quarter names
    # qtd_quarters <- length(unique(quarters))
    # quarter_names <- rep(NA, qtd_quarters)
    # j = 1
    # for (i in 1:qtd_quarters){
    #   quarter_names[i] <- paste0(months_names[j], '-', months_names[j+2])
    #   if(is.na(months_names[j+2])){
    #     quarter_names[i] <- paste0(months_names[j])
    #   }
    #   j <- j+3
    # }


    ### Creating a new column of years only if there is more than 12 periods

    if(length(unique(period)) > 12){
      
      years <- rep(NA, length(period))
      
      breaks <- seq(1, max(period, na.rm = TRUE) - max(period, na.rm = TRUE) %% 12 + 2 * 12, 12)
      labels <- 1:(length(breaks) - 1)
      years  <- as.numeric(cut(period, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = labels))

      # n_intervals <- round(length(unique(period)) / 12)
      # years <- cut(period, breaks = n_intervals, labels = seq(1,n_intervals))
      
      ### Annual mortality rate
      annual_rate <- tapply(X = deaths, INDEX = years, FUN = mean)

      ### Year names
      # qtd_years <- length(unique(years))
      # years_names <- rep(NA, qtd_years)
      # 
      # j <- 1
      # for (i in 1:qtd_years){
      #   years_names[i] <- paste0(months_names[j],'-',months_names[j+11])
      #   if(is.na(months_names[j+11])){
      #     years_names[i] <- paste0(months_names[j])
      #   }
      #   j <- j+12
      # }

      ### Number of patients
      n_years <- as.vector(table(years))

    } else {
      if(option[1] == 'annual'){
        stop("Doesn't make sense to choose for 'annual' option because you only have 12 periods (1 year).")
      }
    }


    ### Number of patients
    n_months <- as.vector(table(period))
    n_quarts <- as.vector(table(quarters))
    
    
    if(option[1] == 'both'){
      if(length(unique(period)) > 12){
        
        output <- list(monthly_rate = monthly_rate, quarterly_rate = quarterly_rate, annual_rate = annual_rate,
                       n_months = n_months, n_quarts = n_quarts, n_years = n_years)
        
      } else {
        
        output <- list(monthly_rate = monthly_rate, quarterly_rate = quarterly_rate, 
                       n_months = n_months, n_quarts = n_quarts)
        
      }

    }

    if(option[1] == 'monthly'){
      output <- list(monthly_rate = monthly_rate, n_months = n_months)
    }

    if(option[1] == 'quarterly'){
      output <- list(quarterly_rate = quarterly_rate, n_quarts = n_quarts)
    }

    if(option[1] == 'annual'){
      output <- list(annual_rate = annual_rate, n_years = n_years)
    }

  }

  else{

    mortality_rate <- mean(deaths)
    output <- mortality_rate

  }

  return(output)

}


