#' Mortality Rate
#'
#' @name mortality_rate
#'
#' @description \code{mortality_rate} function returns a list with the mortality rate and the number of patients for each month or quarter of the year.
#'
#' @param deaths a numerical vector that only contains 0 and 1, indicating whether the patient was alive or dead, respectively.
#'
#' @param period a numerical vector that contains the months when the patients were admitted to the hospital unit. If period variable is NULL (the default), the function will return a single mortality rate.
#'
#' @param option a character string which determines what the function mortality_rate returns. If the option is chosen to be 'both' (the default), the function will return a list containing monthly mortality rate, quarterly mortality rate and the number of patients in each month and quarter. If the option is 'monthly', only the monthly mortality rate and the number of patients in each month are returned. If the option is 'quarterly', only the quarterly mortality rate and the number of patients in each quarter are returned.
#'
#' @author Camila Cardoso
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
                           option = c('both', 'monthly', 'quarterly')
                           ){

  deaths <- as.numeric(deaths)



  ###### Conditions

  if(!is.numeric(deaths)){
    stop('deaths variable must be numeric.')
  }

  if(any(deaths != 0 & deaths != 1)){
    stop('deaths variable must be coded as 0 and 1.')
  }

  if(!is.numeric(period) & !is.null(period)){
    stop('period variable must be numeric or NULL.')
  }

  if(any(period < 1) || any(period > 12)){
    stop('period variable must be between 1 and 12.')
  }



  ##### Function

  if(!is.null(period)){

    period <- as.numeric(period)

    ### Monthly mortality rate
    monthly_rate <- tapply(X = deaths, INDEX = period, FUN = mean)

    ### Creating a new "column" of quarters
    quarters <- rep(NA, length(period))
    quarters <- ifelse(period == 1 | period == 2 | period == 3, 1,
                       ifelse(period == 4 | period == 5 | period == 6, 2,
                              ifelse(period == 7 | period == 8 | period == 9,
                                     3, 4)))

    ### Quarterly mortality rate
    quarterly_rate <- tapply(X = deaths, INDEX = quarters, FUN = mean)


    ### Number of patients
    n_months <- as.vector(table(period))
    n_quarts <- as.vector(table(quarters))


    if(option[1] == 'both'){
      output <- list(monthly_rate = monthly_rate, quarterly_rate = quarterly_rate,
                     n_months = n_months, n_quarts = n_quarts)
    }

    if(option[1] == 'monthly'){
      output <- list(monthly_rate = monthly_rate, n_months = n_months)
    }

    if(option[1] == 'quarterly'){
      output <- list(quarterly_rate = quarterly_rate, n_quarts = n_quarts)
    }

  }

  else{

    mortality_rate <- mean(deaths)
    output <- mortality_rate

  }

  return(output)

}


