#' Calibration Curve
#'
#' @name calcurve
#'
#' @description \code{calcurve} function returns a data.frame containing the number of patients, the observed mortality rate and the predicted mortality rate for each category of the predicted mortality rate. If any other acute physiology score is given, the function will also return the mortality rate predicted by this score for each category.
#'
#' @param deaths a numerical vector that only contains 0 and 1, indicating whether the patient was alive or dead, respectively.
#'
#' @param pred a numerical vector that contains the mortality rate predicted by the main score, in percentage, for each patient.
#'
#' @param score a numerical vector that contains the main score punctuation for each patient, or NULL.
#'
#' @param name_score a character string which determines the name of the main score.
#'
#' @param other_score a list of numerical vectors, where each vector contains the mortality rate predicted by other score, in percentage, for each patient, or NULL (the default).
#'
#' @param name_other_score if other_score variable is different from NULL, this argument must be a vector with the name(s) of the score(s) given.
#'
#' @param categories_option a character string which determines if the categories will refer to the main score or to the predicted mortality rate. Accepted values are 'predicted' (the default), 'score' or 'patients'.
#'
#' @param table logical; if \code{TRUE} prints the \code{data.frame}.
#'
#' @param plot logical; if \code{TRUE} (the default) plots the categories chosen versus the mortality rates in the secondary vertical axis. The main vertical axis refers to the number of patients in each category, represented by the bars.
#'
#' @param title_label main title for \code{calcurve}.
#'
#' @param y1axis_label,y2axis_label labels of the main vertical axis and the secondary axis, respectively, for \code{calcurve}.
#'
#' @param score_color a vector with the colors to be used in the score traces for \code{calcurve}.
#'
#' @param bar_color color of the bars for \code{calcurve}.
#'
#' @param points a vector with markers types of the scores for \code{calcurve}.
#'
#' @param x an object of class 'calcurve'.
#'
#' @param main main title for \code{plot.calcurve}.
#'
#' @param text label of the secondary vertical axis for \code{plot.calcurve}.
#'
#' @param ylab label of the main vertical axis for \code{plot.calcurve}.
#'
#' @param col character vector with the colors of the bars and score traces, in this order, for \code{plot.calcurve}.
#'
#' @param pch a vector with markers types of the scores for \code{plot.curve}.
#'
#' @param ... further arguments passed to \code{\link[graphics]{plot}}.
#'
#' @param cultureCode a character specifying which language should be used for plot x axis title and legends. Possible values are 'pt-BR' and 'en-US'. The default is 'en-US'.
#'
#' @param legend_inset inset distance(s) from the margins as a fraction of the plot region when legend is placed by keyword. See \code{\link[graphics]{legend}}.
#'
#' @param xlab label of the horizontal axis for \code{plot.calcurve}, defaults to \code{NULL}.
#'
#' @param ylab2 label of the secondary vertical axis for \code{plot.calcurve}, defaults to \code{NULL}.
#'
#' @details
#' \itemize{
#' \item If \code{categories_option = 'score'}, the categories will refer to the deciles of the main score punctuation. If \code{categories_option = 'predicted'}, the categories will refer to fixed intervals of the predicted mortality rate. If \code{categories_option = 'patients'} the categories will refer to the deciles of patients.
#' }
#'
#'
#'
#' @author Camila Cardoso
#'
#' @examples
#'
#'
#' # Loading the dataset
#' data(icu)
#'
#' # Calibration Curve Plot
#' a <- calcurve(deaths = icu$UnitDischargeName,
#' pred = icu$Saps3DeathProbabilityStandardEquation,
#' score = icu$Saps3Points, name_score = 'Saps3',
#' categories_option = 'predicted', table = FALSE, plot = TRUE)
#'
#'
#' @export

calcurve <- function(deaths, pred,
                     score = NULL, name_score = 'Saps3',
                     other_score = NULL, name_other_score = NULL,
                     categories_option = c("predicted","score", "patients"),
                     table = FALSE, plot = TRUE,
                     title_label = 'Calibration Curve',
                     y1axis_label = 'Patients (n)',
                     y2axis_label = 'Mortality Rate (%)',
                     score_color = c('#cac7cc', '#ffc341',
                                     '#33cca3'),
                     bar_color = '#1f77b4',
                     points = c(19, 18, 17),
                     cultureCode = 'en-US',
                     legend_inset = -0.7
){



  deaths <- as.numeric(deaths)
  pred   <- as.numeric(pred)

  if(!is.null(score)){
    score <- as.numeric(score)
  }

  if(!is.null(other_score)){
    for(i in 1:length(other_score)){
      other_score[[i]] <- as.numeric(other_score[[i]])
    }
  }



  ### Conditions

  if(!is.numeric(deaths)){
    stop("deaths variable must be numeric.")
  }

  if(any(deaths != 0 & deaths != 1)){
    stop("deaths variable must be coded as 0 and 1.")
  }

  if(!is.numeric(pred)){
    stop("pred variable must be numeric.")
  }

  if(any(pred < 0) || any(pred > 100)){
    stop("pred variable must be between 0 and 100.")
  }

  if(!is.null(score) & !is.numeric(score)){
    stop('score variable must be NULL or numeric.')
  }

  if(!is.null(other_score) & !is.list(other_score)){
    stop('other_score variable must be a list or NULL.')
  }

  if(!is.null(other_score)){
    for(i in 1:length(other_score)){
      if(!is.numeric(other_score[[i]])){
        stop('other_score variable must be a list of numeric vectors.')
      }
      if(any(other_score[[i]] < 0) || any(other_score[[i]] > 100)){
        stop('other_score variable must be a list of vectors with numbers between 0 and 100.')
      }
    }
  }

  if(is.null(name_score)){
    stop('name_score variable cannot be NULL.')
  }

  if(length(name_score) > 1 || !is.character(name_score)){
    stop('name_score variable is a character string of lenght 1.')
  }

  if(length(other_score) > 0){
    if(length(other_score) != length(name_other_score)){
      stop('name_other_score variable must be a vector with the names of the scores given.')
    }
  }

  if (!(categories_option[1] %in% c("score", "predicted", "patients"))) {
    stop("categories_option variable must be either \"score\", \"predicted\", or \"patients\".")
  }

  if(categories_option[1] == 'score'){
    if(is.null(score)){
      stop('score variable cannot be NULL if the categories_option chosen is "score".')
    }
  }



  ### Function

  if(categories_option[1] == 'score'){

    deciles    <- quantile(sort(score), seq(0,1,0.1))
    categories <- cut(x = score, breaks = deciles, include.lowest = T)

  } else if(categories_option[1] == "predicted"){

    categories <- cut(x = pred, breaks = seq(0, 100, 10), include.lowest = T)

  } else {
    deaths <- deaths[order(pred)]
    pred <- sort(pred)
    deciles <- quantile(1:length(deaths),
                        0:10/10)

    quantiles_pred <- round(quantile(pred, 0:10/10), 3)
    categories_labels = paste0("(", quantiles_pred[1:10], ", ",
                               quantiles_pred[2:11], "]")

    substr(categories_labels[1], 1, 1) <- "["

    categories <- cut(x = order(pred, decreasing = F),
                      breaks = deciles,
                      include.lowest = T)

    categories <- factor(categories, labels = categories_labels)
  }

  # Observed Mortality by Category
  obs_rate_by_cat <- 100 * tapply(X = deaths, INDEX = categories, FUN = mean)

  # Predicted Mortality by Category
  pred_rate_by_cat <- tapply(X = pred, INDEX = categories, FUN = mean)

  if(categories_option == "patients"){
    categories_labels <- unique(categories_labels)
    df <- data.frame(row.names = 1:length(categories_labels),
                     'categories' = categories_labels,
                     'n' = as.vector(table(categories)),
                     obs_rate_by_cat,
                     pred_rate_by_cat)
  }else{
    df <- data.frame(row.names = 1:10, 'categories' = names(table(categories)),
                     'n' = as.vector(table(categories)), obs_rate_by_cat,
                     pred_rate_by_cat)

  }


  if(!is.null(other_score)){
    for(i in 1:(length(other_score))){

      col_name <- paste0('score',i + 1,'_pred_by_cat')
      column   <- tapply(X = other_score[[i]], INDEX = categories, FUN = mean)

      df <- data.frame(df, column)
      names(df)[4 + i] <- col_name

    }
  }


  output <- list(df = df,
                 name_score = name_score,
                 name_other_score = name_other_score,
                 categories_option = categories_option,
                 title_label  = title_label,
                 y1axis_label = y1axis_label,
                 y2axis_label = y2axis_label,
                 score_color = score_color,
                 bar_color = bar_color,
                 points = points,
                 cultureCode = cultureCode,
                 legend_inset = legend_inset)

  class(output) <- 'calcurve'

  if(table){

    print(output)

  }

  if(plot){

    plot(output)

  }

  return(output)

}





#' @rdname calcurve
#' @export

print.calcurve <- function(x, ...){
  print(x$df, ...)
}



#' @rdname calcurve
#' @export

plot.calcurve <- function(x, ..., xlab = NULL,
                          ylab2 = NULL,
                          main = x$title_label,
                          text = x$y2axis_label,
                          ylab = x$y1axis_label,
                          col = c(x$bar_color, x$score_color),
                          pch = x$points,
                          cultureCode = x$cultureCode,
                          legend_inset = x$legend_inset
){

  y    <- x$df
  cat  <- y$categories
  freq <- y$n
  obs  <- y$obs_rate_by_cat
  pred <- y$pred_rate_by_cat

  obs[which(is.na(obs))]   <- 0
  pred[which(is.na(pred))] <- 0

  name_score       <- x$name_score
  name_other_score <- x$name_other_score
  categories_option <- x$categories_option

  ### y2lim

  maximum <- max(apply(y[-c(1,2)], MARGIN = 2, FUN = max))



  ### Labels

  observed_label  <- 'Observed Mortality Rate'

  score_legend <- paste('Mortality Rate Predicted by', name_score)
  if(!is.null(name_other_score)){
    for(i in 1:length(name_other_score)){
      score_legend[i + 1] <- paste('Mortality Rate Predicted by',
                                   name_other_score[i])
    }
  }

  if(!is.null(ylab2)){
    text <- ylab2
  }

  if(!is.null(xlab)){
    xaxis_label <- xlab
  }else{
    if(categories_option[1] == 'score'){

      xaxis_label <- paste('Deciles of the Score', name_score)

    } else if(categories_option[1] == 'predicted'){

      xaxis_label <- paste('Fixed Categories of the Mortality Rate Predicted by', name_score)

    } else {
      xaxis_label <- paste('Mortality Rate Predicted by', name_score)
    }
  }

  if(cultureCode == 'pt-BR'){

    observed_label <- 'Mortalidade observada'

    score_legend <- paste('Mortalidade esperada pelo', name_score)
    if(!is.null(name_other_score)){
      for(i in 1:length(name_other_score)){
        score_legend[i + 1] <- paste('Mortalidade esperada pelo',
                                     name_other_score[i])
      }
    }

    if(categories_option[1] == 'score'){

      xaxis_label <- paste('Decis do Score', name_score)

    } else if(categories_option[1] == 'predicted'){

      xaxis_label <- paste('Mortalidade prevista pelo', name_score)

    } else {
      xaxis_label <- paste('Mortalidade prevista pelo', name_score)
    }

  }

  ### Plot

  options(scipen=999)

  par(mar=c(7,4,4,5)+.1)
  bp <- barplot(height = freq, names.arg = cat, ...,
                xlab = xaxis_label, ylab = ylab, main = main,
                ylim = c(0, max(freq) * 1.3),
                col = col[1], border = NA)

  par(new=TRUE)
  bp_new <- barplot(height = rep(0, nrow(y)), names.arg = '', ...,
                    xlab = '', ylab = '', main = '', axes = F,
                    ylim = c(0, maximum * 1.2),
                    col = 1, border = NA, plot = T)
  axis(side = 4)
  mtext(text = text, ..., side = 4, line = 3)


  lines(x = bp_new[which(obs != 0)], y = obs[which(obs != 0)],
        lty = 1, lwd = 2, col = 1)

  lines(x = bp_new[which(pred != 0)], y = pred[which(pred != 0)],
        lty = 2, lwd = 2, col = col[2])
  points(x = bp_new[which(pred != 0)], y = pred[which(pred != 0)],
         pch = pch[1], cex = 1.3, col = col[2])


  if(dim(y)[2] > 4){
    for(i in 5:dim(y)[2]){
      score_new <- y[,i]
      score_new[which(is.na(score_new))] <- 0

      lines(x = bp_new[which(score_new != 0)],
            y = score_new[which(score_new != 0)],
            lty = 2, lwd = 2, col = col[i - 2])
      points(x = bp_new[which(score_new != 0)],
             y = score_new[which(score_new != 0)],
             pch = pch[i - 3], cex = 1.5, col = col[i - 2])
    }
  }

  par(new = TRUE)
  bp <- barplot(height = rep(0, nrow(y)), names.arg = '', axes = F,
                xlab = '', ylab = '', main = '',
                ylim = c(0, max(freq) * 1.3),
                col = col[1], border = NA)
  text(x = bp, y = freq, labels = freq, cex = 0.75, pos = 3, offset = 0.2, font = 2)


  par(xpd = T)

  legend        <- c(observed_label, score_legend)
  colors        <- c(1, col[2:(length(score_legend) + 1)])
  types         <- c(1, rep(2, length(score_legend)))
  markers_types <- c(NA, pch[1:length(score_legend)])
  width_markers <- c(0, rep(1.2, length(score_legend)))

  legend(x = 'bottom', inset = legend_inset, legend = legend, col = colors,
         lty = types, pch = markers_types, cex = 0.7, lwd = 1, bty = 'n',
         pt.cex = width_markers, ncol = 2, xjust = 0.5)

}



