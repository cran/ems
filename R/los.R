# cria vetor com tempo de perman?ncia de cada paciente

los <- function (data, Adate.var = "UnitAdmissionDate", Atime.var = "UnitAdmissionTime", Ddate.var = "UnitDischargeDate", Dtime.var = "UnitDischargeTime", format = "%Y-%m-%d %H:%M"){



  if(!is.null(Atime.var) & !is.null(Dtime.var)){
    # alteracao feita para o banco espanhol
    data[,Atime.var] <- format(strptime(data[,Atime.var], format = "%Y-%m-%d %H:%M"),format = "%H:%M")
    data[,Dtime.var] <- format(strptime(data[,Dtime.var], format = "%Y-%m-%d %H:%M"),format = "%H:%M")

    AdDateTime <- c()
    for (i in 1:nrow(data)){
      AdDateTime <- append(AdDateTime, paste(data[i,Adate.var], data[i,Atime.var], sep=" "))
    }
    data$AdDateTime <- AdDateTime

    DisDateTime <- c()
    for (i in 1:nrow(data)){
      DisDateTime <- append(DisDateTime, paste(data[i,Ddate.var], data[i,Dtime.var], sep = " "))
    }
    data$DisDateTime <- DisDateTime
    formattime <- "%Y-%m-%d %H:%M"
  } else {
    data$DisDateTime <- data[,Ddate.var]
    data$AdDateTime <- data[,Adate.var]
    formattime <- "%Y-%m-%d"
  }


  # los <- strptime(data$DisDateTime, format = format) - strptime(data$AdDateTime, format = format)
  los <- as.numeric(difftime(strptime(data$DisDateTime, format = formattime), strptime(data$AdDateTime, format = formattime), units="days"))
   # data$los = as.numeric(los/86400) #in days
   data$DisDateTime <- NULL
   data$AdDateTime <- NULL
   # data
   los
}

# exemplo
# dt <- baseteste[1:3,17:20]
# los(dt,"UnitAdmissionDate", "UnitAdmissionTime", "UnitDischargeDate", "UnitDischargeTime")

# adicionar avisos de erros para par?metros
# documentar na ems
