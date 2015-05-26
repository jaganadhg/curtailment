# combines models
library("EBMAforecast")
library("ensembleBMA")
data(srft)
summary(srft)
members <- c("CMCG", "ETA", "GASP", "GFS",
             "JMA", "NGPS", "TCWB", "UKMO")
srftData = ensembleData(forecasts = srft[,members],
               dates = srft$date, observations = srft$observation,
               latitude = srft$latitude, longitude = srft$longitude,
               forecastHour = 48)

srftFit = ensembleBMA( srftData, dates = "2004013100",
               model = "normal", trainingDays = 25)

plot( srftFit, srftData, dates = "2004013100")