## ---------------------------------------------------------------------------
## Covid 19 FORECAST BY COUNTRY
##          Data fuente: Our World in Data
## Author: Javier Chang
## ---------------------------------------------------------------------------

## PREREQUISITES
if (!require("drc")) {
  install.packages("drc", dependencies = TRUE)
  library(drc)
}


## PARAMETERS
## ----------
country="PER"  ## PER Peru, PRT Portugal, USA Estados Unidos
numdias <- 20  ## Número de días para el forecast
fn <- logistic() ## DRC model to predict LL.4(), logistic()

## STEP 1 DESCARGA DATA
## --------------------
dfile <- "owid-covid-data.csv"
if (!file.exists(dfile) | as.Date(file.mtime(dfile)) != Sys.Date()) {
  download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                destfile = dfile)
}
covid <- read.csv(dfile,
                    header = TRUE,
                    colClasses = c(date = "Date"))
covid <- subset(covid, iso_code == country)
today<-subset(covid, covid$date==Sys.Date())
hoy=unclass(Sys.Date())


## STEP 2 CALCULA PRONOSTICOS
## --------------------------------------------
fechainicio <- unclass(as.Date("2020-03-15"))
covid$fecha <- unclass(covid$date)
futuro <-
  data.frame(date = as.Date(seq(fechainicio, hoy + numdias), origin = "1970-01-01"))

## Predicción Total Cases
model.m1 <- drm(covid$total_cases ~ covid$fecha, fct = fn) 
futuro$total_cases <-
  predict(model.m1, data.frame(seq(fechainicio, hoy + numdias)))

# Predicción Total Deaths
model.m2 <- drm(covid$total_deaths ~ covid$fecha, fct = fn) 
futuro$total_deaths <-
  predict(model.m2, data.frame(seq(fechainicio, hoy + numdias)))


## STEP 3 GRAFICA LOS DATOS
## --------------------------------------------

## Grafica Predicción Total cases
par(mfrow=c(1,2))

plot(
  covid$date,
  covid$total_cases,
  type = "n",
  xlab = "Fecha",
  ylab = "Cantidad",
  main = paste("Casos confirmados COVID-19 en", country),
  xlim = c(min(covid$date), max(futuro$date)),
  ylim = c(0, max(futuro$total_cases))
)
lines(futuro$date, futuro$total_cases, type = "l", lty=2, col="blue")
lines(covid$date, covid$total_cases, type = "l", lty=1, col="black")
lines(covid$date, covid$new_cases, type = "l", col = "red")

legend(
  "topleft",
  bty = "n",
  col = c("red", "black", "blue"),
  lty = c(1, 1, 2),
  legend = c(paste("New cases  ",format(today$new_cases, big.mark = ",", width=7, justify="right")), 
             paste("Total cases",format(today$total_cases, big.mark = ",", width=7, justify="right")), 
             paste("Forecast   ", format(round(max(futuro$total_cases)), big.mark = ",", width=7, justify="right")))
)

## Grafica Predicción Total deaths
plot(
  covid$date,
  covid$total_deaths,
  type = "n",
  xlab = "Fecha",
  ylab = "Cantidad",
  main = paste("Proyección muertes por COVID-19 en", country),
  xlim = c(min(covid$date), max(futuro$date)),
  ylim = c(0,max(futuro$total_deaths))
)
lines(futuro$date, futuro$total_deaths, type = "l", lty=2, col="blue")
lines(covid$date, covid$total_deaths, type = "l")
lines(covid$date, covid$new_deaths, type = "l", col = "red")

legend(
  "topleft",
  bty = "n",
  col = c("red", "black", "blue"),
  lty = c(1, 1, 2),
  legend = c(paste("New cases  ",format(today$new_deaths, big.mark = ",", width=7, justify="right")), 
             paste("Total cases",format(today$total_deaths, big.mark = ",", width=7, justify="right")), 
             paste("Forecast   ", format(round(max(futuro$total_deaths)), big.mark = ",", width=7, justify="right")))
)

