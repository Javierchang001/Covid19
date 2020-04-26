## Covid 19 Peru data
##
country="PER"

## Descarga archivo
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

## Casos confirmados
par(mfrow=c(2,2))

with(covid, {
  plot(
    date,
    total_cases,
    type = "n",
    xlab = "Fecha",
    ylab = "Cantidad",
    main = paste("Casos confirmados COVID-19 en", country)
  )
  lines(date, total_cases, type = "l", col = "black")
  lines(date, new_cases, type = "l", col = "red")
})

legend(
  "topleft",
  col = c("black", "red"),
  lty = c(1, 1, 1),
  legend = c("Total cases", "New cases")
)

legend(
  "top",
  legend = c(format(today$total_cases, big.mark = ",", width=7, justify="right"),
             format(today$new_cases, big.mark = ",", width=7, justify="right"))
)


## Muertes
with(covid, {
  plot(
    date,
    total_deaths,
    type = "n",
    xlab = "Fecha",
    ylab = "Cantidad",
    main = paste("Muertes COVID-19 en", country)
  )
  lines(date, total_deaths, type = "l", col = "black")
  lines(date, new_deaths, type = "l", col = "red")
})

legend(
  "topleft",
  col = c("black", "red"),
  lty = c(1, 1, 1),
  legend = c("Total cases", "New cases")
)

legend(
  "top",
  legend = c(format(today$total_deaths, big.mark = ",", width=7, justify="right"),
             format(today$new_deaths, big.mark = ",", width=7, justify="right"))
)


## Predicción Total cases
numdias <- 20
covid$fecha <- unclass(covid$date)
model.m1 <- drm(covid$total_cases ~ covid$fecha, fct = LL.4())
futuro <-
  data.frame(date = as.Date(seq(hoy + 1, hoy + numdias), origin = "1970-01-01"))
futuro$total_cases <-
  predict(model.m1, data.frame(seq(hoy + 1, hoy + numdias)))
combinado <- merge(covid, futuro, all = TRUE)
plot(
  combinado$date,
  combinado$total_cases,
  type = "l",
  lty = 2,
  col = "red",
  xlab = "Fecha",
  ylab = "Cantidad",
  main = "Proyección casos confirmados"
)
lines(covid$date, covid$total_cases, type = "l")


## Predicción Total deaths
model.m2 <- drm(covid$total_deaths ~ covid$fecha, fct = LL.4())
futuro <-
  data.frame(date = as.Date(seq(hoy + 1, hoy + numdias), origin = "1970-01-01"))
futuro$total_deaths <-
  predict(model.m2, data.frame(seq(hoy + 1, hoy + numdias)))
combinado <- merge(covid, futuro, all = TRUE)
plot(
  combinado$date,
  combinado$total_deaths,
  type = "l",
  lty = 2,
  col = "red",
  xlab = "Fecha",
  ylab = "Cantidad",
  main = "Proyección muertes",
  ylim = c(0,max(futuro$total_deaths))
)
lines(covid$date, covid$total_deaths, type = "l")
