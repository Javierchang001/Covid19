## ---------------------------------------------------------------------------
## Covid 19 GRAFICA CURVAS DE NUEVOS CASOS DE PAÍSES
##          Data fuente: Our World in Data
## Author: Javier Chang
##
## Regresión con ajuste a Curva Normal
## ---------------------------------------------------------------------------

## PREREQUISITES 
if (!require("ggplot2")) {
     install.packages("ggplot2", dependencies = TRUE)
     library(ggplot2)
}

## PARAMETERS
## ----------
country="PER"  ## PER Peru, PRT Portugal, USA Estados Unidos, KOR Korea, CHN China
numdias <- 20  ## Número de días para el forecast

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
covid <- subset(covid, iso_code %in% list("CHN", "PER", "PTR", "USA", "KOR","ESP", "ITA", "IRN", "CHL", "DEU"))
today<-subset(covid, covid$date==Sys.Date())
hoy=unclass(Sys.Date())



## STEP 3 GRAFICA LOS DATOS
## --------------------------------------------

# Grafica New Cases
ggplot(covid, aes(date, new_cases_per_million)) +
     ggtitle("Nuevos casos por millón de habitantes COVID-19 por países")+
     theme(plot.title = element_text(hjust = 0.5))+
     geom_point(size=0.75) + 
     facet_wrap(~iso_code, ncol=3) +
     geom_smooth()+
     ylim(0,125)
