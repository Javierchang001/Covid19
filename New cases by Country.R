## ---------------------------------------------------------------------------
## Covid 19 GRAFICA CURVAS DE NUEVOS CASOS DE PAÍSES
##          Data fuente: Our World in Data
## Author: Javier Chang
##
## Grafica las curvas de nuevos casos donde primero apareció el virus y con mayor cantidad de casos
## ---------------------------------------------------------------------------

## PREREQUISITES 
if (!require("ggplot2")) {
     install.packages("ggplot2", dependencies = TRUE)
     library(ggplot2)
}
if (!require("dplyr")) {
     install.packages("dplyr", dependencies = TRUE)
     library(dplyr)
}

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


## STEP 2 SELECCIONA LOS PAISES CON MÁS CASOS
## ------------------------------------------
s <- covid[covid$total_cases>0 & covid$iso_code!="",] %>%
     group_by(iso_code) %>%
     summarise(numdays=n(), date=first(date), total_cases=sum(new_cases)) %>%
     arrange(desc(total_cases), date) %>%
     top_n(15)
covid <- subset(covid, (iso_code %in% s$iso_code) & total_cases>0)


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
