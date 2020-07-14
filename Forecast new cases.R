## ---------------------------------------------------------------------------
## Covid 19 FORECAST NEW CASES v1.1
##          Data fuente: Our World in Data
## Author: Javier Chang
##
## Forecast con regresion con Curva Normal
## ---------------------------------------------------------------------------

## PREREQUISITES 
if (!require("ggplot2")) {
     install.packages("ggplot2", dependencies = TRUE)
     library(ggplot2)
}
if (!require("gridExtra")) {
        install.packages("gridExtra", dependencies = TRUE)
        library(gridExtra)
}



## PARAMETERS
## ----------
country="PER"  ## PER Peru, PRT Portugal, USA Estados Unidos, KOR Korea, CHN China
numdias <- 100  ## Numero de dias para el forecast

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
hoy=unclass(Sys.Date())-1


## STEP 2 CALCULA PRONOSTICOS
## --------------------------------------------
fechainicio <- unclass(as.Date("2020-03-15"))
covid$fecha <- unclass(covid$date)
covid$diasemana <- factor(weekdays(covid$date), c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"), ordered=TRUE)
##covid$proptest <- covid$new_cases / covid$new_tests

## Predicción New Cases
x <- covid$fecha
y <- covid$new_cases
modelo.nc <-
     nls(
          y ~ maximo * exp(-((x - mu) ^ 2) / (2 * sigma ^ 2)) / (sigma * sqrt(2 * pi)),
          start = list(
               sigma = 22,
               mu = hoy - 10,
               maximo = 200000
          ),
          control = c(maxiter = 100)
     )
rangoprediccion <-seq(fechainicio, hoy + numdias)
futuro <-
     data.frame(date = as.Date(rangoprediccion, origin="1970-01-01"), 
                new_cases = round(predict(modelo.nc, newdata=data.frame(x=rangoprediccion))))

## Calcula Total Cases
desde=hoy
hasta=hoy+numdias
for (i in fechainicio:hasta){
        if (i<hoy){
                futuro$total_cases[futuro$date==i]<- covid$total_cases[covid$date==i]
        } else{
                futuro$total_cases[futuro$date==i]<- futuro$total_cases[futuro$date==i-1] + futuro$new_cases[futuro$date==i]
        }
     
}


## STEP 3 GRAFICA LOS DATOS
## --------------------------------------------

# Grafica New Cases
newcasestoday <- covid$new_cases[covid$fecha==hoy]
g1<-ggplot(covid, aes(date, new_cases))+
        ggtitle(label=paste("Nuevos casos COVID-19",country))+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_point()+
        geom_line(data=futuro, colour="red")+
        geom_point(aes(x=as.Date(hoy, origin="1970-01-01"), y=newcasestoday), colour="red", size=2)

## Gráfica Total Cases
g2<-ggplot(covid, aes(date, total_cases))+
        ggtitle(paste("Total casos confirmados COVID-19",country))+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_point()+
        geom_line(data=futuro, colour="red")

## Grafica nuevos casos por dia de la semana
g3<-ggplot(covid, aes(diasemana, new_cases))+
        ggtitle(paste("Nuevos casos por dia de la semana",country))+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_boxplot()+
        geom_point(aes(x=weekdays(as.Date(hoy, origin="1970-01-01")), y=newcasestoday), colour="red", size=2)

## Grafica proporción de nuevos casos vs nuevos tests
g4 <- ggplot(covid, aes(date, new_tests))+
        ggtitle(paste("Total de tests realizados",country))+
        theme(plot.title = element_text(hjust = 0.5))+
        geom_col()+
        geom_line(aes(date, new_cases), colour="red")

grid.arrange(g1, g2, g3, g4, ncol=2, nrow=2)