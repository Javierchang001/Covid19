## ---------------------------------------------------------------------------
## Covid 19 STATS BY COUNTRY v1.1
##          Data fuente: Our World in Data
## Author: Javier Chang
##
## Estadísticas por país
## ---------------------------------------------------------------------------

## PREREQUISITES 
if (!require("dplyr")) {
        install.packages("dplyr", dependencies = TRUE)
        library(dplyr)
}
if (!require("ggplot2")) {
     install.packages("ggplot2", dependencies = TRUE)
     library(ggplot2)
}
if (!require("ggdendro")) {
        install.packages("ggdendro", dependencies = TRUE)
        library(ggdendro)
}
if (!require("gridExtra")) {
        install.packages("gridExtra", dependencies = TRUE)
        library(gridExtra)
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

country.data <- covid[covid$total_cases>0 & covid$iso_code!="" & covid$iso_code!="OWID_WRL",] %>% 
        group_by(iso_code, location, population, population_density, median_age, aged_65_older,
                 aged_70_older, gdp_per_capita, extreme_poverty, cvd_death_rate, diabetes_prevalence,
                 female_smokers, male_smokers, handwashing_facilities, hospital_beds_per_100k) %>% 
        summarise(date=min(date),
                  total_cases=max(total_cases, na.rm=TRUE), 
                  max_new_cases=max(new_cases, na.rm=TRUE),
                  total_deaths=max(total_deaths, na.rm=TRUE),
                  max_new_deaths=max(new_deaths, na.rm=TRUE),
                  total_cases_per_million=max(total_cases_per_million, na.rm=TRUE),
                  new_cases_per_million=max(new_cases_per_million, na.rm=TRUE),
                  total_deaths_per_million=max(total_deaths_per_million, na.rm=TRUE),
                  new_deaths_per_million=max(new_deaths_per_million, na.rm=TRUE),
                  total_tests=max(total_tests, na.rm=TRUE),
                  new_tests=max(new_tests, na.rm=TRUE),
                  total_tests_per_thousand=max(total_tests_per_thousand, na.rm=TRUE),
                  new_tests_per_thousand=max(new_tests_per_thousand, na.rm=TRUE)
                  ) %>%
        ungroup()
country.data$days_since_1st_case <- Sys.Date() - country.data$date
country.data$iso_code <- as.character(country.data$iso_code)

cd <- country.data %>% 
        select(1:15, total_cases_per_million, -location, -date) %>% 
        mutate_all(~ifelse(is.infinite(.x), NA, .x)) %>%
        mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x)) %>%
        filter(population>=20000000)
rownames(cd) <- cd$iso_code
cd <- select(cd, -iso_code) 
        
##heatmap(as.matrix(scale(cd)))

hc <- hclust(dist(scale(cd)))
##plot(hc)

#convert cluster object to use with ggplot
dendr <- dendro_data(hc, type="rectangle") 

#your own labels (now rownames) are supplied in geom_text() and label=label
ggplot() + 
        geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
        geom_text(data=label(dendr), aes(x=x, y=y, label=label, hjust=0), size=3) +
        coord_flip() + scale_y_reverse(expand=c(0.2, 0)) + 
        theme(axis.line.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_rect(fill="white"),
              panel.grid=element_blank())

## SVD analysis
cd.svd <- svd(scale(cd))
plot(cd.svd$d^2/sum(cd.svd$d^2), pch = 19, xlab = "Singular vector", ylab = "Variance explained")