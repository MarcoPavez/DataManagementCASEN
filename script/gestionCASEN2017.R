library(haven)
library(tidyverse)

casen2017 <- read_dta("D:/Proyectos_R/bbdd/casen2017.dta")

# exprActualizado <- read_dta("D:/Proyectos_R/bbdd/exprActualizado.dta")

casen2017 <- casen2017 %>% inner_join(exprActualizado)

# rm(exprActualizado)

##TASA DE ESCOLARIDAD PAIS

tasaEscolaridad <- casen2017 %>% 
  select(edad,e6a,expr_C2017_NM) %>% 
  group_by(edad,e6a,expr_C2017_NM) %>% 
  count()

tasaEscolaridad <- tasaEscolaridad %>% 
  summarise(cantidad=expr_C2017_NM*n) %>% 
  group_by(edad,e6a) %>% 
  summarise(sum=sum(cantidad)) %>% 
  filter(edad>=15)

poblacionTotal <- sum(tasaEscolaridad$sum)
poblacionEscolarizada <- tasaEscolaridad %>% filter(e6a!=1 & e6a!=99)
poblacionEscolarizada <- sum(poblacionEscolarizada$sum)

tasaEscolaridad <- (poblacionEscolarizada/poblacionTotal)*100
tasaEscolaridad <- round(tasaEscolaridad,2)

message("La tasa de escolaridad del país es de ",tasaEscolaridad,"%")

## TASA NETA DE ASISTENCIA POR NIVELES DE EDUCACION








## DISTRIBUCION DE LA POBLACION DE 18 AÑOS O MÁS POR NIVEL EDUCACIONAL ALCANZADO (TOTAL Y RANGO ETARIO)






##ANIOS PROMEDIO ESCOLARIDAD DE LAS PERSONAS DE 15 AÑOS Y MAS


tablaCondicional <- casen2017 %>% 
  select (edad,sexo,r3,e6a,e6b,expr_C2017_NM)
      

tablaCondicional$aniosEscolaridad <- NA


tablaCondicional$aniosEscolaridad <-  
  with(tablaCondicional, 
       ifelse(edad>=15 & e6a == 1, 0, 
              ifelse(edad >= 15 & e6a >= 2 & e6a <= 5, 0, 
                     ifelse(edad >= 15 & (e6a == 6 | e6a == 7), e6b, 
                            ifelse(edad >= 15 & (e6a == 8 | e6a == 10), e6b + 6,
                                   ifelse(edad >= 15 & (e6a == 9 | e6a == 11), e6b + 8,
                                          ifelse(edad >= 15 & (e6a >= 12 & e6a <= 17), e6b + 12,
                                                 ifelse(e6a == 99 | e6b == 99, NA, NA))))))))
          


tablaCondicionalFiltrada <- tablaCondicional %>% filter(aniosEscolaridad!=99 | !is.na(aniosEscolaridad))

tablaCondicional15a29 <- tablaCondicionalFiltrada %>% filter(edad>14 & edad<30)


aniosEscolaridadHombres <- tablaCondicionalFiltrada %>% filter(sexo==1)
aniosEscolaridadHombres <- round(sum(aniosEscolaridadHombres$aniosEscolaridad, na.rm = T)/nrow(aniosEscolaridadHombres),1)
aniosEscolaridadMujeres <- tablaCondicionalFiltrada %>% filter(sexo==2)
aniosEscolaridadMujeres <- round(sum(aniosEscolaridadMujeres$aniosEscolaridad, na.rm = T)/nrow(aniosEscolaridadMujeres),1)


aniosPromedioPais <- round(sum(tablaCondicionalFiltrada$aniosEscolaridad)/nrow(tablaCondicionalFiltrada),1)
aniosPromedioJovenes <- round(sum(tablaCondicional15a29$aniosEscolaridad)/nrow(tablaCondicional15a29),1)


message("Los años promedio de escolaridad del país son ",aniosPromedioPais," años")
message("Los años promedio de escolaridad de los jóvenes entre 15 y 29 años son ",aniosPromedioJovenes," años")

tablaCon





























