library(haven)
library(tidyverse)

casen2017 <- read_dta("D:/Proyectos_R/bbdd/casen2017.dta")

exprActualizado <- read_dta("D:/Proyectos_R/bbdd/exprActualizado.dta")

casen2017 <- casen2017 %>% inner_join(exprActualizado)

##TASA DE ESCOLARIDAD PAIS

casen2017tasaEscolaridad <- casen2017 %>% 
  select(edad,e6a,expr_C2017_NM) %>% 
  group_by(edad,e6a,expr_C2017_NM) %>% 
  count()

casen2017tasaEscolaridad <- casen2017tasaEscolaridad %>% 
  summarise(cantidad=expr_C2017_NM*n) %>% 
  group_by(edad,e6a) %>% 
  summarise(sum=sum(cantidad)) %>% 
  filter(edad>=15)

poblacionTotal <- sum(casen2017tasaEscolaridad$sum)
poblacionEscolarizada <- casen2017tasaEscolaridad %>% filter(e6a!=1 & e6a!=99)
poblacionEscolarizada <- sum(poblacionEscolarizada$sum)

tasaEscolaridadStgo <- (poblacionEscolarizada/poblacionTotal)*100

## TASA NETA DE ASISTENCIA POR NIVELES DE EDUCACION








## DISTRIBUCION DE LA POBLACION DE 18 AÑOS O MÁS POR NIVEL EDUCACIONAL ALCANZADO (TOTAL Y RANGO ETARIO)






##ANIOS PROMEDIO ESCOLARIDAD DE LAS PERSONAS DE 15 AÑOS Y MAS


tablaCondicional <- casen2017 %>% 
  select (edad,e6a,e6b,expr_C2017_NM)
      

tablaCondicional$aniosEscolaridad <- NA


tablaCondicional$aniosEscolaridad <-  
  with(tablaCondicional, 
       ifelse(e6a == 99, 99,
              ifelse(is.na(e6b), 99,
                     ifelse(e6b == 99, 99,
                            ifelse(e6a < 5, e6b, 
                                   ifelse(e6a == 5 | e6a == 6 | e6a == 7, e6b,
                                          ifelse(e6a == 8 | e6a == 10, 6 + e6b,
                                                 ifelse(e6a == 9 | e6a == 11, 8 + e6b,
                                                        ifelse(e6a >= 12 & e6a <= 15, 12 + e6b,
                                                               ifelse(e6a == 16 | e6a == 17, 19 + e6b, e6b))))))))))


tablaCodicionalFiltrada <- tablaCondicional %>% filter(aniosEscolaridad!=99)
tablaCondicional15a29 <- tablaCodicionalFiltrada %>% filter(edad>14 & edad<30)

sum(tablaCodicionalFiltrada$aniosEscolaridad)/193137
sum(tablaCondicional15a29$aniosEscolaridad)/48109

