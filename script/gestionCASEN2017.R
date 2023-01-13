library(haven)
library(tidyverse)

casen2017 <- read_dta("D:/Proyectos_R/bbdd/casen2017.dta")

exprActualizado <- read_dta("D:/Proyectos_R/bbdd/exprActualizado.dta")

casen2017STGO <- casen2017 %>% filter(region==13)

casen2017STGO <- casen2017STGO %>% inner_join(exprActualizado)


casen2017STGO <- casen2017STGO %>% 
  select(edad,e6a,expr_C2017_NM) %>% 
  group_by(edad,e6a,expr_C2017_NM) %>% 
  count()

casen2017STGO <- casen2017STGO %>% 
  summarise(cantidad=expr_C2017_NM*n) %>% 
  group_by(edad,e6a) %>% 
  summarise(sum=sum(cantidad)) %>% 
  filter(edad>=15)

poblacionTotal <- sum(casen2017STGO$sum)
poblacionEscolarizada <- casen2017STGO %>% filter(e6a!=1 & e6a!=99)
poblacionEscolarizada <- sum(poblacionEscolarizada$sum)

tasaEscolaridadStgo <- (poblacionEscolarizada/poblacionTotal)*100
