library(haven)
library(tidyverse)

casen2017 <- read_dta("bbdd/casen2017/Casen 2017.dta")

casen2017STGO <- casen2017 %>% filter(region==13)

casen2017STGO <- casen2017STGO %>% 
  select(edad,e6a,expr) %>% 
  group_by(edad,e6a,expr) %>% 
  count()

casen2017STGO <- casen2017STGO %>% 
  summarise(cantidad=expr*n) %>% 
  group_by(edad,e6a) %>% 
  summarise(sum=sum(cantidad))
