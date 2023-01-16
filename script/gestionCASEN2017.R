library(haven)
library(tidyverse)

casen2017 <- read_dta("D:/Proyectos_R/bbdd/casen2017.dta")

# exprActualizado <- read_dta("D:/Proyectos_R/bbdd/exprActualizado.dta")
# casen2017 <- casen2017 %>% inner_join(exprActualizado)
# rm(exprActualizado)

########## TASA DE ESCOLARIDAD PAIS ############
 
tasaEscolaridad <- casen2017 %>% select(edad,e6a,expr_C2017_NM) %>% group_by(edad,e6a,expr_C2017_NM) %>% count()

tasaEscolaridad <- tasaEscolaridad %>% summarise(cantidad=expr_C2017_NM*n) %>% group_by(edad,e6a) %>% summarise(sum=sum(cantidad)) %>% filter(edad>=15)

poblacionTotal <- sum(tasaEscolaridad$sum)
poblacionEscolarizada <- tasaEscolaridad %>% filter(e6a!=1 & e6a!=99)
poblacionEscolarizada <- sum(poblacionEscolarizada$sum)

tasaEscolaridad <- (poblacionEscolarizada/poblacionTotal)*100
tasaEscolaridad <- round(tasaEscolaridad,2)

message("La tasa de escolaridad del país es de ",tasaEscolaridad,"%")

# ------------------------------------------------------------
# ------------------------------------------------------------
# ------------------------------------------------------------


########### TASA NETA DE ASISTENCIA POR NIVELES DE EDUCACION ##################

tasaNetaAsist <- casen2017 %>% 
  select(edad,e2,e3,e6a,expr_C2017_NM,region,qaut)

#####PARVULARIO

#Asistencia parvularia por region
asiste05 <- tasaNetaAsist %>% filter(edad<=5 & (e6a==2|e6a==3|e6a==4) & (e2 == 1|e3 == 1)) %>% group_by(region,expr_C2017_NM) %>% count()

asiste05 <- asiste05 %>% group_by(region) %>% summarise(cantidad=sum(expr_C2017_NM*n))

#Poblacion parvularia (menor o igual a 5 anios)
pob05 <- tasaNetaAsist %>% filter(edad <= 5) %>% group_by(expr_C2017_NM,region) %>% count()

pob05 <- pob05 %>% group_by(region) %>% summarise(cantidad=sum(expr_C2017_NM*n))

#Calculo tasa
TNAParvularia <- round(sum(asiste05$cantidad)/sum(pob05$cantidad) * 100, 1)


#####BASICA

#Asistencia nivel basica por region
asiste613 <- tasaNetaAsist %>% filter(edad > 5 & edad <= 13 & e6a == 7 & (e2 == 1|e3 == 1)) %>% group_by(region,expr_C2017_NM) %>% count()

asiste613 <- asiste613 %>% group_by(region) %>% summarise(cantidad=sum(expr_C2017_NM*n))

#Poblacion nivel basica (mayor a 5 anios y menor o igual a 13 anios)
pob613 <- tasaNetaAsist %>% filter(edad > 5 & edad <= 13) %>% group_by(expr_C2017_NM,region) %>% count()

pob613 <- pob613 %>% group_by(region) %>% summarise(cantidad=sum(expr_C2017_NM*n))

#Calculo tasa
TNABasica <- round(sum(asiste613$cantidad)/sum(pob613$cantidad) * 100, 1)


#####MEDIA

#Asistencia nivel medio por region
asiste1417 <- tasaNetaAsist %>% filter(edad >= 14 & edad <= 17 & (e6a == 9 | e6a == 11) & (e2 == 1|e3 == 1)) %>% group_by(region,expr_C2017_NM) %>% count()

asiste1417 <- asiste1417 %>% group_by(region) %>% summarise(cantidad=sum(expr_C2017_NM*n))

#Poblacion nivel medio (mayor a 13 anios y menor o igual a 17)
pob417 <- tasaNetaAsist %>% filter(edad >= 14 & edad <= 17) %>% group_by(expr_C2017_NM,region) %>% count()
pob417 <- pob417 %>% group_by(region) %>% summarise(cantidad=sum(expr_C2017_NM*n))

#Calculo tasa
TNAMedia <- round(sum(asiste1417$cantidad)/sum(pob417$cantidad) * 100, 1)


####SUPERIOR

#Asistencia nivel superior por region
asiste1824 <- tasaNetaAsist %>% filter(edad >= 18 & edad <= 24 & (e6a == 12|e6a==14) & (e2 == 1|e3 == 1)) %>% group_by(region,expr_C2017_NM) %>% count()

asiste1824 <- asiste1824 %>% group_by(region) %>% summarise(cantidad=sum(expr_C2017_NM*n))

#Poblacion nivel superior (mayor o igual a 18 anios y menor o igual a 24 anios)

pob1824 <- tasaNetaAsist %>% filter(edad >= 18 & edad <= 24) %>% group_by(expr_C2017_NM,region) %>% count()

pob1824 <- pob1824 %>% group_by(region) %>% summarise(cantidad=sum(expr_C2017_NM*n))

#Calculo tasa
TNASuperior <- round(sum(asiste1824$cantidad)/sum(pob1824$cantidad) * 100, 1)


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


####### DISTRIBUCION DE LA POBLACION DE 18 AÑOS O MÁS #############
####### POR NIVEL EDUCACIONAL ALCANZADO (TOTAL Y RANGO ETARIO) ############

distribNivelEduc <- casen2017 %>% select(edad,sexo,e6a,e6b)

distribNivelEduc <- distribNivelEduc %>% filter(edad>=18 & (e6a >=6 & e6a <= 15))

### DISTRIBUCION NIVEL BASICO
distriBasica <- distribNivelEduc %>% filter(e6a == 6 | e6a == 7) %>% mutate(termino = ifelse(e6a == 6 & e6b == 6, 1, 
                                                                                             ifelse(e6a==7 & e6b == 8, 1, 2)))

distriBasica <- distriBasica %>% group_by(termino) %>% count(termino)
distriBasica$nivel <- 1
### DISTRIBUCION NIVEL MEDIO
distriMedia <- distribNivelEduc %>% filter(e6a == 8 | e6a == 9 | e6a == 10 | e6a == 11) %>% mutate(termino = ifelse((e6a == 8 | e6a == 10) & e6b >= 6, 1, 
                                                                                                                   ifelse((e6a==9 | e6a ==11) & e6b >= 4,1,2)))

distriMedia <- distriMedia %>% group_by(termino) %>% count(termino)
distriMedia$nivel <- 2

### DISTRIBUCION NIVEL SUPERIOR

distriSuperior <- distribNivelEduc %>% filter(e6a == 12 | e6a == 13 | e6a == 14 | e6a == 15) %>% mutate(termino = ifelse(e6a==13,1,
                                                                                                                         ifelse(e6a==15,1,2)))

distriSuperior <- distriSuperior %>% group_by(termino) %>% count(termino)
distriSuperior$nivel <- 3

### Total alumnos igual o mayores de 18 anios dentro del sistema educativo (basica, media, superior)
distribucionTotal <- sum(distriBasica$n)+sum(distriMedia$n)+sum(distriSuperior$n)

### Union distribuciones de niveles individuales
distribucionNiveles <- rbind(distriBasica,distriMedia,distriSuperior)

# Remove df no utiles
rm(distriBasica,distriMedia,distriSuperior,distribNivelEduc)

# Calculo distribucion de poblacion =+ 18 por nivel

distribucionNiveles %>% mutate(porcentajeDistribucion = round((n/distribucionTotal)*100,1))

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


##ANIOS PROMEDIO ESCOLARIDAD DE LAS PERSONAS DE 15 AÑOS Y MAS


escolaridadPromedio <- casen2017 %>% 
  select (edad,sexo,r3,e6a,e6b)


escolaridadPromedio$aniosEscolaridad <- NA


escolaridadPromedio$aniosEscolaridad <-  
  with(escolaridadPromedio, 
    ifelse(edad>=15 & e6a == 1, 0, 
      ifelse(edad >= 15 & e6a >= 2 & e6a <= 5, 0, 
        ifelse(edad >= 15 & (e6a == 6 | e6a == 7), e6b, 
          ifelse(edad >= 15 & (e6a == 8 | e6a == 10), e6b + 6,
            ifelse(edad >= 15 & (e6a == 9 | e6a == 11), e6b + 8,
              ifelse(edad >= 15 & (e6a >= 12 & e6a <= 17), e6b + 12,
               ifelse(e6a == 99 | e6b == 99, NA, NA))))))))


escolaridadPromedio <- escolaridadPromedio %>% filter(aniosEscolaridad != 99 | !is.na(aniosEscolaridad))


escolaridadPromedioPais <- round(mean(escolaridadPromedio$aniosEscolaridad),1)

escolaridadPromedioJovenes <- escolaridadPromedio %>% filter(edad >= 15 & edad <= 29)
escolaridadPromedioJovenes <- round(mean(escolaridadPromedioJovenes$aniosEscolaridad),1)

escolaridadPromedioHombres <- escolaridadPromedio %>% filter(sexo==1)
escolaridadPromedioHombres <- round(mean(escolaridadPromedioHombres$aniosEscolaridad),1)

escolaridadPromedioMujeres <- escolaridadPromedio %>% filter(sexo==2)
escolaridadPromedioMujeres <- round(mean(escolaridadPromedioMujeres$aniosEscolaridad),1)

escolaridadPromedioPueblosInd <- escolaridadPromedio %>% filter(r3>=1 & r3 <=9)
escolaridadPromedioPueblosInd <- round(mean(escolaridadPromedioPueblosInd$aniosEscolaridad),1)




message("La escolaridad promedio del país son ",escolaridadPromedioPais," años")
message("La escolaridad promedio de los jóvenes entre 15 y 29 años son ",escolaridadPromedioJovenes," años")
message("La escolaridad promedio de los hombres son ",escolaridadPromedioHombres," años")
message("La escolaridad promedio de las mujeres son ",escolaridadPromedioMujeres," años")
message("La escolaridad promedio de las personas pertenecientes a un pueblo originario ",escolaridadPromedioPueblosInd," años")






























