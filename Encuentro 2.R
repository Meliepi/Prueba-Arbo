#Breve apunte sobre librerias y paquetes----
#librerias: ya hay paquetes y librerias "preinstaladas"
#hay mas de 18mil 
#PAQUETES: colecciones de funciones, datos y código compilado de R. Se usan tareas específicas,
#como visualización geoespacial, gráficos, análisis psicométricos, minería de datos, etc

#LIBRERIAS:directorios donde R guarda los paquetes
#ustedes van a tener que instalar algunos paquetes para luego instalar las librerias


#activacion de librerias----
#lxs que nunca hayan usado R, van a tener que instalar los paquetes en principio

install.packages("readr")
install.packages("dplyr")


library(tidyverse)#paquetes que incluye a dplyr, tidyr, ggplot2, readr, purr, tibble, stringr y forcat. Muy, muy utilizado
?tidyverse
install.packages("tidyverse")

library(dplyr) #para manipular y transformar datos. Permite prepararlos para su análisis estadístico.
#por ej: select, filter, mutate, arrange, count, group_by
?dplyr

library(readxl) #para importar .xlxs
install.packages("readx1")

library(readr)#para importar .csv


library(ggplot2)#para graficar. 
?ggplot2
install.packages("ggplot2")

#al escribir library(), se importan las funciones contenidas en el paquete al entorno de trabajo actual.

#IMPORTANTE: hay que correr las library() cada que se inicie una sesión en R.
#esto se debe a que aunque ya hayas importado las funciones de un paquete con anterioridad, las sesiones de R se inician “limpias”.


#carga de base de datos
#imp!: el camino más directo es tener el dataset en la misma carpeta del .Rproj

#nombre <- read.csv2("nombre.csv", sep = ";")


arbovirus <- read.csv2("ARBO_NEUQUEN.csv", sep = ";")
#¿cuántas observaciones tiene y cuantas variables? ¿Cuál es su formato?
nrow(arbovirus)
ncol(arbovirus)
class(arbovirus)

#DATAFRAME: estructura de datos similar a una tabla. Los datos están organizados en filas y columnas. 
#arbovirus <- as.dataframe(arbovirus) esto es para cambiar a dataframe

#Carguemos la base como un xlsx

#Miremos como está compuesta nuestra tabla
#str es para ver como esta conformada la base las llavecitas son para hacer bloques, para juntar

{str(arbovirus)
  
  #hago unas tablas para ver eventos que tiene
  #funcion: table
  table(arbovirus$EVENTO)
  
}

#Preguntas a mi set de datos-----
#a)como es la distribucion temporal de las notificaciones de arbovirus? histograma usamos SE y AÑO
#b)como es la distribucion temporal de las notificaciones de los eventos que componen el grupo?
#c)distribucion por sx
#d)distribuc por edad
#e)distribucion por region
#f)cuales son los sintomas más frecuentes
#g)internacion 

#filtros por prov de residencia---- es importante que siempre que tengo una flecha de asiggnacion y una pipe tengo que tener un objeto desde el cual se 

#uso de la funcion filter. ¿Por cuál columna vamos a filtar? 

arbovirus <- arbovirus %>% #pipe (se lee paip) o tuberias
  filter(ID_PROV_INDEC_RESIDENCIA==58)


  
#SE min---- (para construirla!!!!!)
#ya sabemos que la SE_FIS no siempre está completa asique se arma semana minima
arbovirus <- arbovirus %>%
  mutate(SE_MIN = pmin(SEPI_SINTOMA, SEPI_CONSULTA, SEPI_MUESTRA, SEPI_APERTURA,
                       na.rm = TRUE))

#na.rm = TRUE le pide que si encuentra un na (NA) lo excluya, si le pusieramos na.rm = false lo trae 
#hagamos lo mismo con el año
arbovirus <- arbovirus %>% 
  mutate(ANIO_MIN2 = pmin(ANIO_EPI_SINTOMA, ANIO_EPI_CONSULTA, ANIO_EPI_MUESTRA, ANIO_EPI_APERTURA,
         na.rm= TRUE))


#hagamos lo mismo con la variable AÑO



#na.rm = remove NA Si mis valores ausentes decido ignorarlos, pongo TRUE



#Bueno vamo a grafica----
{
  #a) Distribucion temporal de las notificaciones de arbo /// group_by es para armar tablas, se ponen las variables que quiero entre parentesis
  
  arbovirus_evolutivo <- arbovirus %>% 
    group_by(ANIO_MIN2, SE_MIN, SEXO, CLASIFICACION_MANUAL) %>% 
    summarise(casos = n(), .groups = "drop") %>% 
    mutate(ANIO_SE= paste(ANIO_MIN2, SE_MIN, sep = "-")) %>% 
    arrange(ANIO_MIN2,SE_MIN) %>% 
    mutate(ANIO_SE=factor(ANIO_SE,levels=unique(ANIO_SE))) %>% 
    as.data.frame()
  
  

  
  
  #Grafico
  #estructura de los graficos (como lo voy a llamar al grafico, flecha de asignacion y lugar de donde salgan los datos
  #nombre de mi grafico <- nombre de mis datos %>% 
  #ggplot(aes(x=, y=)) +
  #geom_bar(stat="identity") #le estoy diciendo que no sume ni calcule nada, yo ya calcule los valores previamente
  
  
  #armemos el grafico entre todxs considerar que el nombre de los ejes va en minuscula
  class(arbovirus_evolutivo)
arbovirus_evolutivo <- as.data.frame(arbovirus_evolutivo)
class(arbovirus_evolutivo)  
  
  histograma_1 <- arbovirus_evolutivo %>% 
  ggplot(aes(x=ANIO_SE,y=casos))+
  geom_bar(stat="identity")

  histograma_1
  print(histograma_1)



#cual es la salida de la consola? 
  
  #que tal los formatos de mis columnas?
  #puedo ordenar como una variable continua una variable con formato caracter?
  
  
  
  #metemos un poco más de mano
  #propuesta completisima   
  arbovirus_grafico_evolutivo <-  arbovirus_evolutivo %>%  
    ggplot(aes(x=ANIO_SE, y=casos)) +
    geom_bar(stat = "identity") + 
    scale_x_discrete(
      breaks = levels(factor(arbovirus_evolutivo$ANIO_SE))[seq(1, length(levels(factor(arbovirus_evolutivo$ANIO_SE))), by = 5)],
      expand = c(0, 0))
  arbovirus_grafico_evolutivo
  
  
  # me muestra las semanas del grafico desordenadas dado que no lo ordeno, toma la variable como 80 por ej en vez de se 8. para cambiar y que sea factor se hace lo sigueinte
  arbovirus_evolutivo$ANIO_SE <- as.factor(arbovirus_evolutivo$ANIO_SE)
  
  arbovirus_grafico_evolutivo <-  arbovirus_evolutivo %>%
  ggplot(aes(x=ANIO_SE, y=casos)) +
    geom_bar(stat = "identity") + 
    scale_x_discrete(
      breaks = levels(factor(arbovirus_evolutivo$ANIO_SE))[seq(1, length(levels(factor(arbovirus_evolutivo$ANIO_SE))), by = 5)],
      expand = c(0, 0))
  arbovirus_grafico_evolutivo
  
  class(arbovirus_evolutivo)
  
  
  #ES IMPORTANTE PONER DE DONDE SACA LOS DATOS!!!!arbovirus_grafico_evolutivo <-  arbovirus_evolutivo %>% en este caso
  
   #los colores en R y los temas
  
  
  
  
  
  
  
  
  
  #propuesta completisima   
  arbovirus_grafico_evolutivo <-  arbovirus_evolutivo %>%  
    ggplot(aes(x=ANIO_SE, y=casos)) +
    geom_bar(stat = "identity", fill="purple", color="blue") + #fill= color de relleno #color=color de linea
    scale_x_discrete(
      breaks = levels(factor(arbovirus_evolutivo$ANIO_SE))[seq(1, length(levels(factor(arbovirus_evolutivo$ANIO_SE))), by = 5)],
      expand = c(0, 0)) +
    theme_light()+
    labs(
      y="Notificaciones de arbovirus",
      x= "Año-SE")
  
  
  arbovirus_grafico_evolutivo 
  
  
  
  #bizarreando
  install.packages("ggimage")
  library(ggimage)
  
  
  arbovirus_grafico_evolutivo_2 <- arbovirus_evolutivo %>%
    ggplot(aes(x = ANIO_SE, y = casos)) +
    geom_image(aes(image = C:/Users/Epidemio/Documents/R para Resis/R-residencia/Taller 2025), size = 0.1) +  
    scale_x_discrete(
      breaks = levels(factor(arbovirus_evolutivo$ANIO_SE))[seq(1, length(levels(factor(arbovirus_evolutivo$ANIO_SE))), by = 5)],
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(0, max(arbovirus_evolutivo$casos, na.rm = TRUE), by = 50),
      expand = c(0, 0)
    ) +
    theme_minimal()
  
  arbovirus_grafico_evolutivo_2
  
  
}
