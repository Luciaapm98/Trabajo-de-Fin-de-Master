# *******************************************************************************
#                     INSTALAMOS LOS PAQUETES NECESARIOS
# *******************************************************************************

# Borammos todo
rm(list = ls())
cat("\14") #borra la consola
# Lectura de los datos
library(readr)
# Depuracion y preprocesamiento
library(tidyverse)
# Resumen numerico
library(skimr)
# Leer archivos de sav (SPSS)
library(haven)
library(foreign)
# Leer archivos csv/excel..
library(readr)
# Codigo de SQL en R
library(sqldf)
#Para añadir fuentes tipográficas:
library(sysfonts)
library(showtextdb)
library(showtext)
font_add(family = "cotton", # Nombre que quieras usar
         regular = "Cotton Butter.ttf") # Texto de la pestana 'General' mas la extension
font_add(family = "storytime", # Nombre que quieras usar
         regular = "storytime.ttf") # Texto de la pestana 'General' mas la extension
font_add_google("Lobster Two", "lobstertwo")
font_add_google("Poppins", "poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()
library(NatParksPalettes)



# ******************************************************************************
#                         TEMA DE LOS GRAFICOS
# ******************************************************************************

library(MetBrewer)
# Tema: vacío de inicio
theme_set(theme_bw(base_family = "poppins"))

#Configuramos tema
theme_update(
  # Fuentes de los textos
  text = element_text(size = 9, color = "black"),
  # Fuentes y ajustes de título, subtítulo y caption
  plot.title = element_text(family = "lobstertwo", size = 23,
                            face = "bold", color = "#2a475e"),
  line = element_line(color = "black", size = .5,
                      linetype = 1, lineend = "butt"),
  rect = element_rect(fill = "white", color = "black",
                      size = .5, linetype = 1),
  panel.background = element_rect(fill = "white", color = NA),
  panel.border = element_rect(color = "gray30",
                              fill = NA, size = .7),
  panel.grid.major = element_line(color = "gray90", size = 1),
  panel.grid.minor = element_line(color = "gray90", size = .5,
                                  linetype = "dashed"),
  panel.spacing = unit(12, "pt"),
  panel.spacing.x = NULL,
  panel.spacing.y = NULL,
  panel.ontop = FALSE,
  # Fuentes y ajustes de los ejes
  axis.text = element_text(size = 13, color = "black"),
  axis.title.y = element_text(size = 16, angle = 90,  family = "Roboto Mono"),
  axis.title.x = element_text(size = 16,  family = "Roboto Mono"),
  axis.line = element_line(colour = "grey50"),
  # Fondo
  plot.background =
    element_rect(fill = "white", color = "#fbf9f4"))



# *****************************************************************************
#                    CARGAMOS LOS DATOS
# *****************************************************************************


Sharedata <- read_sav("./sharew8ca_rel8-0-0_ca.sav")


# *****************************************************************************
#              ELIMINAMOS VARIABLES QUE CONSIDERAMOS POCO REPRESENTATIVAS
# *****************************************************************************

Sharedata <- Sharedata %>% select(-c(caph089_2,  cah011_1, cah011_2, 
                           cah011_3, cah007_4, cah011_4,  cac003_1, cac003_2,
                           cac003_3, cac003_3b, cac003_4b, cac003_5b, cac003_5,
                           cac003_5b, cac003_6, cac003_6b, cac003_7, cac003_7b, 
                           cac003_8, cac003_8b, cac003_97, cac003_97b, cac003_4,
                           cac005_3b, cac005_4b, cac005_5b, cac005_6b, cac005_7b,
                           cac005_8b, cac005_97b, cac005_8,
                           cac007_, cac008_1, cac008_2, cac008_3, cac008_3b, 
                           cac008_4, cac008_4b, cac008_5, cac008_5b, cac008_6, 
                           cac008_6b, cac008_7, cac008_7b, cac008_8, cac008_8b,
                           cac008_97, cac008_97b, cac011_3b, cac011_4b, cac011_5b,
                           cac011_6b, cac011_7b, cac011_8b, cac011_8, cac011_97b,
                           cac014_3b, cac014_4b, cac014_5b, cac014_6b, cac014_7b,
                           cac014_8b, cac014_97b, cac014_8,
                           caq006_1, caq006_2, caq006_3, 
                           caq006_4, caq006_97, caq011_1, caq011_2, caq015_,
                           caq011_3, caq011_4, caq011_97, caq016_1, caq016_2, 
                           caq016_3, caq016_4, caq016_97, caq027_, caq028_1, 
                           caq028_2, caq028_3, caq028_4, caq028_5, caq028_97, 
                           caq020_, caq021_, caq022_, caq023_1, caq023_2, caq023_3, 
                           caq023_4, caq023_5, caq023_97, caw003_, caw012_, 
                           caw013_, caw022_, caw020_ ,caw023_1, caw023_2, caw025_, 
                           caw026_1, caw026_2, cae004_1, cae004_2, cae004_3, 
                           cae004_4, cae004_97, cas011_1, cas011_2, cas011_3, 
                           cas011_4, cas016_, cas027_1, cas027_2, cas027_3, 
                           cas013_1, cas013_2,  cas013_3, cas013_4, cas028_, 
                           cas021_1, cas021_2, cas021_3, cas021_4,
                           caf001_, caf002_, caf005_, mergeidp8, language_ca))


# ******************************************************************************
#                       UNION DE OTRA PARTE DE LA ENCUESTA
# ******************************************************************************

# Dentro de la encuesta ShareData se incluye, ademas del dataset utilizado para
# este trabajo, una base de datos con datos que no varian de encuesta a encuesta
# sobre los encuestados: numero de hijos, numero de personas que viven en su casa,
# que relacion tienen dichas personas con el encuestado.....

# ............................Cargamos dicha base de datos......................

# Elegimos el fichero que queremos
file.choose()

# Leemos el fichero tipo spss
Sharedata_fix <- read.spss( "C:\\Users\\lucia\\OneDrive\\Documentos\\Universidad Complutense\\TFM\\TFM\\sharew8_rel1-0-0_cv_r.sav",
                            to.data.frame=TRUE)

# .......................Nos quedamos con las variables que nos interesan.......

# De entre todas las variables de las que dispone este conjunto de datos, nos 
# quedamos con el ID del encuestado, el tamaño de la casa 
# y el mes en el que se le realizó la encuesta al encuestado.

Sharedata_fix <- Sharedata_fix %>% select(c(mergeid,hhsize_update_ca,
                                            int_month_ca, partnerinhh_update_ca))

# Unimos esta base de datos a la base de datos de nuestro trabajo. Lo realizamos
# mediante un "left join" de manera que, aquellos encuestados que no aparezcan 
# en el fichero Sharedata_fix en vez de ser borrados en nuestro fichero (sharedata)
# se mantendran con na's en las variables del fichero Sharedata_fix


Sharedata <- merge(Sharedata,Sharedata_fix,by = "mergeid", all= FALSE, 
                   all.x=TRUE, all.y=FALSE)


# Con el objetivo de poder unir este conjunto de datos con otro con datos del 
# covid por mes, necesitamos que la variable int_month_ca (que indica el mes en
# el que se encuesto a cada persona) no tenga valores nulos, puesto que en caso 
# de que los tenga al unir ambos conjuntos de datos aunque el conjunto de datos 
# del covid no tenga nulos en dicha observacion, si la variable int_moth_ca 
# tiene ausentes en tal observación, los datos del covid tambien seran ausentes.

# Por ello, necesitamos imputar los ausentes de la variable int_month_ca. Como 
# para cada pais las encuestas se relizan a lo largo de tres meses, imputaremos
# los ausentes por el mes en el que haya habido mas encuestados por pais. Para
# ello, comenzamos creando una funcion que nos extraiga la moda:

getmode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(table(match(v, uniqv)))]
}

# Aplicamos la funcion creando una nueva variable en nuestro conjunto de datos 
# llamado "Mode" con la moda de la variable int_month_ca por pais de manera que 
# para cada pais extraemos el mes en el que mas encuestas se han realizado

Sharedata <- Sharedata %>%
  group_by(country) %>%
  mutate(Mode = getmode(int_month_ca))


# Una vez tenemos esa variable imputamos los ausentes de int_month_ca por 
# la variable Mode que acabamos de crear, es decir, por el mes en el que se 
# hayan realizado mas encuestas por pais. 
# La funcion coalesce imputa el ausente en una observacion de la variable
# int_month_ca por el valor que tome la variable Mode para dicha observacion.

Sharedata <- Sharedata %>% ungroup() %>%
  mutate(int_month_ca = coalesce(int_month_ca, Mode))


# Eliminamos la variable Mode que hemos creado para imputar los ausentes

Sharedata <- Sharedata %>% select(-c(Mode))


# ::::::::::::::::::::: SHARE WAVE 8 :::::::::::::::::::::::::::::::::::::::::::

# A parte de la encuesta que vamos a utilizar para este TFM, Share realizo una 
# encuesta con preguntas similares a los de los otros años anteriores a la 
# pandemia, en la cual pregunta a los encuestados muchos mas temas a parte de 
# lo que trataremos en esta encuesta. En concreto, nos centraremos en dos temas
# principales que, por la naturaleza de las preguntas, creemos que pueden tener
# relacion con la variable objetivo. 

# En primer lugar, hay un modulo llamado ACTIVITIES en el que se pregunta a los 
# encuestados sobre sus sentimientos hacia varios aspectos de su vida y sobre 
# la cantidad de actividades que realizan.
# Seleccionamos las variables que nos interesan (las cuales estudiaremos mas 
# adelante):

Share_activities <- read_dta(file = "C:/Users/lucia/Downloads/sharew8_rel8-0-0_ac.dta")
Share_activities <- Share_activities %>% select(mergeid, ac012_, ac016_, ac020_, 
                                                ac021_, ac022_, ac023_, ac024_,
                                                ac025_)

# REnombramos las variables
Share_activities <-  dplyr::rename(Share_activities, satisfactionlife = ac012_,
                            leftout = ac016_,
                            lookforwardeachday = ac020_,
                            lifemeaning = ac021_,
                            backhapp = ac022_,
                            fullenergy = ac023_,
                            fulloport = ac024_,
                            futuregood = ac025_
                            )

# Unimos estas variables a nuestro conjunto de datos mediante un "left join" por
# la variable "mergeID", un ID unico para cada encuestado. No para todos los 
# encuestados se realizaron ambas encuestas asi que, para aquellos que solo 
# aparecen en la encuesta del corona se asignara un NA en estas variables.

Sharedata <- left_join(Sharedata,Share_activities,by = "mergeid")


# Por otro lado, existe otro modulo llamado BEHAVIOURAL RISKS en el que se 
# pregunta a los encuestados por temas relacionados con el alcohol o fumar
# Seleccionamos las variables que nos interesan.

Share_behavioural <- read_dta(file = "C:/Users/lucia/Downloads/sharew8_rel8-0-0_br.dta")
Share_behavioural <- Share_behavioural %>% select(mergeid, 
                                                  br001_, br003_,br039_, br623_)


# Renombramos las variables
Share_behavioural <-  dplyr::rename(Share_behavioural, eversmokedaily = br001_,
                             yearsmoked = br003_,
                             drinklastsevendays = br039_,
                             sixormoredrinks = br623_
)


# La variable br003_ indica cuantos años el encuestado ha fumado, para aquellos
# que no han fumado nunca se les asigna un NA. Convertimos estos NAs en valores 
# validos para que, al hacer la union con nuestra encuesta, no se confundan estos
# NAs con los resultantes de la union.

# Imputamos dichos NAs por cero (cero años han fumado) teniendo en cuenta que a 
# aquellas personas que han fumado menos de un año se le asigna un uno.

Share_behavioural %>% dplyr::count(eversmokedaily)
Share_behavioural %>% dplyr::count(is.na(yearsmoked))


Share_behavioural <- Share_behavioural %>% mutate(yearsmoked = ifelse(is.na(yearsmoked),
                                                                0,yearsmoked))

# Unimos estas variables a nuestra encuesta:

Sharedata <- left_join(Sharedata,Share_behavioural,by = "mergeid")



# ******************************************************************************
#              UNION OXFORD GOVERMENT RESPONSE TRACKER
# ******************************************************************************

# Este dataset contiene informacion sobre las medidas de los gobiernos contra el
# covid alrededor del mundo. Se han extraido respuestas políticas desde el 1 de 
# Enero de 2020 en alrededor de 180 paises y se llevan a cabo a traves de 23 
# indicadores. 

# Importamos el conjunto de datos

covid <- read_csv("./OxCGRT_withnotes_2020.csv")


# De entre todas variables de las que dispone el conjunto de datos (indicadores
# e indices) nos quedaremos con tres indices: Indice de la respuesta general del 
# gobierno, indice de la salud y las restricciones, Indice de ayudas economicas
# e indice de severidad. Además, incluiremos el número de casos confirmados y
# el número de muertes por covid.

covid <- covid %>% select(CountryName, Date, ConfirmedCases, ConfirmedDeaths, 
                          GovernmentResponseIndex, ContainmentHealthIndex, 
                          StringencyIndex, EconomicSupportIndex)

# Ponemos las variables en minúscula

covid <- covid %>% rename_all(tolower)

# ::::::::::::::: PREPROCESAMIENTO DE LA BASE DE DATOS ::::::::::::::::::::::::

# La base de datos anterior dispone de una fecha (añomesdia) de la cual queremos 
# extraer el mes con el objetivo de poder asignar cada una de las variables 
# de este conjunto de datos a cada una  de las personas encuestadas por pais 
# según el mes en el que se les realizo la encuesta (recordemos que hemos 
# incluido el mes en el que se realizo la encuesta a nuestro conjunto de datos).

# Para extraer el mes de la fecha utilizaremos codigo de SQL implementado en R
# mediante el paquete SQLDF. 


# Lo primero que realizamos es seleccionar los diferentes valores de las varia-
# bles nombre del pais y fecha. A continuacion extraemos en una variable llamada
# "Month" el mes (desde la posicion 5 de la variable "date" nos quedamos con los
# dos siguientes caracteres). A continuación, con el objetivo de poder segun el
# mes esta base de datos con nuestra encuesta creamos una nueva variable llamada
# "month text" con el nombre de los meses en vez del numero (solo para junio, 
# julio, agosto y septiembre; meses en los que se realizo la encuestra). 


covid <-sqldf(
  "   SELECT DISTINCT countryname, date,
                      SUBSTRING(Date,5,2) AS month,
                      CASE WHEN SUBSTRING(Date,5,2) = '05' THEN 'May'
                           WHEN SUBSTRING(Date,5,2) = '06' THEN 'June'
                           WHEN SUBSTRING(Date,5,2) = '07' THEN 'July'
                           WHEN SUBSTRING(Date,5,2) = '08' THEN 'August'
                           WHEN SUBSTRING(Date,5,2) = '09' THEN 'September'
                           ELSE NULL END AS monthtext,
                      stringencyindex, governmentresponseindex, 
                      economicsupportindex, confirmedcases, confirmeddeaths,
                      containmenthealthindex
                      
      FROM covid
      "
)


# Para cada uno de los dias de cada mes tenemos un valor distinto de los indices
# y de los casos y muertes por covid confirmadas. Como queremos asignar un unico
# valor a cada mes puesto que en nuestra encuesta tenemos tan solo la informacion
# del mes en el que se encuesto a cada persona y no del dia, vamos a extraer el 
# promedio de los indices por mes (para cada uno de los meses obtenemos la media
# de todos los dias de dicho mes) y el maximo de los casos confirmados y muertes
# confirmadas por mes. Agrupamos estos valores, además, por país.


covid <- sqldf(
  "
      SELECT  CountryName,MonthText,
              AVG(StringencyIndex)         AS StringencyIndex ,
              AVG(GovernmentResponseIndex) AS GovernmentResponseIndex,
              AVG(EconomicSupportIndex)    AS EconomicSupportIndex, 
              AVG(ContainmentHealthIndex)  AS ContainmentHealthIndex,
              MAX(ConfirmedDeaths)         AS ConfirmedDeaths,
              MAX(ConfirmedCases)          AS ConfirmedCases
      FROM covid
      WHERE MonthText IS NOT NULL
      GROUP BY CountryName,MonthText
  "
)


# ::::::::::::::: CONVERTIMOS LOS CODIGOS DE LOS PAISES DE LA ENCUESTA :::::::::
# ::::::::::::::::::::: EN NOMBRES :::::::::::::::::::::::::::::::::::::::::::::


# Para poder unir los datos del Covid a nuestra encuesta por País, debemos modi-
# ficar los codigos de los paises que aparecen en nuestra encuesta (variable 
# Country) por el nombre del pais correspondiente a dicho codigo (el nombre de
# dicho pais debe estar escrito de manera analoga al nombre que aparece en el 
# dataset del covid para que la union se realice correctamente).


# Creamos una funcion que modifique los codigos por los nombres de los paises

COuntry_Name <- function(country) {
  dplyr::case_when((country == 12) ~ "Germany", 
                   (country == 13) ~ "Sweden",
                   (country == 14) ~ "Netherlands",
                   (country == 15) ~ "Spain",
                   (country == 16) ~ "Italy",
                   (country == 17) ~ "France",
                   (country == 18) ~ "Denmark",
                   (country == 19) ~ "Greece",
                   (country == 20) ~ "Switzerland",
                   (country == 23) ~ "Belgium",
                   (country == 25) ~ "Israel",
                   (country == 28) ~ "Czech Republic",
                   (country == 29) ~ "Poland",
                   (country == 31) ~ "Luxembourg",
                   (country == 32) ~ "Hungary",
                   (country == 33) ~ "Portugal",
                   (country == 34) ~ "Slovenia",
                   (country == 35) ~ "Estonia",
                   (country == 47) ~ "Croatia",
                   (country == 48) ~ "Lithuania",
                   (country == 51) ~ "Bulgaria",
                   (country == 53) ~ "Cyprus",
                   (country == 55) ~ "Finland",
                   (country == 57) ~ "Latvia",
                   (country == 59) ~ "Malta",
                   (country == 61) ~ "Romania",
                   TRUE ~ "Slovak Republic")
}

Sharedata <- Sharedata %>% mutate(country= COuntry_Name(country))


# :::::::::UNA VEZ TENEMOS LOS NOMBRES UNIMOS LOS DOS CONJUNTOS DE DATOS::::::::


Sharedata     <-left_join(Sharedata, covid, by = c("int_month_ca" = "monthtext",
                                                     "country" = "countryname"))




# ******************************************************************************
#                          PREGUNTAS FINANCIERAS
# ******************************************************************************

# .................. variable cae0001_ .........................................

# Esta variable indica si el encuestado es el encargado de responder a las 
# las preguntas financieras de la vivienda


# Renombramos las variables


Sharedata <-  dplyr::rename(Sharedata, financialrespondent=cae001_)



Sharedata %>% dplyr::count(financialrespondent) %>% mutate(prop = n / sum(n)*100)


# Para todas las preguntas financieras de una vivienda, solo responde, en caso de
# que mas de una persona en la misma vivienda sea encuestada, uno de ellos. 
# Al resto de encuestados de la misma vivienda se les pone un "-999999 -> no 
# APlicable". Puesto que para los encuestados de la misma vivienda las respuestas
# a estas preguntas son las mismas, vamos a imputar todos los "no Aplicables" por 
# el valor de la persona respondable de responder a esta pregunta; es decir, 
# si en una pareja que convive en la misma, solo responde el marido a estas 
# preguntas y la mujer tiene como respuesta "no Aplicable" vamos a imputar
# las respuestas del marido a la mujer. 



# Para ello primero vamos a crear un dataset con los responsables de responder
# a estas preguntas, es decir, a los que han contestado que si a la pregunta 
# cae001_. Este dataset contendra el id del encuestado, el id de la vivienda en
# la que reside el encuestado (variable hhid) y las preguntas economicas de esta
# seccion

# Renombramos las preguntas financieras

Sharedata <-  dplyr::rename(Sharedata, incomebeforecorona=cahh017e,
                     financialsupport = cae003_,
                     lowestincomesincecorona= cae005e,
                     makeendsmeet = caco007_,
                     postponepayment = cae011_,
                     dipsavings = cae012_)

Sharedata_union <- Sharedata %>% filter(financialrespondent == 1) %>% select(
  mergeid, hhid8,coupleid8, incomebeforecorona, financialsupport, lowestincomesincecorona, 
  makeendsmeet, postponepayment, dipsavings, hhsize_update_ca)

# Creamos un bucle en la que si el id de la vivienda (household ID: hhid8) 
# de nuestro conjunto de datos se encuenta en la base de datos que acabamos de
# crear (Sharedata_union), entonces reemplazamos el valor de las variables 
# economicas oroginales por el que aparece en el conjunto de union, es decir, 
# reemplazamos los "No aplicables" de nuestro conjunto de datos por los datos 
# de la persona de la vivienda responsable de responder estas preguntas (datos 
# guardados en el conjunto Sharedata_union)


for (i in Sharedata_union$hhid8){
  Sharedata$incomebeforecorona =  ifelse(Sharedata$hhid8 == i, 
                               Sharedata_union$incomebeforecorona, Sharedata$incomebeforecorona)
  Sharedata$financialsupport =  ifelse(Sharedata$hhid8 == i, 
                               Sharedata_union$financialsupport, Sharedata$financialsupport)
  Sharedata$lowestincomesincecorona =  ifelse(Sharedata$hhid8 == i, 
                              Sharedata_union$lowestincomesincecorona, Sharedata$lowestincomesincecorona)
  Sharedata$makeendsmeet =  ifelse(Sharedata$hhid8 == i, 
                              Sharedata_union$makeendsmeet, Sharedata$makeendsmeet)
  Sharedata$postponepayment =  ifelse(Sharedata$hhid8 == i, 
                               Sharedata_union$postponepayment, Sharedata$postponepayment)
  Sharedata$dipsavings =  ifelse(Sharedata$hhid8 == i, 
                              Sharedata_union$dipsavings, Sharedata$dipsavings)
}

# Eliminamos la variable financial respondent

Sharedata <- Sharedata %>% select(-financialrespondent)


# ******************************************************************************
#                      VARIABLE OBJETIVO
# ******************************************************************************

# La variable objetivo de la encuesta original toma valores 
# -9: Cuando no se ha sentido mas triste en el ultimo mes
# -2: Se niega a contestar
# -1: No lo sabe
#  1: Se ha sentido mas triste en el ultimo mes que antes de que comenzara la 
#     pandemia
#  2: Se ha sentido igual de triste en el ultimo mes que cuando comenzo la 
#     pandemia
#  3: Se ha sentido menos triste en el ultimo mes que antes de que comenzara la 
#     pandemia
#  NAs

Sharedata <-  dplyr::rename(Sharedata, sadness=camh802_)

# Con motivo del gran numero de observaciones de las que disponemos, nos 
# quedamos tan solo con aquellos encuestados que si se han sentido tristes en el 
# ultimo mes (quitamos el valor -9) y ademas eliminamos los se niega a contestar, 
# los no lo sabe y los NAs.

Sharedata <- Sharedata %>% filter(sadness == 1 | sadness == 2 | sadness == 3)

# Nos quedamos con 14255 observaciones. Ademas, con el objetivo de convertir la 
# variable objetivo en binaria, otorgamos el valor 1 "Se ha sentido mas triste 
# que antes de que comenzara la pandemia" y 0: "Se ha sentido igual o menos 
# triste que antes de que comenzara la pandemia". Para ello, creamos una funcion 
# que otorgue el valor 0 a los valores 2 y 3 de la variable objetivo y  el valor
# 1 al propio valor 1 de la variable objetivo

objetivo <- function(sadness) {
  dplyr::case_when((sadness == 2) ~ 0, 
                   (sadness == 3) ~ 0,
                   TRUE ~ 1)
}

Sharedata_final <- Sharedata %>% mutate(sadness= objetivo(sadness))


# Eliminamos la variable en la que preguntan si el encuestado se ha sentido 
# triste o deprimido en el ultimo mes

Sharedata_final <- Sharedata_final %>% select(-camh002_)


#Gráfico de barras:

fg <- Sharedata_final %>%
  dplyr::count(sadness) %>%
  mutate(
    perc = round(proportions(n) * 100, 1),
    res = str_c(n, "\n","(", perc, "%)"),
    sadness = as.factor(sadness)
  )

library(MetBrewer)
ggplot(fg, aes(sadness, y=n, fill = sadness)) +
  labs(y = "Sadness",
       x = "Count",
       color = "Sadness",
       title = "Distribución variable objetivo",
       subtitle = "Bar chart de la variable objetivo: Sadness.",
       caption =
         paste0("Autor: Lucía Pascual Martínez | ",
                "Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)")) +
  scale_fill_manual(values=c("#c399a2", "#7d87b2"))+
  ylim(NA, 10000) +
  geom_col(color="black") +
  geom_text(aes(label = res), vjust = -0.15, size=4)

# ******************************************************************************
#                  DEPURACION DE NUESTRA ENCUESTA
# ******************************************************************************


# En vez de tener una variable con el año de nacimiento de cada encuestado, 
# vamos a crear una variable con la edad de dicho encuestado. Puesto que tenemos
# el mes de nacimiento del encuestado (cadn002_) y el mes en el que se le realizo
# la encuesta, podemos ver si dicho encuestado habia cumplido años antes de que
# se le realizara la encuesta (en cuyo caso su edad seria 2020 - año nacimiento)
# o si no (en cuyo caso su edad seria 2020 - año nacimiento - 1)


# Renombramos las variables


Sharedata_final <-  dplyr::rename(Sharedata_final, monthborn=cadn002_,
                     yearbirth = cadn003_)



# COnvertimos los meses de int_month_ca en numeros

Sharedata_final <- Sharedata_final %>% mutate(int_month_ca = 
                             case_when(int_month_ca == "May" ~ "5",
                                       int_month_ca == "June" ~ "6",
                                       int_month_ca == "July" ~ "7", 
                                       int_month_ca == "August" ~"8",
                                       int_month_ca == "September" ~ "9",
                                       TRUE ~ as.character(NA)))


Sharedata_final <- Sharedata_final %>% mutate(int_month_ca = as.numeric
                                              (int_month_ca),
                                              monthborn = as.numeric(monthborn),
                                              yearbirth = as.numeric(yearbirth))

# Ahora creamos una funcion de manera que si el mes en el que fue encuestado
# es mayor (o igual) que el mes de nacimiento entonces su edad sera 2020 - Año 
# Nacimiento y en caso contrario sera 2020 - Año nacimiento - 1.


Sharedata_final <- Sharedata_final %>% mutate(age = ifelse(
                                        !is.na(int_month_ca) >= monthborn, 
                                        2020 - yearbirth, 2019 - yearbirth))


# La variable cadn003_ nos indica el año de nacimiento de cada persona, puesto 
# que queremos solo quedarnos con aquellas personas mayores de 50 años y a veces
# se entrevistan a personas que conviven con dichos encuestados mayores de 50
# aunque no tengan 50 años, vamos a eliminar de nuestra base de datos a 
# aquellos encuestados con edad menor a los 50 años (que hayan nacido en 1971 o
# posterior).


Sharedata_final <- Sharedata_final %>% filter(age >= 50)

# Eliminamos las variables que no usaremos mas

Sharedata_final <- Sharedata_final %>% select(-c(monthborn, yearbirth, 
                                                 int_month_ca))



# Estudiamos la relación de la edad con la variable objetivo
# La gente de mucha edad se ha sentido menos o igual de triste.

ggplot(Sharedata_final, aes(x = age, fill=factor(sadness))) +
  geom_histogram( colour = "brown")+
  theme(text = element_text(family= "lobstertwo", size=12), axis.title.y= element_text(size=25, family="lobstertwo"), 
        axis.title.x=element_text(size=20, family="lobstertwo")) +
  labs(title= "Tristeza vs Edad")

# ...........Variable cadn042_ .................................................


# Genero de los encuestados, cambiamos los números de cada género por categorías. 

Sharedata_final <-  dplyr::rename(Sharedata_final, gender=cadn042_)

Sharedata_final <- Sharedata_final %>% mutate(gender = ifelse(gender == 1,
                                                              "Male", "Female"))



Sharedata_final %>% dplyr::count(gender) %>% mutate(prop = n / sum(n)*100)

# Estudiamos la relación del genero con la variable obejtivo

ggplot(Sharedata_final, aes(x = factor(gender), fill= factor(sadness))) + 
  geom_bar(color= "black") 

prop.table(table(Sharedata_final$sadness, Sharedata_final$gender), margin=2)

# Las mujeres estan mas tristes que los hombres

# Female      Male
# 0 0.3559202 0.3878407
# 1 0.6440798 0.6121593

# ........................ Variable caa006_.....................................

# La renombramos

Sharedata_final <-  dplyr::rename(Sharedata_final, movehouse=caa006_)

# Esta variable indica si la persona encuestada se ha mudado o no de manera 
# temproal debido al coronavirus. 

Sharedata_final %>% dplyr::count(movehouse) %>% mutate(prop = n / sum(n)*100)

# > Sharedata_final %>% count(movehouse) %>% mutate(prop = n / sum(n)*100)
# # A tibble: 3 x 3
# caa006_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -1 [-1. Don't know]                         4  0.0282
# 2  1 [1. Usual home]                      13842 97.7   
# 3  2 [2. Lives now temporarily elsewhere]   320  2.26


# Dicha variable tiene muy poca variabilidad (98% de no frente a 1% de si). 
# Estudiamos su relacion con la variable objetivo para ver si la podemos
# eliminar.

ggplot(Sharedata_final, aes(x = factor(sadness), fill=factor(movehouse))) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1.5), vjust=-0.5) +
  theme(text = element_text(family= "lobstertwo", size=12), axis.title.y= element_text(size=25, family="lobstertwo"), 
        axis.title.x=element_text(size=20, family="lobstertwo")) +
  labs(title= "Tristeza vs Mudanza")


Sharedata_final <- Sharedata_final %>% select(-movehouse)

# ................ Variable CAPH003_ ..........................................

# La renombramos

library(dplyr)
Sharedata_final <-  rename(Sharedata_final, healthbcovid=caph003_)

# Esta variable indica si el nivel en el que el encuestado tenia su salud antes
# de la entrada del coronavirus. Es una variable ordinal, la pasamos a factor 
# con el fin de facilitar la exploracion de datos y convertimos los "Refusal" 
# y "Don't know" en NAs. Además, las categorias Excelente y Muy Bueno tienen 
# una proporcion mucho mas pequeña que el resto de categorias, por lo que las 
# unimos en una unica categoria "Muy buena"


saludantescovid <- function(healthbcovid) {
  dplyr::case_when((healthbcovid == 1) ~ "Very Good", 
                   (healthbcovid == 2) ~ "Very Good",
                   (healthbcovid == 3) ~ "Good",
                   (healthbcovid == 4) ~ "Fair",
                   (healthbcovid == 5) ~ "Poor",
                   TRUE ~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(healthbcovid= saludantescovid(healthbcovid))
Sharedata_final %>% count(healthbcovid) %>% mutate(prop = n / sum(n)*100)

# Estudiamos su relacion con la variable objetivo

ggplot(Sharedata_final, aes(x = factor(sadness), fill=factor(healthbcovid))) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1.5), vjust=-0.5) +
  theme(text = element_text(family= "lobstertwo", size=12), axis.title.y= element_text(size=25, family="lobstertwo"), 
        axis.title.x=element_text(size=20, family="lobstertwo")) +
  labs(title= "Tristeza vs Mudanza")


# Cuanto mejor tenian su salud ante del covid, mas encuestados se encuentran
# trsites 

prop.table(table(Sharedata_final$sadness, Sharedata_final$healthbcovid), margin=2)

# Fair      Good      Poor Very Good
# 0 0.3706704 0.3542219 0.4419865 0.2855485
# 1 0.6293296 0.6457781 0.5580135 0.7144515


# ................ VAriable CAH002_ ..........................................

# La renombramos

Sharedata_final <-  rename(Sharedata_final, healthacovid=cah002_)

# Esta variabe indica si la salud del encuestado ha mejorado, empeorado o se ha 
# mantenido igual despues de la irrupcion de la pandemia. 

# # A tibble: 5 x 3
# cah002_     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -2 [-2. Refusal]           1  0.00706
# 2 -1 [-1. Don't know]       11  0.0777 
# 3  1 [1. Improved]         387  2.73   
# 4  2 [2. Worsened]        2832 20.0    
# 5  3 [3. About the same] 10935 77.2


# la dejamos como esta conviertiendo los "no sabe" y "se niega" en valores nulos.
# la convertimos en categorica ya que los numeros que lleva la encuesta no siguen
# un orden logico. Además, unimos las categorias "Mejorado" y "Mas o menos igual"
# debido a que la categoría "Mejorado" tiene muy poca variabilidad.


saluddespcovid <- function(healthacovid) {
  dplyr::case_when((healthacovid == 1) ~ "Improved or the same", 
                   (healthacovid == 2) ~ "Worsened",
                   (healthacovid == 3) ~ "Improved or the same",
                   TRUE ~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(healthacovid= saluddespcovid(healthacovid))

# Estudiamos su relacion con la variable objetivo

ggplot(Sharedata_final, aes(x = factor(sadness), fill=factor(healthacovid))) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1.5), vjust=-0.5) +
  theme(text = element_text(family= "lobstertwo", size=12), axis.title.y= element_text(size=25, family="lobstertwo"), 
        axis.title.x=element_text(size=20, family="lobstertwo")) +
  labs(title= "Tristeza vs Mudanza")

prop.table(table(Sharedata_final$sadness, Sharedata_final$healthacovid), margin=2)

# Improved or the same  Worsened
# 0            0.4001943 0.2277542
# 1            0.5998057 0.7722458

# ................. Variable cah003_ ..........................................


# La renombramos

Sharedata_final <-  rename(Sharedata_final, seriousillness=cah003_)


# Esta variable indica si desde la ultima entrevista el encuestado ha sufrido 
# alguna enfermedad grave

# # A tibble: 4 x 3
# cah003_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        2  0.0141
# 2 -1 [-1. Don't know]    23  0.162 
# 3  1 [1. Yes]          2396 16.9   
# 4  5 [5. No]          11745 82.9 


# Un 17% de los encuestados dice que si frente a un 82.9% de los encuestados que 
# opina que no. Convertimos los refusal y don't know en NAs. Ademas convertimos 
# los "5" que indican No en "0"

Sharedata_final <- Sharedata_final %>% mutate(seriousillness = ifelse(
  seriousillness == -1 | seriousillness ==-2, NA,
  ifelse(seriousillness == 5, 0, 1)))

# Estudiamos su relacion con la variable objetivo

ggplot(Sharedata_final, aes(x = factor(sadness), fill=factor(seriousillness))) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1.5), vjust=-0.5) +
  theme(text = element_text(family= "lobstertwo", size=12), axis.title.y= element_text(size=25, family="lobstertwo"), 
        axis.title.x=element_text(size=20, family="lobstertwo")) +
  labs(title= "Tristeza vs Mudanza")

prop.table(table(Sharedata_final$sadness, Sharedata_final$seriousillness), margin=2)

# 0         1
# 0 0.3719029 0.3343072
# 1 0.6280971 0.6656928


# ............. Variables CAH004_ ............................................


# Estas variables nos indican las enfermedades graves que ha podido tener 
# el encuestado (en caso de que las tengan).


# --- Variable CAH004_1: indica si dicha enfermedad ha sido una fractura de 
# cadera, tiene muy poca variabilidad asi que la eliminas (tan solo un 0.2%
# responde que si.)

# La renombramos

Sharedata_final <-  rename(Sharedata_final, hipfracture=cah004_1)


Sharedata_final %>% count(hipfracture) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah004_1     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 11770 83.1    
# 2 -2 [-2. Refusal]                          1  0.00706
# 3 -1 [-1. Don't know]                       2  0.0141 
# 4  1 [1. Yes]                             132  0.932  
# 5  5 [5. No]                             2261 16.0

# Transformamos los no aplicables en que no tiene esta enfermedad (un cero), y 
# los no se sabe en NA

majorillnes <- function(x) {
  dplyr::case_when((x == 1) ~ 1, 
                   (x == 5) ~ 0,
                   (x == -9) ~ 0,
                   TRUE ~ as.double(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(hipfracture= majorillnes(hipfracture))

# La eliminamos por tener muy pcoa variabilidad:

Sharedata_final <- Sharedata_final %>% select(-hipfracture)


# --- Variable CAH004_2:Indica si a enfermedad ha sido diabetes o alto azucar
# en sangre. 

# La renombramos

Sharedata_final <-  rename(Sharedata_final, diabetes=cah004_2)


Sharedata_final %>% count(diabetes) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah004_2     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 11770 83.1   
# 2 -2 [-2. Refusal]                          2  0.0141
# 3 -1 [-1. Don't know]                       6  0.0424
# 4  1 [1. Yes]                             493  3.48  
# 5  5 [5. No]                             1895 13.4

# Transformamos los "No aplicables" y los "NA"

Sharedata_final <- Sharedata_final %>% mutate(diabetes= majorillnes(diabetes))


prop.table(table(Sharedata_final$sadness, Sharedata_final$diabetes), margin=2)

# 0         1
# 0 0.3655324 0.3671400
# 1 0.6344676 0.6328600

# -- Variable CAH004_3: Indica si la enfermedad ha sido presion arterial alta
# o hipertension

# La renombramos

Sharedata_final <-  rename(Sharedata_final, hypertension=cah004_3)

Sharedata_final %>% count(hypertension) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah004_3     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 11770 83.1    
# 2 -2 [-2. Refusal]                          1  0.00706
# 3 -1 [-1. Don't know]                       9  0.0635 
# 4  1 [1. Yes]                            1090  7.69   
# 5  5 [5. No]                             1296  9.15  

# Un 47% de las personas que si tuvieron una enfermedad grave desde la ultima
# entrevista tuvieron presion arterial alta.  Transformamos los no aplicables 
# en que no tiene esta enfermedad (un cero), y los no se sabe en NA.

Sharedata_final <- Sharedata_final %>% mutate(hypertension = majorillnes(hypertension))

prop.table(table(Sharedata_final$sadness, Sharedata_final$hypertension), margin=2)

# 0         1
# 0 0.3672126 0.3477064
# 1 0.6327874 0.6522936

# --- Variable CAH004_4: Indica si la enfermedad grave fue algun problema rela-
# cionado con el corazon.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, heartdisease=cah004_4)


Sharedata_final %>% count(heartdisease) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah004_4     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 11770 83.1    
# 2 -2 [-2. Refusal]                          1  0.00706
# 3 -1 [-1. Don't know]                       9  0.0635 
# 4  1 [1. Yes]                             674  4.76   
# 5  5 [5. No]                             1712 12.1 


# un 28% de las personas que si tuvieron alguna enfermedad grave desde la ultima
# entrevista han tenido alguna problema en el corazon. Convertimos los No aplica-
# bles en 0

Sharedata_final <- Sharedata_final %>% mutate(heartdisease= majorillnes(heartdisease))

prop.table(table(Sharedata_final$sadness, Sharedata_final$heartdisease), margin=2)

# 0         1
# 0 0.3657469 0.3635015
# 1 0.6342531 0.6364985

# --- Variable CAH004_5: Indica si la enfermedad grave ha sido problemas cronicos
# en el pulmon, bronquitis o derivados

# La renombramos

Sharedata_final <-  rename(Sharedata_final, lungdisease=cah004_5)

Sharedata_final %>% count(lungdisease) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah004_5     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 11770 83.1    
# 2 -2 [-2. Refusal]                          1  0.00706
# 3 -1 [-1. Don't know]                       8  0.0565 
# 4  1 [1. Yes]                             312  2.20   
# 5  5 [5. No]                             2075 14.6 



# Estudiamos su relacion con la variable objetivo

ggplot(Sharedata_final, aes(x = factor(sadness), fill=factor(lungdisease))) +
  geom_bar(position = position_dodge()) +
  geom_text(stat='count', 
            aes(label=stat(count)), 
            position = position_dodge(width=1.5), vjust=-0.5) +
  theme(text = element_text(family= "lobstertwo", size=12), axis.title.y= element_text(size=25, family="lobstertwo"), 
        axis.title.x=element_text(size=20, family="lobstertwo")) +
  labs(title= "Tristeza vs Mudanza")

# La eliminamos porque tiene muy poca variabilidad y relacion con la variable objetivo

Sharedata_final <- Sharedata_final %>% select(-lungdisease)

# -- Variable CAH004_6: Indica si la enfermedad grave ha sido cancer o un tumor
# maligno.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, cancer=cah004_6)


Sharedata_final %>% count(cancer) %>% mutate(prop = n / sum(n)*100)


# # A tibble: 5 x 3
# cah004_6     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 11770 83.1    
# 2 -2 [-2. Refusal]                          1  0.00706
# 3 -1 [-1. Don't know]                      14  0.0988 
# 4  1 [1. Yes]                             378  2.67   
# 5  5 [5. No]                             2003 14.1 



# La eliminamos porque tiene muy poca variabilidad y relacion con la variable objetivo

Sharedata_final <- Sharedata_final %>% select(-cancer)


# --- VAriable CAH004_7: Indica si la enfermedad ha sido otra de las no mencio-
# nadas anteriormente:

# La renombramos

Sharedata_final <-  rename(Sharedata_final, otherdisease=cah004_7)


Sharedata_final %>% count(otherdisease) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah004_7     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 11770 83.1    
# 2 -2 [-2. Refusal]                          2  0.0141 
# 3 -1 [-1. Don't know]                       1  0.00706
# 4  1 [1. Yes]                            1545 10.9    
# 5  5 [5. No]                              848  5.99 


# un 10.9% de los encuestados ha tenido una enfermedad diferente a las mencio-
# nadas con anterioridad. Convertimos los don't know en NA y los No aplicables
# en No ha tenido enfermedad grave

Sharedata_final <- Sharedata_final %>% mutate(otherdisease= majorillnes(otherdisease))

prop.table(table(Sharedata_final$sadness, Sharedata_final$otherdisease), margin=2)

# 0         1
# 0 0.3692344 0.3365696
# 1 0.6307656 0.6634304



# Puesto que cada encuestado puede responder a que ha tenido mas de una enfermedad
# grave, vamos a crear una variable que sea la suma de las enfermedades que haya
# tenido cada persona (poniendo un cero para aquellas personas que no hayan
# tenido ninguna).

vector <- c("diabetes","hypertension", "heartdisease", 
            "otherdisease")


Sharedata_final$numberillness <- rowSums(Sharedata_final[,vector])


Sharedata_final%>% count(numberillness) %>% mutate(prop = n / sum(n)*100)
# # A tibble: 9 x 3
# Illness     n     prop
# <dbl> <int>    <dbl>
#   1       0 11819 83.4    
# 2       1   991  7.00   
# 3       2   662  4.67   
# 4       3   441  3.11   
# 5       4   181  1.28   
# 6       5    38  0.268  
# 7       6     3  0.0212 
# 8       7     1  0.00706
# 9      NA    30  0.212 

# Unimos las categorias de "mas de 3" porque tienen muy poca proporcion:


illness <- function(x) {
  dplyr::case_when((x == 0) ~ "None", 
                   (x == 1) ~ "One",
                   (x == 2) ~ "Two",
                   (x >= 3) ~ "More than two",
                   TRUE ~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(numberillness= illness(numberillness))

prop.table(table(Sharedata_final$sadness, Sharedata_final$numberillness), margin=2)

# More than two      None       One       Two
# 0     0.3546256 0.3701979 0.3167825 0.3681818
# 1     0.6453744 0.6298021 0.6832175 0.6318182


# .............. Variable CAPH089_1.............................................

# Esta variable indica si en los ultimos 6 meses el encuestado se ha caido. 

# La renombramos

Sharedata_final <-  rename(Sharedata_final, falldown=caph089_1)

# VEamos su proporcion.


Sharedata_final %>% count(falldown) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# caph089_1     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        8  0.0565
# 2 -1 [-1. Don't know]    31  0.219 
# 3  1 [1. Yes]          2331 16.5   
# 4  5 [5. No]          11796 83.3 

# Convierto el numero 5 en 0 con el objetivo de que sea binaria y el -2 y -1 en 
# valores nulos.

depuracion <- function(x) {
  dplyr::case_when((x == 1) ~ 1, 
                   (x == 5) ~ 0,
                   TRUE ~ as.double(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(falldown= depuracion(falldown))

# Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$falldown), margin=2)

# 0         1
# 0 0.3581723 0.4028314
# 1 0.6418277 0.5971686


# .............. Variable CAPH089_3.............................................

# Esta variable indica si en los ultimos 6 meses el encuestado se ha desmayado o
# sufrido mareos. 

# La renombramos

Sharedata_final <-  rename(Sharedata_final, dizziness=caph089_3)

# Veamos su proporcion.


Sharedata_final %>% count(dizziness) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# caph089_3     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        8  0.0565
# 2 -1 [-1. Don't know]    33  0.233 
# 3  1 [1. Yes]          4437 31.3   
# 4  5 [5. No]           9688 68.4 

# Convierto el numero 5 en 0 con el objetivo de que sea binaria  y los no se sabe
# o se niega a contestar en NAs


Sharedata_final <- Sharedata_final %>% mutate(dizziness= depuracion(dizziness))

# Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$dizziness), margin=2)

# 0         1
# 0 0.3441371 0.4110886
# 1 0.6558629 0.5889114

# .............. Variable CAPH089_4.............................................

# Esta variable indica si en los ultimos 6 meses el encuestado ha tenido fatiga

# La renombramos

Sharedata_final <-  rename(Sharedata_final, fatigue=caph089_4)

# Veamos su proporcion.


Sharedata_final %>% count(fatigue) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# caph089_4     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]       10  0.0706
# 2 -1 [-1. Don't know]    51  0.360 
# 3  1 [1. Yes]          7008 49.5   
# 4  5 [5. No]           7097 50.1 

# Convierto el numero 5 en 0 con el objetivo de que sea binaria  y los no se sabe
# o se niega a contestar en NAs


Sharedata_final <- Sharedata_final %>% mutate(fatigue= depuracion(fatigue))


# Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$fatigue), margin=2)

# 0         1
# 0 0.3421164 0.3884132
# 1 0.6578836 0.6115868

# ................. Variable CAH006_ ...........................................

# Esta variable indica si el encuestado toma medicacion regularmente.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, prescriptiondrug=cah006_)


Sharedata_final %>% count(prescriptiondrug) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# cah006_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        4  0.0282
# 2 -1 [-1. Don't know]     5  0.0353
# 3  1 [1. Yes]         12037 85.0   
# 4  5 [5. No]           2120 15.0 

# Un 85% de los encuestados toma medicacion regularmente. Convierto el numero 5
# en 0 con el objetivo de que sea binaria  y los no se sabe o se niega a 
# contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(prescriptiondrug= depuracion(prescriptiondrug))

# Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$prescriptiondrug), margin=2)

# 0         1
# 0 0.3235849 0.3729335
# 1 0.6764151 0.6270665


# -- A continuacion se pregunta si el encuestado toma alguno de los siguientes
# medicamentos


# --- Variable CAH007_1 Medicacion para el colesterol alto

# La renombramos

Sharedata_final <-  rename(Sharedata_final, cholesteroldrug=cah007_1)


Sharedata_final %>% count(cholesteroldrug) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah007_1     n   prop
# <dbl+lbl> <int>  <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]  2129 15.0  
# 2 -2 [-2. Refusal]                         19  0.134
# 3 -1 [-1. Don't know]                     106  0.748
# 4  1 [1. Yes]                            4958 35.0  
# 5  5 [5. No]                             6954 49.1 

# Transformamos los no aplicables 
# en que no tiene esta enfermedad (un cero), y los no se sabe en NA.

Sharedata_final <- Sharedata_final %>% mutate(cholesteroldrug = majorillnes(cholesteroldrug))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$cholesteroldrug), margin=2)

# 0         1
# 0 0.3623252 0.3682937
# 1 0.6376748 0.6317063

# --- Variable CAH007_2 Medicacion para la presion arterial alta

# La renombramos

Sharedata_final <-  rename(Sharedata_final, highbloodpreasuredrug=cah007_2)


Sharedata_final %>% count(highbloodpreasuredrug) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah007_1     n   prop
# <dbl+lbl> <int>  <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]  2129 15.0  
# 2 -2 [-2. Refusal]                         19  0.134
# 3 -1 [-1. Don't know]                     106  0.748
# 4  1 [1. Yes]                            4958 35.0  
# 5  5 [5. No]                             6954 49.1 

# Transformamos los no aplicables 
# en que no tiene esta enfermedad (un cero), y los no se sabe en NA.

Sharedata_final <- Sharedata_final %>% mutate(highbloodpreasuredrug = majorillnes(highbloodpreasuredrug))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$highbloodpreasuredrug), margin=2)

# 0         1
# 0 0.3385965 0.3830040
# 1 0.6614035 0.6169960

# --- Variable CAH007_3 Medicacion para problemas coronarios


# La renombramos

Sharedata_final <-  rename(Sharedata_final, coronarydiseasedrug=cah007_3)

Sharedata_final %>% count(coronarydiseasedrug) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah007_3     n   prop
# <dbl+lbl> <int>  <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]  2129 15.0  
# 2 -2 [-2. Refusal]                         19  0.134
# 3 -1 [-1. Don't know]                      95  0.671
# 4  1 [1. Yes]                            3072 21.7  
# 5  5 [5. No]                             8851 62.5  

# Transformamos los no aplicables 
# en que no tiene esta enfermedad (un cero), y los no se sabe en NA.

Sharedata_final <- Sharedata_final %>% mutate(coronarydiseasedrug = majorillnes(coronarydiseasedrug))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$coronarydiseasedrug), margin=2)

# 0         1
# 0 0.3514572 0.4140625
# 1 0.6485428 0.5859375
 
# --- Variable CAH007_5 Diabetes

# La renombramos

Sharedata_final <-  rename(Sharedata_final, diabetesdrug=cah007_5)

Sharedata_final %>% count(diabetesdrug) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah007_5     n   prop
# <dbl+lbl> <int>  <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]  2129 15.0  
# 2 -2 [-2. Refusal]                         20  0.141
# 3 -1 [-1. Don't know]                      49  0.346
# 4  1 [1. Yes]                            2580 18.2  
# 5  5 [5. No]                             9388 66.3  

# Transformamos los no aplicables 
# en que no tiene esta enfermedad (un cero), y los no se sabe en NA.

Sharedata_final <- Sharedata_final %>% mutate(diabetesdrug = majorillnes(diabetesdrug))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$diabetesdrug), margin=2)

# 0         1
# 0 0.3546062 0.4120155
# 1 0.6453938 0.5879845

# --- Variable CAH007_6 Bronquitis cronica

# La renombramos

Sharedata_final <-  rename(Sharedata_final, bronchitisdrug=cah007_6)


Sharedata_final %>% count(bronchitisdrug) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah007_6     n   prop
# <dbl+lbl> <int>  <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]  2129 15.0  
# 2 -2 [-2. Refusal]                         21  0.148
# 3 -1 [-1. Don't know]                      75  0.529
# 4  1 [1. Yes]                             954  6.73 
# 5  5 [5. No]                            10987 77.6  

# Transformamos los no aplicables 
# en que no tiene esta enfermedad (un cero), y los no se sabe en NA.

Sharedata_final <- Sharedata_final %>% mutate(bronchitisdrug = majorillnes(bronchitisdrug))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$bronchitisdrug), margin=2)

# 0         1
# 0 0.3652790 0.3574423
# 1 0.6347210 0.6425577

# Puesto que cada encuestado puede responder a que toma mas de un medicamento,
# vamos a crear una variable que sea la suma de los medicamentos que tome
# cada persona (poniendo un cero para aquellas personas que no tomen ninguno)

vector <- c("cholesteroldrug","highbloodpreasuredrug", "coronarydiseasedrug", 
            "diabetesdrug", "bronchitisdrug")

Sharedata_final$numdrugs <- rowSums(Sharedata_final[,vector])

Sharedata_final %>% count(numdrugs) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 7 x 3
# Medication     n   prop
# <dbl> <int>  <dbl>
#   1          0  3882 27.4  
# 2          1  4032 28.5  
# 3          2  3300 23.3  
# 4          3  1965 13.9  
# 5          4   685  4.84 
# 6          5    84  0.593
# 7         NA   218  1.54

# como la categoria de 5 medicamentos tiene muy poca proporcion, la juntamos con
# la de 4 medicamentos:


medication <- function(x) {
  dplyr::case_when((x == 5) ~ 4, 
                   TRUE ~ as.double(x))
}

Sharedata_final <- Sharedata_final %>% mutate(numdrugs= medication(numdrugs))



#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$numdrugs), margin=2)

# 0         1         2         3         4
# 0 0.3338485 0.3583829 0.3666667 0.4122137 0.4109233
# 1 0.6661515 0.6416171 0.6333333 0.5877863 0.5890767

# .................. Variable CAH010_ .....................................


# Esta variable indica si el encuestado, desde que comenzo el coronavirus, 
# ha salido de su casa

# La renombramos

Sharedata_final <-  rename(Sharedata_final, lefthome=cah010_)


Sharedata_final %>% count(lefthome) %>% mutate(prop = n / sum(n)*100)
# 
# # A tibble: 4 x 3
# cah010_     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -2 [-2. Refusal]        4  0.0282 
# 2 -1 [-1. Don't know]     1  0.00706
# 3  1 [1. Yes]         10640 75.1    
# 4  5 [5. No]           3521 24.9 

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(lefthome= depuracion(lefthome))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$lefthome), margin=2)

# 0         1
# 0 0.4018745 0.3534774
# 1 0.5981255 0.6465226

# ...................Variable CAH012_ .....................................


# Esta variable indica, si el encuestado ha salido de casa desde que comenzo la
# pandemia. Con qué frecuencia ha utilizado la mascarilla.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, facemaskout=cah012_)


Sharedata_final %>% count(facemaskout) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 7 x 3
# cah012_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]  3526 24.9   
# 2 -2 [-2. Refusal]                          6  0.0424
# 3 -1 [-1. Don't know]                       8  0.0565
# 4  1 [1. Always]                         6923 48.9   
# 5  2 [2. Often]                          1183  8.35  
# 6  3 [3. Sometimes]                      1075  7.59  
# 7  4 [4. Never]                          1445 10.2 

# Vemos que casi el 50% de los encuestados (un 65% de entre los que si salieron
# desde el comienzo de la pandemia) llevaron siempre la mascarilla.


# Como un 24.9% no aplica, transformamos estos valores (-9) en la categoria 
# "Has not left home" y transformamos los "se niega" y "no sabe" a NAs.


lefthome <- function(x) {
  dplyr::case_when((x == 1) ~ "Always", 
                   (x == 2) ~ "Often",
                   (x == 3) ~ "Sometimes",
                   (x == 4) ~ "Never",
                   (x == -9) ~ "Has not left home",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(facemaskout = lefthome(facemaskout))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$facemaskout), margin=2)


# ...................Variable CAH013_ .....................................


# Esta variable indica, si el encuestado ha salido de casa desde que comenzo la
# pandemia, si ha mantenido distancia con las demas personas en la calle.


# La renombramos

Sharedata_final <-  rename(Sharedata_final, keepdistance=cah013_)


Sharedata_final %>% count(keepdistance) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 7 x 3
# cah013_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]  3526 24.9   
# 2 -2 [-2. Refusal]                          6  0.0424
# 3 -1 [-1. Don't know]                      34  0.240 
# 4  1 [1. Always]                         8280 58.4   
# 5  2 [2. Often]                          1779 12.6   
# 6  3 [3. Sometimes]                       418  2.95  
# 7  4 [4. Never]                           123  0.868 

# Vemos que la mayoria de los encuestados guardaron distancia con los demas 
# al salir a la calle.


# Las categorias "a veces" y "nunca" tienen muy poca proporcion por lo que las 
# unimos en una sola. Como la categoria "nunca" tiene solo un 0.8% la unimos
# a "A veces"


lefthome <- function(x) {
  dplyr::case_when((x == 1) ~ "Always", 
                   (x == 2) ~ "Often",
                   (x == 3) ~ "Sometimes",
                   (x == 4) ~ "Sometimes",
                   (x == -9) ~ "Has not left home",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(keepdistance = lefthome(keepdistance))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$keepdistance), margin=2)

# Always Has not left home     Often Sometimes
# 0 0.3349034         0.4021554 0.3923553 0.5046211
# 1 0.6650966         0.5978446 0.6076447 0.4953789

# ..........................Variable CAH014_ .................................

# Esta variable indica si la persona se lava las manos con mayor frecuencia 
# desde que comenzo el coronavirus. 

# La renombramos

Sharedata_final <-  rename(Sharedata_final, handwash=cah014_)


Sharedata_final %>% count(handwash) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# cah014_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        2  0.0141
# 2 -1 [-1. Don't know]    24  0.169 
# 3  1 [1. Yes]         12653 89.3   
# 4  5 [5. No]           1487 10.5 

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(handwash= depuracion(handwash))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$handwash), margin=2)

# 0         1
# 0 0.4969738 0.3500356
# 1 0.5030262 0.6499644


# ..........................Variable CAH015_ .................................

# Esta variable indica si la persona usa mas desinfectante de manos desde que 
# comenzó la pandemia

# La renombramos

Sharedata_final <-  rename(Sharedata_final, handsanitizer=cah015_)

Sharedata_final %>% count(handsanitizer) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 3 x 3
# cah015_     n   prop
# <dbl+lbl> <int>  <dbl>
#   1 -1 [-1. Don't know]    22  0.155
# 2  1 [1. Yes]         11693 82.5  
# 3  5 [5. No]           2451 17.3  

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(handsanitizer= depuracion(handsanitizer))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$handsanitizer), margin=2)

# 0         1
# 0 0.4634843 0.3449072
# 1 0.5365157 0.6550928

# ..........................Variable CAH016_ .................................

# Esta variable indica si la persona presta mas atencion a cubrirse cuando 
# tose o estornuda desde que comenzo la pandemia

# La renombramos

Sharedata_final <-  rename(Sharedata_final, covercoughandsneeze=cah016_)


Sharedata_final %>% count(covercoughandsneeze) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# cah016_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        7  0.0494
# 2 -1 [-1. Don't know]   102  0.720 
# 3  1 [1. Yes]         11885 83.9   
# 4  5 [5. No]           2172 15.3   

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(covercoughandsneeze= depuracion(covercoughandsneeze))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$covercoughandsneeze), margin=2)

# 0         1
# 0 0.4226519 0.3542280
# 1 0.5773481 0.6457720

# ..........................Variable CAH017_ .................................

# Esta variable indica si la persona toma medicamentos para prevenir el 
# coronavirus


# La renombramos

Sharedata_final <-  rename(Sharedata_final, drugagainstcovid=cah017_)


Sharedata_final %>% count(drugagainstcovid) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 3 x 3
# cah017_     n   prop
# <dbl+lbl> <int>  <dbl>
#   1 -1 [-1. Don't know]    26  0.184
# 2  1 [1. Yes]          1113  7.86 
# 3  5 [5. No]          13027 92.0  

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(drugagainstcovid= depuracion(drugagainstcovid))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$drugagainstcovid), margin=2)

# 0         1
# 0 0.3664696 0.3522013
# 1 0.6335304 0.6477987

# ..........................Variable CAH020_ .................................

# Esta variable indica si el encuestado se ha sentido nervioso, ansioso o al 
# limite en el ultimo mes.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, anxiety=cah020_)


Sharedata_final %>% count(anxiety) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# cah020_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        2  0.0141
# 2 -1 [-1. Don't know]    26  0.184 
# 3  1 [1. Yes]          9586 67.7   
# 4  5 [5. No]           4552 32.1  

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(anxiety= depuracion(anxiety))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$anxiety), margin=2)

# 0         1
# 0 0.5037346 0.2992906
# 1 0.4962654 0.7007094


# ..........................Variable CAH021_ .................................

# Esta variable indica si el encuestado se ha sentido mas, menos o igual de 
# nervioso, ansioso o al limite en el ultimo mes que antes de la pandemia.


# La renombramos

Sharedata_final <-  rename(Sharedata_final, anxietylevel=cah021_)


Sharedata_final %>% count(anxietylevel) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cah021_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]  4580 32.3   
# 2 -1 [-1. Don't know]                       6  0.0424
# 3  1 [1. More so]                        6934 48.9   
# 4  2 [2. Less so]                         165  1.16  
# 5  3 [3. About the same]                 2481 17.5 
  
# Convertimos los numeros en categorias ya que no siguen un orden logico. La 
# categoria "Not Applicable" la cambiamos por "Not Nervous" para indicar que 
# se corresponde con los encuestados que han respondido que no a la pregunta 
# anterior.  Agrupamos "menos" e "igual" por tener poca representatividad

depu2 <- function(x) {
  dplyr::case_when((x == 1) ~ "More so", 
                   (x == 2) ~ "Less or the same",
                   (x == 3) ~ "Less or the same",
                   (x == -9) ~ "Not nervous",
                   TRUE ~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(anxietylevel= depu2(anxietylevel))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$anxietylevel), margin=2)

# Less or the same   More so Not nervous
# 0        0.7611489 0.1225844   0.5043668
# 1        0.2388511 0.8774156   0.4956332


# ..........................Variable CAMH007_ .................................

# Esta variable indica si el encuestado ha tenido problemas para dormir 
# recientemente


# La renombramos

Sharedata_final <-  rename(Sharedata_final, troublesleeping=camh007_)


Sharedata_final %>% count(troublesleeping) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# camh007_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]                                           3  0.0212
# 2 -1 [-1. Don't know]                                       18  0.127 
# 3  1 [1. Trouble with sleep or recent change in pattern]  7213 50.9   
# 4  2 [2. No trouble sleeping]                             6932 48.9 

depu3 <- function(x) {
  dplyr::case_when((x == 1) ~ 1, 
                   (x == 2) ~ 0,
                   TRUE ~ as.double(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(troublesleeping= depu3(troublesleeping))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$troublesleeping), margin=2)

# 0         1
# 0 0.3727640 0.3582421
# 1 0.6272360 0.6417579

# ..........................Variable CAMH807_ .................................

# Esta variable indica, si el encuestado ha tenido problemas para dormir 
# recientemente, si ha sido mas, menos o igual que antes de la pandemia

# La renombramos

Sharedata_final <-  rename(Sharedata_final, sleeptroublelevel=camh807_)


Sharedata_final %>% count(sleeptroublelevel) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 6 x 3
# camh807_     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]  6953 49.1    
# 2 -2 [-2. Refusal]                          1  0.00706
# 3 -1 [-1. Don't know]                      12  0.0847 
# 4  1 [1. More so]                        3031 21.4    
# 5  2 [2. Less so]                         127  0.897  
# 6  3 [3. About the same]                 4042 28.5  

# Convertimos los numeros en categorias ya que no siguen un orden logico. La 
# categoria "Not Applicable" la cambiamos por "Not Nervous" para indicar que 
# se corresponde con los encuestados que han respondido que no a la pregunta 
# anterior. Unimos las categorías "Menos y mas o menos el mismo".

depu4 <- function(x) {
  dplyr::case_when((x == 1) ~ "More so", 
                   (x == 2) ~ "Less or the same",
                   (x == 3) ~ "Less or the same",
                   (x == -9) ~ "Not trouble sleeping",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(sleeptroublelevel= depu4(sleeptroublelevel))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$sleeptroublelevel), margin=2)

# Less or the same   More so Not trouble sleeping
# 0        0.5396978 0.1085450            0.3732202
# 1        0.4603022 0.8914550            0.626779

# ..........................Variable CAMH037_ .................................

# Esta variable indica cada cuanto tiempo se siente solo el encuestado

# La renombramos

Sharedata_final <-  rename(Sharedata_final, loneliness=camh037_)



Sharedata_final %>% count(loneliness) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# camh037_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]                 2  0.0141
# 2 -1 [-1. Don't know]             78  0.551 
# 3  1 [1. Often]                 2894 20.4   
# 4  2 [2. Some of the time]      4940 34.9   
# 5  3 [3. Hardly ever or never]  6252 44.1 

# Cambiamos los no sabe o se niega a contestar por NAs. La convertimos en categorias

lonely <- function(x) {
  dplyr::case_when((x == 1) ~ "Often", 
                   (x == 2) ~ "Sometimes",
                   (x == 3) ~ "Never",
                   TRUE ~ as.character(NA))
}



Sharedata_final <- Sharedata_final %>% mutate(loneliness = lonely(loneliness))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$loneliness), margin=2)

# Never     Often Sometimes
# 0 0.3741203 0.3286109 0.3742915
# 1 0.6258797 0.6713891 0.6257085

# ..........................Variable CAMH837_ .................................

# Esta variable indica si el tiempo que se siente solo el encuestado es mas, 
# menos o igual que antes de la pandemia. 


# La renombramos

Sharedata_final <-  rename(Sharedata_final, lonelinesslevel=camh837_)

Sharedata_final %>% count(lonelinesslevel) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 6 x 3
# camh837_     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]  6332 44.7    
# 2 -2 [-2. Refusal]                          1  0.00706
# 3 -1 [-1. Don't know]                      14  0.0988 
# 4  1 [1. More so]                        3856 27.2    
# 5  2 [2. Less so]                         192  1.36   
# 6  3 [3. About the same]                 3771 26.6

# Convertimos los numeros en categorias ya que no siguen un orden logico. La 
# categoria "Not Applicable" la cambiamos por "Not Lonely" para indicar que 
# se corresponde con los encuestados que han respondido que no a la pregunta 
# anterior.  Unimos las categorías "Menos y mas o menos el mismo".

depu5 <- function(x) {
  dplyr::case_when((x == 1) ~ "More so", 
                   (x == 2) ~ "Less or the same",
                   (x == 3) ~ "Less or the same",
                   (x == -9) ~ "Not lonely",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(lonelinesslevel= depu5(lonelinesslevel))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$lonelinesslevel), margin=2)

# Less or the same   More so Not lonely
# 0        0.5856674 0.1226660  0.3757107
# 1        0.4143326 0.8773340  0.6242893


# ..................variable CAC002_ .........................................

# Esta variable indica si, desde el comienzo del covid, el encuestado o algun
# conocido suyo ha experimentado sintomas relacionados con el COVID.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, covidsymptom=cac002_)


Sharedata_final %>% count(covidsymptom) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# cac002_     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -2 [-2. Refusal]        1  0.00706
# 2 -1 [-1. Don't know]    76  0.536  
# 3  1 [1. Yes]          1724 12.2    
# 4  5 [5. No]          12365 87.3

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(covidsymptom= depuracion(covidsymptom))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$covidsymptom), margin=2)

# 0         1
# 0 0.3788920 0.2598608
# 1 0.6211080 0.7401392

# ..................variable CAC004_ .........................................

# Esta variable indica si, desde el comienzo del covid, el encuestado o algun
# conocido suyo ha dado positivo en un test de covid.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, positivetest=cac004_)

Sharedata_final %>% count(positivetest) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# cac004_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        3  0.0212
# 2 -1 [-1. Don't know]   123  0.868 
# 3  1 [1. Yes]          1095  7.73  
# 4  5 [5. No]          12945 91.4

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(positivetest= depuracion(positivetest))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$positivetest), margin=2)


# 0         1
# 0 0.3738895 0.2493151
# 1 0.6261105 0.7506849

# A continuacion se les pregunta que quien ha sido la persona que ha dado 
# positivo en dicho test de covid, todas estas variables (cada una para cada
# persona (el encuestado, hijo, pareja, amigo...)) tienen muy poca variabilidad
# y, en lugar de eliminarlas, vamos a convertirlas en dos variables: Una 
# que indique si dicha persona ha sido alguien de su nucleo familiar o su casa
# y otra que indique si ha sido algun amigo, vecino, companero de trabajo u otro.
# Ademas los "Se niega" o "no contesta" los pasamos a NAs y los que no apliquen
# (porque nadie conocido ni el mismo ha dado positivo) los ponemos como "0" o 
# "No"


# La renombramos

Sharedata_final <-  rename(Sharedata_final, respondenttest=cac005_1,
                           partnertest=cac005_2, parenttest=cac005_3,
                           childtest=cac005_4, otherhouseholdtest=cac005_5,
                           otherrelativetest=cac005_6, friendtest=cac005_7,
                           othertest=cac005_97,)


# La variable que llamaremos closepositive indica si el que dio positivo fue un 
# familiar, el mismo encuestado o alguien que conviva con el.

Sharedata_final <- Sharedata_final %>% mutate(closepositive = ifelse(
  respondenttest == 1 | partnertest == 1 | parenttest == 1 |
    childtest == 1 | otherhouseholdtest == 1 | otherrelativetest == 1, 
                              1, ifelse(respondenttest == 0 | respondenttest == -9 |
                     partnertest == 0 | partnertest == -9 | parenttest == -9 |
                       parenttest == 0 | childtest == 0 | childtest == -9 |
                       otherhouseholdtest == 0 | otherhouseholdtest == -9 | 
                       otherrelativetest == -9 |
                       otherrelativetest == 0, 0, NA)))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$closepositive), margin=2)

# 0         1
# 0 0.3700799 0.2727273
# 1 0.6299201 0.7272727


# La variable que llamaremos otherpositive indica si el que dio positivo fue un 
# amigo, vecino u otros.

Sharedata_final <- Sharedata_final %>% mutate(otherpositive = ifelse(
  friendtest == 1 | othertest == 1, 1, 
                                 ifelse(friendtest == 0 | othertest == 0 |
                                          othertest == -9 | friendtest == -9, 0, NA)))

# Tabla de contingencia
# 0         1
# 0 0.3716523 0.2004008
# 1 0.6283477 0.7995992


# Eliminamos las variables que ya no usaremos mas: 

Sharedata_final <- Sharedata_final %>% select(-c(respondenttest,partnertest,
                                                 parenttest, childtest, otherhouseholdtest,
                                                 otherrelativetest, friendtest, othertest))


# ..................variable CAC010_ .........................................

# Esta variable indica si, desde el comienzo del covid, el encuestado o algun
# conocido suyo ha sido hospitalizado por COVID.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, covidhospitalized=cac010_)


Sharedata_final %>% count(covidhospitalized) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# cac010_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        5  0.0353
# 2 -1 [-1. Don't know]    68  0.480 
# 3  1 [1. Yes]           595  4.20  
# 4  5 [5. No]          13498 95.34

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(covidhospitalized= depuracion(covidhospitalized))

# Tabla de contingencia
# 0         1
# 0 0.3721292 0.2117647
# 1 0.6278708 0.7882353


# A continuacion se les pregunta que quien ha sido la persona que ha sido 
# hositalizada por covid-19, todas estas variables (cada una para cada
# persona (el encuestado, hijo, pareja, amigo...)) tienen muy poca variabilidad
# y, aún agrupandolas no tiene suficiente porcentaje en los "Si" por lo que 
# decidimos eliminarlas

# La renombramos

Sharedata_final <-  rename(Sharedata_final, respondenthospitalized=cac011_1,
                           partnerhospitalized=cac011_2, parenthospitalized=cac011_3,
                           childhospitalized=cac011_4, otherhouseholdhospitalized=cac011_5,
                           otherrelativehospitalized=cac011_6, friendhospitalized=cac011_7,
                           otherhospitalized=cac011_97)



# Eliminamos las variables que ya no usaremos mas: 

Sharedata_final <- Sharedata_final %>% select(-c(respondenthospitalized, partnerhospitalized,
                                                 parenthospitalized, childhospitalized,
                                                 otherhouseholdhospitalized, 
                                                 otherrelativehospitalized,
                                                 friendhospitalized,
                                                 otherhospitalized))



# ..................variable CAC013_ .........................................

# Esta variable indica si, desde el comienzo del covid, el encuestado o algun
# conocido suyo ha muerto por COVID.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, coviddeath=cac013_)

Sharedata_final %>% count(coviddeath) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# cac013_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        5  0.0353
# 2 -1 [-1. Don't know]    43  0.304 
# 3  1 [1. Yes]           482  3.40  
# 4  5 [5. No]          13636 96.3 

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(coviddeath= depuracion(coviddeath))


# Tabla de contingencia
# 0         1
# 0 0.3712966 0.1970954
# 1 0.6287034 0.8029046


# A continuacion se les pregunta que quien ha sido la persona que ha 
# fallecido por covid-19, todas estas variables (cada una para cada
# persona (el encuestado, hijo, pareja, amigo...)) tienen muy poca variabilidad
# y, , aún agrupandolas no tiene suficiente porcentaje en los "Si" por lo que 
# decidimos eliminarlas


Sharedata_final <-  rename(Sharedata_final, 
                           partnerdeath=cac014_2, parentdeath=cac014_3,
                           childdeath=cac014_4, otherhouseholddeath=cac014_5,
                           otherrelativedeath=cac014_6, frienddeath=cac014_7,
                           otherdeath=cac014_97)


# Eliminamos las variables que ya no usaremos mas: 

Sharedata_final <- Sharedata_final %>% select(-c( partnerdeath,
                                                 parentdeath,childdeath,
                                                 otherhouseholddeath, otherrelativedeath,
                                                 frienddeath, otherdeath))



#.................. Variable CAQ005_ ...........................................

# Esta variable indica si el encuestado se ha abstenido de acudir al medico por 
# miedo a contagiarse del covid. 

# La renombramos

Sharedata_final <-  rename(Sharedata_final, forgotreatment=caq005_)


Sharedata_final %>% count(forgotreatment) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 3 x 3
# caq005_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -1 [-1. Don't know]    10  0.0706
# 2  1 [1. Yes]          2449 17.3   
# 3  5 [5. No]          11707 82.6 

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(forgotreatment= depuracion(forgotreatment))

# Tabla de contingencia

# 0         1
# 0 0.3840437 0.2780727
# 1 0.6159563 0.7219273

#.................. Variable CAQ010_ ...........................................

# Esta variable indica si el encuestado tenia una cita medica que el medico 
# o el centro medico hayan decidido posponer debido al covid.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, postponedappointment=caq010_)


Sharedata_final %>% count(postponedappointment) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# caq010_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        2  0.0141
# 2 -1 [-1. Don't know]    17  0.120 
# 3  1 [1. Yes]          4556 32.2   
# 4  5 [5. No]           9591 67.7 


# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(postponedappointment= 
                                                depuracion(postponedappointment))

# Tabla de contingencia

# 0         1
# 0 0.3788969 0.3375768
# 1 0.6211031 0.6624232


#.................. Variable CAQ025_ ...........................................

# Esta variable indica si el encuestado ha acudido al hospital desde que comenzo
# el coronavirus.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, treatedhospital=caq025_)

Sharedata_final %>% count(treatedhospital) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# caq025_     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -2 [-2. Refusal]        1  0.00706
# 2 -1 [-1. Don't know]     3  0.0212 
# 3  1 [1. Yes]          1623 11.5    
# 4  5 [5. No]          12539 88.5 

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(treatedhospital= 
                                                depuracion(treatedhospital))

# Tabla de contingencia

# 0         1
# 0 0.3711620 0.3222428
# 1 0.6288380 0.6777572


#.................. Variable CAEP805__ .........................................

# Esta variable indica si el encuestado, en el momento en el que comenzo la 
# pandemia, tenia trabajo o estaba sin trabajo (incluyendo negocios familiares)

# La renombramos

Sharedata_final <-  rename(Sharedata_final, employed=caep805_)


Sharedata_final %>% count(employed) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 3 x 3
# caep805_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]     4  0.0282
# 2  1 [1. Yes]       2185 15.4   
# 3  5 [5. No]       11977 84.5  

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(employed= depuracion(employed))

# Tabla de contingencia

# 0         1
# 0 0.3773900 0.3006865
# 1 0.6226100 0.6993135



# .................. Variable CAW001_ ..........................................

# Esta variable indica si el encuestado, en caso de que trabajara antes de la 
# pandemia, debido a esta perdio  el trabajo (despedido o tuvo que cerrar el
# negocio).

# La renombramos

Sharedata_final <-  rename(Sharedata_final, lostjob=caw002_)

Sharedata_final %>% count(lostjob) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 3 x 3
# caw002_     n  prop
# <dbl+lbl> <int> <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 11981 84.6 
# 2  1 [1. Yes]                             540  3.81
# 3  5 [5. No]                             1645 11.6 

# Conviertimos la categoria -9 (no aplica porque el sujero no trabajaba antes 
# de la pandemia en: "No trabajaba")


work <- function(x) {
  dplyr::case_when((x == 1) ~ "Yes", 
                   (x == 5) ~ "No",
                   (x == -9) ~ "Not working",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(lostjob= work(lostjob))

# Tabla de contingencia

# No Not working       Yes
# 0 0.3331307   0.3774309 0.2018519
# 1 0.6668693   0.6225691 0.7981481

# .................. Variable CAW010_ ..........................................

# Esta variable indica cual fue su lugar de trabajo durante la pandemia. Desde 
# casa, desde la oficina, ambas o ninguna de las anteriores (ninguna de las 
# anteriores indica que no trabajo (ni en su casa ni en su lugar de trabajo))


# La renombramos

Sharedata_final <-  rename(Sharedata_final, workplace=caw010_)

Sharedata_final %>% count(workplace) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 6 x 3
# caw010_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -9 [-9. Not applicable (qqn routing)]                11981 84.6   
# 2 -2 [-2. Refusal]                                         2  0.0141
# 3  1 [1. Worked at home only]                            356  2.51  
# 4  2 [2. Worked at the usual work place]                1010  7.13  
# 5  3 [3. Worked from home and at the usual work place]   282  1.99  
# 6  4 [4. None of these]                                  535  3.78 

# Conviertimos la categoria -9 (no aplica porque el sujero no trabajaba antes 
# de la pandemia en: "No trabajaba") y el resto de categorias numericas en 
# categorias nominales 


work <- function(x) {
  dplyr::case_when((x == 1) ~ "home and or the usual work place", 
                   (x == 2) ~ "Usual work place only",
                   (x == 3) ~ "home and or the usual work place",
                   (x == 4) ~ "None of these",
                   (x == -9) ~ "Not working",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(workplace= work(workplace))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$workplace), margin=2)

# home and or the usual work place None of these Not working Usual work place only
# 0                        0.2601881     0.2542056   0.3774309             0.3504950
# 1                        0.7398119     0.7457944   0.6225691             0.6495050

# .................. Variable CAW016_ ..........................................

# Esta variable indica si, en caso de que el encuestado trabajara en su lugar de 
# trabajo, fue proporcionado con mascarillas, gel desinfectante, guantes....

# La renombramos

Sharedata_final <-  rename(Sharedata_final, protection=caw016_)


Sharedata_final %>% count(protection) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# caw016_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 12872 90.9   
# 2 -1 [-1. Don't know]                       2  0.0141
# 3  1 [1. Yes]                            1103  7.79  
# 4  5 [5. No]                              187  1.32  
# 5 NA                                        2  0.0141

# Conviertimos la categoria -9 (no aplica o bien porque el encuestado no 
# trabajaba antes de la pandemia o bien porque trabaja desde casa o no trabaja
# despues de la pandemia.

work <- function(x) {
  dplyr::case_when((x == 1) ~ "Yes", 
                   (x == 5) ~ "No",
                   (x == -9) ~ "Not working or working just from home",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(protection= work(protection))

# Eliminamos la variable por tener poca variabilidad 

Sharedata_final <- Sharedata_final %>% select(-protection)



# .................. Variable CAW017_ ..........................................

# Esta variable indica si, en caso de que el encuestado trabajara en su lugar de 
# trabajo, se sintio seguro en terminos sanitarios (protegido contra el covid)

# La renombramos

Sharedata_final <-  rename(Sharedata_final, safeworkplace=caw017_)


Sharedata_final %>% count(safeworkplace) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 8 x 3
# caw017_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 12872 90.9   
# 2 -2 [-2. Refusal]                          3  0.0212
# 3 -1 [-1. Don't know]                       2  0.0141
# 4  1 [1. Very safe]                       440  3.11  
# 5  2 [2. Somewhat safe]                   631  4.45  
# 6  3 [3. Somewhat unsafe]                 169  1.19  
# 7  4 [4. Very unsafe]                      47  0.332 
# 8 NA                                        2  0.0141

# Conviertimos la categoria -9 (no aplica o bien porque el encuestado no 
# trabajaba antes de la pandemia o bien porque trabaja desde casa o no trabaja
# despues de la pandemia. Ademas, la categoria "Muy inseguro" tiene muy poca 
# proporcion, convertimos las cuatro categorias en "Muy seguro", "Seguro",
# "Inseguro".

work <- function(x) {
  dplyr::case_when((x == 1) ~ "Very safe", 
                   (x == 2) ~ "Somewhat safe",
                   (x == 3) ~ "Unsafe",
                   (x == 4) ~ "Unsafe",
                   (x == -9) ~ "Not working or working just from home",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(safeworkplace= work(safeworkplace))


# Tabla de contingencia

# Not working or working just from home Somewhat safe    Unsafe Very safe
# 0                             0.3692511     0.3169572 0.3009259 0.3636364
# 1                             0.6307489     0.6830428 0.6990741 0.6363636

# .................. Variable CAW021_ ..........................................

# Esta variable indica si, en caso de que el encuestado trabaje durante el covid,
# ha reducido sus horas laborales desde el comienzo de la pandemia.
# Si ha cerrado el negocio o ha dejado el trabajo por la pandemia, el encuestado
# marca que si en esta pregunta.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, reducedworkinghours=caw021_)


Sharedata_final %>% count(reducedworkinghours) %>% mutate(prop = n / sum(n)*100)

# 
# # A tibble: 5 x 3
# caw021_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 12516 88.4   
# 2 -2 [-2. Refusal]                          4  0.0282
# 3 -1 [-1. Don't know]                       4  0.0282
# 4  1 [1. Yes]                             441  3.11  
# 5  5 [5. No]                             1201  8.48 

# Conviertimos la categoria -9 (no aplica o bien porque el encuestado no 
# trabajaba antes de la pandemia.


work <- function(x) {
  dplyr::case_when((x == 1) ~ "Yes", 
                   (x == 5) ~ "No",
                   (x == -9) ~ "Not working",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(reducedworkinghours= work(reducedworkinghours))


# Tabla de contingencia

#          No Not working       Yes
# 0 0.3372190   0.3721636 0.2585034
# 1 0.6627810   0.6278364 0.7414966

# .................. Variable CAW024_ ..........................................

# Esta variable indica si, en caso de que el encuestado trabaje durante el covid,
# ha aumentado sus horas laborales desde el comienzo de la pandemia.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, increasedworkinghours=caw024_)


Sharedata_final %>% count(increasedworkinghours) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# caw024_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 12516 88.4   
# 2 -2 [-2. Refusal]                          3  0.0212
# 3 -1 [-1. Don't know]                       6  0.0424
# 4  1 [1. Yes]                             251  1.77  
# 5  5 [5. No]                             1390  9.81

# Conviertimos la categoria -9 (no aplica o bien porque el encuestado no 
# trabajaba antes de la pandemia.


Sharedata_final <- Sharedata_final %>% mutate(increasedworkinghours= work(increasedworkinghours))


# la eliminamos por tener muy poco porcentaje de si's y poca relacion con la variable
# objetivo

# Tabla de contingencia

#          No Not working       Yes
# 0 0.3107914   0.3721636 0.3426295
# 1 0.6892086   0.6278364 0.6573705


Sharedata_final <- Sharedata_final %>% select(-increasedworkinghours)


# ................. ECONOMIC SITUATION .........................................

# Estudiemos las variables relacionadas con los datos financieros de los 
# encuestados

# ............ Variable CAHH017_ ...............................................

# Esta variable indica el mayor sueldo que recibio la vivienda antes del covid 
# (tras impuestos). Como cada pais tiene su moneda, es dificil comparar los 
# distintos paises. Por ello, en el dataset se incluye la variable "Exrate" que
# indica la conversion de la moneda del pais a euros, tan solo tenemos que 
# multiplicar esta variable por la tasa de conversion y tendremos todos los 
# ingresos en euros. 


Sharedata_final <- Sharedata_final %>% mutate(incomebeforecorona = ifelse(
  incomebeforecorona == -9999999 , incomebeforecorona, ifelse(
    incomebeforecorona == -9999991 , incomebeforecorona, ifelse(
      incomebeforecorona == -9999992 , incomebeforecorona, incomebeforecorona*exrate
                                 ))))

# Como tenemos gente que se niega a contesrtar, consideramos que esta categoria
# es importante y, por lo tanto, la dejamos como tal y realizamos intervalos 



g1 <- ggplot(Sharedata_final, aes(x=incomebeforecorona)) +
  geom_histogram(col='black', fill="#8cc8bc") +
  scale_x_continuous(limits = c(0, 100000)) +
  labs(x = "Mayor sueldo mes en la vivienda antes del covid", y = 'Frecuencias', 
       title = 'Histograma variable incomebeforecorona',
       subtitle = 'incomebeforecorona antes de la agrupación',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)')) 

g1 <- g1 + labs(y="", caption = "")


incomeb <- function(x) {
  dplyr::case_when((x >= 0 & x <= 3000) ~ "Low", 
                   (x >=3000 & x<=20000) ~ "Average",
                   (x > 20000) ~ "High",
                   (x == -9999991) ~ "Refusal",
                   (x == -9999992) ~ "Refusal",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(incomebeforecorona= incomeb(incomebeforecorona))
Sharedatalevels <- Sharedata_final
Sharedatalevels$incomebeforecorona <- factor(Sharedatalevels$incomebeforecorona , 
                                             levels=c("Refusal", "Low", 
                                                                                           
                                                      "Average", "High"))
g2 <- ggplot(data=subset(Sharedatalevels, !is.na(incomebeforecorona)), 
             aes(x = (incomebeforecorona), fill=(incomebeforecorona))) +
  geom_bar(col='black') +
  scale_fill_manual(values = c("#D3D3D3","#224b5e", "#6d2f20","#df7e66")) +
  theme(legend.position='none')+
  labs(x = "Mayor sueldo mes en la vivienda antes del covid", y = 'Count', 
       title = 'Histograma variable incomebeforecorona',
       fill = "incomebeforecorona",
       subtitle = 'incomebeforecorona después de la agrupación',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)')) 


library(cowplot)
plot_grid(g1,g2, align = "h", ncol= 2 , rel_widths = c(1/2,1/2))




# ............. Variable cae003_ ...............................................

# Esta variable indica si el encuestado ha recibido ayuda financiera (del 
# gobierno, amigos, familiares...)


Sharedata_final %>% count(financialsupport) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cae003_     n   prop
# <dbl> <int>  <dbl>
#   1      -9   289  2.04 
# 2      -2   133  0.939
# 3      -1   116  0.819
# 4       1 [1.Yes]  1100  7.77 
# 5       5 [5.No]   12528 88.4


# cambiamos el 5 de "No" por cero y los no sabe, no contesta o no aplica por NAs 
# al tener muy poca proporción

income <- function(x) {
  dplyr::case_when((x == 1) ~ 1, 
                   (x == 5) ~ 0,
                   TRUE ~ as.numeric(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(financialsupport= income(financialsupport))

#Estudioamos su ralcion con la variable objetivo
# Los se niega los transformamos a NAs por no tener unra relacion clara con la variable 
# objetigvo
prop.table(table(Sharedata_final$sadness, Sharedata_final$financialsupport), margin=2)


# 0         1
# 0 0.3681354 0.3154545
# 1 0.6318646 0.6845455


# ............ Variable CAE005e ...............................................

# Esta variable indica el menor sueldo que recibio la vivienda antes del covid 
# (tras impuestos). Como cada pais tiene su moneda, es dificil comparar los 
# distintos paises. Por ello, en el dataset se incluye la variable "Exrate" que
# indica la conversion de la moneda del pais a euros, tan solo tenemos que 
# multiplicar esta variable por la tasa de conversion y tendremos todos los 
# ingresos en euros. 


Sharedata_final <- Sharedata_final %>% mutate(lowestincomesincecorona = ifelse(
  lowestincomesincecorona == -9999999 , lowestincomesincecorona, ifelse(
    lowestincomesincecorona == -9999991 , lowestincomesincecorona, ifelse(
      lowestincomesincecorona == -9999992 , lowestincomesincecorona, 
      lowestincomesincecorona*exrate
    ))))


# Como tenemos gente que se niega a contesrtar, consideramos que esta categoria
# es importante y, por lo tanto, la dejamos como tal y realizamos intervalos 
# en esta variable. 



g5 <- ggplot(Sharedata_final, aes(x=lowestincomesincecorona)) +
  geom_histogram(col='black', fill="#8cc8bc") +
  scale_x_continuous(limits = c(0, 100000)) +
  labs(x = "Menor sueldo mes en la vivienda desde el covid", y = 'Count', 
       title = 'Histograma variable lowestincomesincecorona',
       subtitle = 'lowestincomesincecorona antes de la agrupación',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)')) 

g5 <- g5 + labs(y="", caption = "")

incomes <- function(x) {
  dplyr::case_when((x >= 0 & x <= 1000) ~ "Low", 
                   (x >=1000 & x<=20000) ~ "Average",
                   (x > 20000) ~ "High",
                   (x == -9999991) ~ "Refusal",
                   (x == -9999992) ~ "Refusal",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(lowestincomesincecorona= 
                                                incomeb(lowestincomesincecorona))

Sharedatalevels <- Sharedata_final
Sharedatalevels$lowestincomesincecorona <- factor(Sharedatalevels$lowestincomesincecorona , 
                                                  levels=c("Refusal", "Low", 
                                                          "Average", "High"))
g6 <- ggplot(data=subset(Sharedatalevels, !is.na(lowestincomesincecorona)), 
             aes(x = (lowestincomesincecorona), fill=(lowestincomesincecorona))) +
  geom_bar(col='black') +
  scale_fill_manual(values = c("#D3D3D3","#224b5e", "#6d2f20","#df7e66")) +
  theme(legend.position='none')+
  labs(x = "Menor sueldo mes en la vivienda desde el covid", y = 'Count', 
       title = 'Histograma variable lowestincomesincecorona',
       fill = "incomebeforecorona",
       subtitle = 'lowestincomesincecorona después de la agrupación',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)')) 


library(cowplot)
plot_grid(g5,g6, align = "h", ncol= 2 , rel_widths = c(1/2,1/2))



# ............. Variable caco007_ ...............................................

# Esta variable indica si el encuestado, pensando en todos los ingresos de la u
# unidad familiar, cree que ha tenido dificultades para llegar a fin de mes 
# desde que comenzo la pandemia.


Sharedata_final %>% count(makeendsmeet) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 8 x 3
# caco007_     n     prop
# <dbl> <int>    <dbl>
#   1       -9   289  2.04   
# 2       -2    59  0.416  
# 3       -1    78  0.551  
# 4        1 [With great difficulty] 1262  8.91   
# 5        2 [With some difficulty] 3262 23.0    
# 6        3 [Fairly easily]       4680 33.0    
# 7        4 [Easily]              4535 32.0    
# 8       NA     1  0.00706


# convertimos en categorias y pasamos no sabe, no contesta o no aplica a NA 
# LOS NO APLICA SON EL RESULTADO DE LA UNION CON LOS FINANCIAL RESPONDENT QUE 
# NO EXISTEN (LOS 289)


income2 <- function(x) {
  dplyr::case_when((x == 1) ~ "Great difficulty", 
                   (x == 2) ~ "Some difficulty",
                   (x == 3) ~ "Fairly easily",
                   (x == 4) ~ "Easily",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(makeendsmeet= income2(makeendsmeet))

#Estudioamos su ralcion con la variable objetivo
# Los se niega los transformamos a NAs por no tener unra relacion clara con la variable 
# objetigvo
prop.table(table(Sharedata_final$sadness, Sharedata_final$makeendsmeet), margin=2)

# Easily Fairly easily Great difficulty Some difficulty
# 0 0.3816979     0.3649573        0.3399366       0.3436542
# 1 0.6183021     0.6350427        0.6600634       0.6563458


# ............. Variable cae011_ ...............................................

# Esta variable indica si el encuestado ha tenido que posponer pagos como la 
# renta o facturas, en caso de que haya tenido dificultades en llegar a fin de 
# mes

Sharedata_final %>% count(postponepayment) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 6 x 3
# cae011_     n    prop
# <dbl> <int>   <dbl>
#   1      -9  9504 67.1   
# 2      -2     3  0.0212
# 3      -1     9  0.0635
# 4       1   414  2.92  
# 5       5  4098 28.9   
# 6      NA   138  0.974 

# tiene muy poca variabilidad por lo que la eliminamos

Sharedata_final <- Sharedata_final %>% select(-postponepayment)

# ............. Variable cae012_ ...............................................

# Esta variable indica si el encuestado ha tenido que utilizar sus ahorros para
# cubrir los gastos del dia a dia, en caso de que haya tenido dificultades en 
# llegar a fin de mes


Sharedata_final %>% count(dipsavings) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 6 x 3
# cae012_     n    prop
# <dbl> <int>   <dbl>
#   1      -9  9504 67.1   
# 2      -2    17  0.120 
# 3      -1     8  0.0565
# 4       1   812  5.73  
# 5       5  3687 26.0   
# 6      NA   138  0.974 

# Cambiamos lo aplicables por "not difficulty making ends meet" ya que esta 
# pregunta es solo para aquellos que ha tenido dificultades para llegar a fin de mes

income4 <- function(x) {
  dplyr::case_when((x == 1) ~ "Yes", 
                   (x == 5) ~ "No",
                   (x == -9) ~ "Not difficulty making ends meet",
                   TRUE ~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(dipsavings= income4(dipsavings))


#Estudioamos su ralcion con la variable objetivo
# Los se niega los transformamos a NAs por no tener unra relacion clara con la variable 
# objetigvo
prop.table(table(Sharedata_final$sadness, Sharedata_final$dipsavings), margin=2)

# No Not difficulty making ends meet       Yes
# 0 0.3411988                       0.3758418 0.3485222
# 1 0.6588012                       0.6241582 0.6514778


# ..............................................................................
#                   SOCIAL NETWORS
# .............................................................................



# ............... Variable cas003_1 .............................................


# Esta variable indica cada cuanto tiempo el encuestado ha estado en contacto 
# con sus hijos de fuera de su vivienda cara a cara.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, personalcontactchildren=cas003_1)


Sharedata_final %>% count(personalcontactchildren) %>% mutate(prop = n / sum(n)*100)


# # A tibble: 9 x 3
# cas003_1     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]                21  0.148 
# 2 -1 [-1. Don't know]             17  0.120 
# 3  1 [1. Daily]                 2538 17.9   
# 4  2 [2. Several times a week]  1883 13.3   
# 5  3 [3. About once a week]     2135 15.1   
# 6  4 [4. Less often]            3726 26.3   
# 7  5 [5. Never]                 2492 17.6   
# 8 99 [99. Not applicable]       1352  9.54  
# 9 NA                               2  0.0141


# El 99 indica que son observaciones missing por diseño de la encuesta, con lo 
# que las transformamos en NAs. De igual manera, transformamos en NAs los "no
# sabe o no contesta". 


social1 <- function(x) {
  dplyr::case_when((x == 1) ~ "Daily", 
                   (x == 2) ~ "Several tiemes a week",
                   (x == 3) ~ "About once a week",
                   (x == 4) ~ "Less often",
                   (x == 5) ~ "Never",
                   (x== 99) ~ "No children",
                   TRUE ~ as.character(NA))
}



Sharedata_final <- Sharedata_final %>% mutate(personalcontactchildren= social1(personalcontactchildren))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$personalcontactchildren), margin=2)

# About once a week     Daily Less often     Never No children Several tiemes a week
# 0         0.3793911 0.4093775  0.3354804 0.3318620   0.3964497             0.3701540
# 1         0.6206089 0.5906225  0.6645196 0.6681380   0.6035503             0.6298460

# ............... Variable cas003_2 .............................................


# Esta variable indica cada cuanto tiempo el encuestado ha estado en contacto 
# con sus padres de fuera de su vivienda cara a cara.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, personalcontactparent=cas003_2)

Sharedata_final %>% count(personalcontactparent) %>% mutate(prop = n / sum(n)*100)


# # A tibble: 9 x 3
# cas003_2     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]                17  0.120 
# 2 -1 [-1. Don't know]             25  0.176 
# 3  1 [1. Daily]                  315  2.22  
# 4  2 [2. Several times a week]   219  1.55  
# 5  3 [3. About once a week]      255  1.80  
# 6  4 [4. Less often]             618  4.36  
# 7  5 [5. Never]                 1387  9.79  
# 8 99 [99. Not applicable]      11328 80.0   
# 9 NA                               2  0.0141

#  De igual manera, transformamos en NAs los "no
# sabe o no contesta". Por otro lado, hay categorias con poca representatividad,
# por lo tanto agrupamos las categorias en "Often", "Sometimes" y "Never"


social <- function(x) {
  dplyr::case_when((x == 1) ~ "Often", 
                   (x == 2) ~ "Often",
                   (x == 3) ~ "Often",
                   (x == 4) ~ "Sometimes",
                   (x == 5) ~ "Never",
                   (x== 99) ~ "No parents",
                   TRUE ~ as.character(NA))
}



Sharedata_final <- Sharedata_final %>% mutate(personalcontactparent= social(personalcontactparent))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$personalcontactparent), margin=2)

# Never No parents     Often Sometimes
# 0 0.3229993  0.3764124 0.3409379 0.2944984
# 1 0.6770007  0.6235876 0.6590621 0.7055016


# ............... Variable cas003_3 .............................................


# Esta variable indica cada cuanto tiempo el encuestado ha estado en contacto 
# con sus familiares (no padres o hijos) de fuera de su vivienda cara a cara.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, personalcontactfamily=cas003_3)


Sharedata_final %>% count(personalcontactfamily) %>% mutate(prop = n / sum(n)*100)


# # A tibble: 9 x 3
# cas003_3     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]                14  0.0988
# 2 -1 [-1. Don't know]             17  0.120 
# 3  1 [1. Daily]                  352  2.48  
# 4  2 [2. Several times a week]   613  4.33  
# 5  3 [3. About once a week]     1016  7.17  
# 6  4 [4. Less often]            4839 34.2   
# 7  5 [5. Never]                 6745 47.6   
# 8 99 [99. Not applicable]        568  4.01  
# 9 NA                               2  0.0141

#  De igual manera, transformamos en NAs los "no
# sabe o no contesta". Por otro lado, hay categorias con poca representatividad,
# por lo tanto agrupamos las categorias en "Often", "Sometimes" y "Never"


social3 <- function(x) {
  dplyr::case_when((x == 1) ~ "Often", 
                   (x == 2) ~ "Often",
                   (x == 3) ~ "Often",
                   (x == 4) ~ "Sometimes",
                   (x == 5) ~ "Never",
                   (x== 99) ~ "No relatives",
                   TRUE ~ as.character(NA))
}



Sharedata_final <- Sharedata_final %>% mutate(personalcontactfamily= social3(personalcontactfamily))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$personalcontactfamily), margin=2)

# Never No relatives     Often Sometimes
# 0 0.3369904    0.3908451 0.4063604 0.3843769
# 1 0.6630096    0.6091549 0.5936396 0.6156231



# ............... Variable cas003_4 .............................................


# Esta variable indica cada cuanto tiempo el encuestado ha estado en contacto 
# con sus amigos, vecinos, compañeros...  fuera de su vivienda cara a cara.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, personalcontactnonrelatives=cas003_4)


Sharedata_final %>% count(personalcontactnonrelatives) %>% mutate(prop = n / sum(n)*100)


# # A tibble: 9 x 3
# cas003_4     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]                10  0.0706
# 2 -1 [-1. Don't know]             23  0.162 
# 3  1 [1. Daily]                  813  5.74  
# 4  2 [2. Several times a week]  1572 11.1   
# 5  3 [3. About once a week]     1527 10.8   
# 6  4 [4. Less often]            4803 33.9   
# 7  5 [5. Never]                 5200 36.7   
# 8 99 [99. Not applicable]        216  1.52  
# 9 NA                               2  0.0141

#  De igual manera, transformamos en NAs los "no
# sabe o no contesta". 

social3 <- function(x) {
  dplyr::case_when((x == 1) ~ "Daily", 
                   (x == 2) ~ "Several times a week",
                   (x == 3) ~ "About once a week",
                   (x == 4) ~ "Less often",
                   (x == 5) ~ "Never",
                   (x== 99) ~ "No non relatives",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(personalcontactnonrelatives= 
                                                social3(personalcontactnonrelatives))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$personalcontactnonrelatives), margin=2)

# About once a week     Daily Less often     Never No non relatives Several times a week
# 0         0.3948919 0.4157442  0.3714345 0.3382692        0.4861111            0.3625954
# 1         0.6051081 0.5842558  0.6285655 0.6617308        0.5138889            0.6374046


# ............... Variable cas004_1 .............................................


# Esta variable indica cada cuanto tiempo el encuestado ha estado en contacto 
# con sus hijos por telefono, email o cualquier aparato electronico.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, onlinecontactchildren=cas004_1)


Sharedata_final %>% count(onlinecontactchildren) %>% mutate(prop = n / sum(n)*100)


# # A tibble: 9 x 3
# cas004_1     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]                19  0.134 
# 2 -1 [-1. Don't know]             23  0.162 
# 3  1 [1. Daily]                 5747 40.6   
# 4  2 [2. Several times a week]  4113 29.0   
# 5  3 [3. About once a week]     1431 10.1   
# 6  4 [4. Less often]             708  5.00  
# 7  5 [5. Never]                  626  4.42  
# 8 99 [99. Not applicable]       1497 10.6   
# 9 NA                               2  0.0141

# De igual manera, transformamos en NAs los "no
# sabe o no contesta".

social5 <- function(x) {
  dplyr::case_when((x == 1) ~ "Daily", 
                   (x == 2) ~ "Several times a week",
                   (x == 3) ~ "About once a week",
                   (x == 4) ~ "Less often",
                   (x == 5) ~ "Never",
                   (x == 99) ~ "No children",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(onlinecontactchildren= social5(onlinecontactchildren))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$onlinecontactchildren), margin=2)

# About once a week     Daily Less often     Never No children Several times a week
# 0         0.4255765 0.3090308  0.4901130 0.4680511   0.3934536            0.3749088
# 1         0.5744235 0.6909692  0.5098870 0.5319489   0.6065464            0.6250912

# ............... Variable cas004_2 .............................................


# Esta variable indica cada cuanto tiempo el encuestado ha estado en contacto 
# con sus padres por telefono, email o cualquier aparato electronico.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, onlinecontactparents=cas004_2)


Sharedata_final %>% count(onlinecontactparents) %>% mutate(prop = n / sum(n)*100)


# # A tibble: 9 x 3
# cas004_2     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]                14  0.0988
# 2 -1 [-1. Don't know]             44  0.311 
# 3  1 [1. Daily]                  611  4.31  
# 4  2 [2. Several times a week]   563  3.97  
# 5  3 [3. About once a week]      333  2.35  
# 6  4 [4. Less often]             220  1.55  
# 7  5 [5. Never]                  977  6.90  
# 8 99 [99. Not applicable]      11402 80.5   
# 9 NA                               2  0.0141

# El 99 indica que son observaciones missing por diseño de la encuesta, con lo 
# que las transformamos en NAs. De igual manera, transformamos en NAs los "no
# sabe o no contesta". Por otro lado, hay categorias con poca representatividad,
# por lo tanto recategorizamos en "Todos los dias", "A veces" y
# "Nunca"

social6 <- function(x) {
  dplyr::case_when((x == 1) ~ "Daily", 
                   (x == 2) ~ "Sometimes",
                   (x == 3) ~ "Sometimes",
                   (x == 4) ~ "Sometimes",
                   (x == 5) ~ "Never",
                   (x == 99) ~ "No parents",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(onlinecontactparents= social6(onlinecontactparents))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$onlinecontactparents), margin=2)

# Daily     Never No parents Sometimes
# 0 0.2602291 0.3490276  0.3779162 0.3127240
# 1 0.7397709 0.6509724  0.6220838 0.6872760


# ............... Variable cas004_3 .............................................


# Esta variable indica cada cuanto tiempo el encuestado ha estado en contacto 
# con otros familiares online

# La renombramos

Sharedata_final <-  rename(Sharedata_final, onlinecontactrelatives=cas004_3)



Sharedata_final %>% count(onlinecontactrelatives) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 9 x 3
# cas004_3     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]                13  0.0918
# 2 -1 [-1. Don't know]             32  0.226 
# 3  1 [1. Daily]                 1097  7.74  
# 4  2 [2. Several times a week]  3541 25.0   
# 5  3 [3. About once a week]     3381 23.9   
# 6  4 [4. Less often]            3941 27.8   
# 7  5 [5. Never]                 1652 11.7   
# 8 99 [99. Not applicable]        507  3.58  
# 9 NA                               2  0.014 



#  De igual manera, transformamos en NAs los "no
# sabe o no contesta". 

social7 <- function(x) {
  dplyr::case_when((x == 1) ~ "Daily", 
                   (x == 2) ~ "Several times a week",
                   (x == 3) ~ "About once a week",
                   (x == 4) ~ "Less often",
                   (x == 5) ~ "Never",
                   (x == 99) ~ "No relatives",
                   TRUE ~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(onlinecontactrelatives= social7(onlinecontactrelatives))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$onlinecontactrelatives), margin=2)

# About once a week     Daily Less often     Never No relatives Several times a week
# 0         0.3493049 0.2935278  0.4181680 0.4449153    0.4260355            0.2973736
# 1         0.6506951 0.7064722  0.5818320 0.5550847    0.5739645            0.7026264

# ............... Variable cas004_4 .............................................


# Esta variable indica cada cuanto tiempo el encuestado ha estado en contacto 
# con amigos, compañeros, vecinos.. por telefono, email o cualquier aparato 
# electronico.

# La renombramos

Sharedata_final <-  rename(Sharedata_final, onlinecontactnonrelatives=cas004_4)



Sharedata_final %>% count(onlinecontactnonrelatives) %>% mutate(prop = n / sum(n)*100)

# 
# # A tibble: 9 x 3
# cas004_4     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]                11  0.0777
# 2 -1 [-1. Don't know]             37  0.261 
# 3  1 [1. Daily]                 1022  7.21  
# 4  2 [2. Several times a week]  3363 23.7   
# 5  3 [3. About once a week]     3177 22.4   
# 6  4 [4. Less often]            4007 28.3   
# 7  5 [5. Never]                 2275 16.1   
# 8 99 [99. Not applicable]        272  1.92  
# 9 NA                               2  0.0141


#  De igual manera, transformamos en NAs los "no
# sabe o no contesta". 

social8 <- function(x) {
  dplyr::case_when((x == 1) ~ "Daily", 
                   (x == 2) ~ "Several times a week",
                   (x == 3) ~ "About once a week",
                   (x == 4) ~ "Less often",
                   (x == 5) ~ "Never",
                   (x == 99) ~ "No non relatives",
                   TRUE ~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(onlinecontactnonrelatives= social8(onlinecontactnonrelatives))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$onlinecontactnonrelatives), margin=2)

# About once a week     Daily Less often     Never No non relatives Several times a week
# 0         0.3550519 0.3062622  0.3930621 0.4320879        0.4926471            0.3047874
# 1         0.6449481 0.6937378  0.6069379 0.5679121        0.5073529            0.6952126
#  

# ............... Variable cas010_ .............................................


# Esta variable indica si el encuestado, durante la pandemia, ha ayudado a otros
# a conseguri productos basicos (comida, medicamentos...)


# La renombramos

Sharedata_final <-  rename(Sharedata_final, giveproducts=cas010_)


Sharedata_final %>% count(giveproducts) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cas010_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -2 [-2. Refusal]        5  0.0353
# 2 -1 [-1. Don't know]     3  0.0212
# 3  1 [1. Yes]          2035 14.4   
# 4  5 [5. No]          12121 85.6   
# 5 NA                      2  0.0141

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(giveproducts= depuracion(giveproducts))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$giveproducts), margin=2)

# 0         1
# 0 0.3718340 0.3287469
# 1 0.6281660 0.6712531

# ............... Variable cas012_ .............................................


# Esta variable indica si el encuestado, durante la pandemia, ha cuidado a otras 
# personas ajenas a su casa

# La renombramos

Sharedata_final <-  rename(Sharedata_final, providepersonalcare=cas012_)



Sharedata_final %>% count(providepersonalcare) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# cas012_     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -2 [-2. Refusal]        1  0.00706
# 2 -1 [-1. Don't know]     1  0.00706
# 3  1 [1. Yes]           589  4.16   
# 4  5 [5. No]          13573 95.8    
# 5 NA                      2  0.0141 

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(providepersonalcare= depuracion(providepersonalcare))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$providepersonalcare), margin=2)

# 0         1
# 0 0.3673469 0.3259762
# 1 0.6326531 0.6740238

# ............... Variable cas015_ .............................................


# Esta variable indica si el encuestado, desde el comienzo de la pandemia, 
# ha participado en actividades voluntarias

# La renombramos

Sharedata_final <-  rename(Sharedata_final, volunteering=cas015_)

Sharedata_final %>% count(volunteering) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# volunteering     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -2 [-2. Refusal]        1  0.00706
# 2 -1 [-1. Don't know]     1  0.00706
# 3  1 [1. Yes]           533  3.76   
# 4  5 [5. No]          13629 96.2    
# 5 NA                      2  0.0141 

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(volunteering= depuracion(volunteering))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$volunteering), margin=2)

# 0         1
# 0 0.3677452 0.3114447
# 1 0.6322548 0.6885553

# ............... Variable cas020_ .............................................


# Esta variable indica si el encuestado, durante la pandemia, ha sido ayudado por 
# personas ajenas a su vivienda a obtener necesidades como comida, agua, arreglos
# en casa?

# La renombramos

Sharedata_final <-  rename(Sharedata_final, receiveproducts=cas020_)

Sharedata_final %>% count(receiveproducts) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 4 x 3
# cas020_     n    prop
# <dbl+lbl> <int>   <dbl>
#   1 -1 [-1. Don't know]     2  0.0141
# 2  1 [1. Yes]          5291 37.3   
# 3  5 [5. No]           8871 62.6   
# 4 NA                      2  0.0141

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(receiveproducts= depuracion(receiveproducts))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$receiveproducts), margin=2)

# 0         1
# 0 0.3626423 0.3706294
# 1 0.6373577 0.6293706

# ............... Variable cas025_ .............................................

# Esta variable indica si el encuestado, durante la pandemia, ha redibido cuidado de otras 
# personas ajenas a su casa

# La renombramos

Sharedata_final <-  rename(Sharedata_final, receivedpersonalcare=cas025_)


Sharedata_final %>% count(receivedpersonalcare) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 5 x 3
# receivedpersonalcare     n    prop
# <dbl+lbl> <int>   <dbl>
#   1  -2 [-2. Refusal]        4  0.0282
# 2  -1 [-1. Don't know]     2  0.0141
# 3   1 [1. Yes]          1419 10.0   
# 4   5 [5. No]          12739 89.9   
# 5  NA                      2  0.0141

# Convierto el numero 5 en 0   y los no se sabe o se niega a contestar en NAs

Sharedata_final <- Sharedata_final %>% mutate(receivedpersonalcare= depuracion(receivedpersonalcare))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$receivedpersonalcare), margin=2)

# 0         1
# 0 0.3575634 0.4369274
# 1 0.6424366 0.5630726


# ............... Variable cas026_ .............................................

# Esta variable indica si el encuestado, en caso de que haya recibido ayuda de
# otras personas, ha tenido disificultades en recibirla comparada con antes de
# la pandemia


# La renombramos

Sharedata_final <-  rename(Sharedata_final, difficultyreceivedcared=cas026_)


Sharedata_final %>% count(difficultyreceivedcared) %>% mutate(prop = n / sum(n)*100)

# # A tibble: 6 x 3
# difficultyreceivedcared     n     prop
# <dbl+lbl> <int>    <dbl>
#   1 -9 [-9. Not applicable (qqn routing)] 12745 90.0    
# 2 -2 [-2. Refusal]                          1  0.00706
# 3 -1 [-1. Don't know]                       3  0.0212 
# 4  1 [1. Yes]                             311  2.20   
# 5  5 [5. No]                             1104  7.79   
# 6 NA                                        2  0.0141 

# El porcentaje de si's es muy pequeño, por lo que la eliminamos.

Sharedata_final <- Sharedata_final %>% select(-difficultyreceivedcared)


# ............... Variable hhsize_update_ca .............................................

# Esta variable indica el tamaño familiar del encuestado


Sharedata_final %>% count(hhsize_update_ca) %>% mutate(prop = n / sum(n)*100)

# Estudiamos su relacion con la variable objetivo
h <-  Sharedata_final %>% filter(!is.na(hhsize_update_ca)) %>%  
  dplyr::count(hhsize_update_ca, sadness) %>% group_by(hhsize_update_ca)
h$lab <- paste0(round(prop.table(table(Sharedata_final$sadness, 
                                       Sharedata_final$hhsize_update_ca), 
                                 margin=2) * 100, 2), sep="%")


# Porcentaje de cada categoría de la variabe.
hhsizesm<- as.data.frame.matrix(prop.table(table(Sharedata_final$sadness, 
                                                 Sharedata_final$hhsize_update_ca), margin=2))
hhsizesm <- as.data.frame(t(hhsizesm))

g3 <- h %>%
  ggplot(aes(x = factor(hhsize_update_ca),n, fill=factor(sadness))) + 
  geom_col(color="black") + 
  labs(y = "count",
       x = "Tamaño vivienda",
       color = "sadness",
       title = "Tamaño vivienda vs Tristeza del Encuestado",
       subtitle = "hhsize_update_ca vs sadness.",
       fill = "Sadness",
       caption =
         paste0("Autor: Lucía Pascual Martínez | ",
                "Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)")) +
  scale_fill_manual(values=c("#c399a2", "#7d87b2"))+
  geom_text(aes(label=lab),position='stack',vjust=1.8, check_overlap = TRUE)

library(ggpmisc)
g3 <- g3 + theme(legend.position="none") + labs(caption =
                                                  paste0("",
                                                         ""))+annotate(geom = "table",
                                                                       x = 9,
                                                                       y =6000,
                                                                       size=4,
                                                                       table.theme = ttheme_gtlight,
                                                                       label = list(hhsizesm))


#Estudioamos su ralcion con la variable objetivo
as.data.frame.matrix(prop.table(table(Sharedata_final$sadness, 
                                      Sharedata_final$hhsize_update_ca), margin=2))


# unificamos las categorias del 5 en adelante


Sharedata_final <- Sharedata_final %>% mutate(hhsize_update_ca = ifelse(
     hhsize_update_ca >=5, 5, hhsize_update_ca
))

# Porcentaje de cada categoría de la variabe.
hhsizesmd<- Sharedata_final %>% count(hhsize_update_ca) %>% mutate(prop = n / sum(n)*100)
# Porcentaje de cada categoría de la variabe.
hhsizesmd<- as.data.frame.matrix(prop.table(table(Sharedata_final$sadness, 
                                                  Sharedata_final$hhsize_update_ca), margin=2))
hhsizesmd<- as.data.frame(t(hhsizesmd))
# Estudiamos su relacion con la variable objetivo
h2 <-  Sharedata_final %>% filter(!is.na(hhsize_update_ca)) %>%  
  dplyr::count(hhsize_update_ca, sadness) %>% group_by(hhsize_update_ca)
h2$lab <- paste0(round(prop.table(table(Sharedata_final$sadness, 
                                        Sharedata_final$hhsize_update_ca), margin=2) * 100, 2), sep="%")

g4 <- h2 %>%
  ggplot(aes(x = factor(hhsize_update_ca),n, fill=factor(sadness))) + 
  geom_col(color="black") + 
  labs(y = "count",
       x = "Tamaño vivienda",
       color = "sadness",
       title = "Tamaño vivienda vs Tristeza del Encuestado",
       subtitle = "hhsize_update_ca vs sadness (tras agrupar valores).",
       fill = "Sadness",
       caption =
         paste0("Autor: Lucía Pascual Martínez | ",
                "Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)")) +
  scale_fill_manual(values=c("#c399a2", "#7d87b2"))+
  geom_text(aes(label=lab),position='stack',vjust=1.5, check_overlap = TRUE)+annotate(geom = "table",
                                                                                       x = 6,
                                                                                       y =5000,
                                                                                       size=3,
                                                                                       table.theme = ttheme_gtlight,
                                                                                       label = list(hhsizesmd))


library(gridExtra)
library(cowplot)
plot_grid(g3,g4, align = "h", ncol= 2)

# 1         2         3         4         5
# 0 0.4097441 0.3399248 0.3427198 0.3515358 0.4292683
# 1 0.5902559 0.6600752 0.6572802 0.6484642 0.5707317


# ............... Variable satisfaction_life .............................................


# Indica la satisfacción con la vida que tiene el encuestado del 1 al 10


Sharedata_final %>% count(satisfactionlife) %>% mutate(prop = n / sum(n)*100)

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$satisfactionlife), margin=2)

#  Comvertirmos en categorica, tenemos muchos NAs resultantes de la union con la 
# ptra base de datos para los cuales imputar no tendria sentido
# los refusal y don't know si los dejamos como NAs

satis <- function(x) {
  dplyr::case_when((x <=4) ~ "Low", 
                   (x >4 & x<=7) ~ "Medium",
                   (x >=7 & x<=10) ~ "High",
                  (is.na(x)) ~ "No data",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(satisfactionlife= satis(satisfactionlife))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$satisfactionlife), margin=2)

# High       Low    Medium   No data
# 0 0.3175376 0.4656918 0.4015237 0.3657274
# 1 0.6824624 0.5343082 0.5984763 0.6342726

# Estudiamos su relacion con la variable objetivo
h3 <-  Sharedata_final %>% filter(!is.na(satisfactionlife)) %>%  
  dplyr::count(satisfactionlife, sadness) %>% group_by(satisfactionlife)
h3$lab <- paste0(round(prop.table(table(Sharedata_final$sadness, 
                                        Sharedata_final$satisfactionlife), margin=2) * 100, 2), sep="%")

h3 %>%
  ggplot(aes(x = factor(satisfactionlife),n, fill=factor(sadness))) + 
  geom_col(color="black") + 
  labs(y = "count",
       x = "Satisfación con la vida",
       color = "sadness",
       title = "Satisfación con la vida vs Tristeza del Encuestado",
       subtitle = "satisfactionlife vs sadness (tras agrupar valores).",
       fill = "Sadness",
       caption =
         paste0("Autor: Lucía Pascual Martínez | ",
                "Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)")) +
  scale_fill_manual(values=c("#c399a2", "#7d87b2"))+
  geom_text(aes(label=lab),position='stack',vjust=1.5, check_overlap = TRUE)



# ............... Variable leftout .............................................


# Indica si el encuestado se ha sentido excluido alguna vez


Sharedata_final %>% count(leftout) %>% mutate(prop = n / sum(n)*100)


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$leftout), margin=2)


# -2        -1         1         2         3         4
# 0 0.4615385 0.4788732 0.3952282 0.3721627 0.3611111 0.3464744
# 1 0.5384615 0.5211268 0.6047718 0.6278373 0.6388889 0.6535256



# Definimos sus categorias, dejamos los no sabe o se niega como NA al no tener 
# especial relacion con la variable objetivo y tratarse tan solo de un 1.59% de los
# datos.

categories <- function(x) {
  dplyr::case_when((x == 1) ~ "Often", 
                   (x == 2) ~ "Sometimes",
                   (x == 3) ~ "Rarely",
                   (x == 4) ~ "Never",
                   (is.na(x)) ~ "No data",
                   TRUE~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(leftout= categories(leftout))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$leftout), margin=2)

# Never     Often    Rarely Sometimes
# 0 0.3464744 0.3952282 0.3611111 0.3721627
# 1 0.6535256 0.6047718 0.6388889 0.6278373

# ............... Variable lookforwardeachday .............................................


# Indica si el encuestado tiene ganas de empezar el dia siguiente

Sharedata_final %>% count(lookforwardeachday) %>% mutate(prop = n / sum(n)*100)

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$lookforwardeachday), margin=2)

# Definimos sus categorias

Sharedata_final <- Sharedata_final %>% mutate(lookforwardeachday= categories(lookforwardeachday))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$lookforwardeachday), margin=2)

# Never     Often    Rarely Sometimes
# 0 0.4542587 0.3313622 0.4558271 0.3692130
# 1 0.5457413 0.6686378 0.5441729 0.6307870


# ............... Variable lifehasameaning .............................................


# Indica si el encuestado cree que la vida tiene sentido

Sharedata_final %>% count(lifemeaning) %>% mutate(prop = n / sum(n)*100)

# Definimos sus categorias, dejamos los no sabe o se niega como NA al no tener 
# especial relacion con la variable objetivo y tratarse tan solo de un 1.70% de los
# datos

Sharedata_final <- Sharedata_final %>% mutate(lifemeaning= categories(lifemeaning))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$lifemeaning), margin=2)

# Estudiamos su relacion con la variable objetivo
h4 <-  Sharedata_final %>% filter(!is.na(lifemeaning)) %>%  
  dplyr::count(lifemeaning, sadness) %>% group_by(lifemeaning)
h4$lab <- paste0(round(prop.table(table(Sharedata_final$sadness,
                                        Sharedata_final$lifemeaning), margin=2) * 100, 2), sep="%")
h4$lifemeaning <- factor(h4$lifemeaning , levels=c("No data", "Never", "Often", "Rarely", "Sometimes"))

p1 <- h4 %>%
  ggplot(aes(x = factor(lifemeaning),n, fill=factor(sadness))) + 
  geom_col(color="black") + 
  labs(y = "count",
       x = "La vida tiene un significado",
       color = "sadness",
       title = "La vida tiene un significado vs Tristeza",
       subtitle = "lifemeaning vs sadness (tras agrupar valores).",
       fill = "Sadness") +
  scale_fill_manual(values=c("#c399a2", "#7d87b2"))+
  geom_text(aes(label=lab),position='stack',vjust=1.5, check_overlap = TRUE)


# ............... Variable backhapp .............................................


# Indica si el encuestado mira a su pasado con felicidad

Sharedata_final %>% count(backhapp) %>% mutate(prop = n / sum(n)*100)

# Definimos sus categorias, dejamos los no sabe o se niega como NA al no tener 
# especial relacion con la variable objetivo y tratarse tan solo de un 1.70% de los
# datos

Sharedata_final <- Sharedata_final %>% mutate(backhapp= categories(backhapp))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$backhapp), margin=2)

# Never   No data     Often    Rarely Sometimes
# 0 0.4285714 0.3657274 0.3197492 0.4034934 0.3900893
# 1 0.5714286 0.6342726 0.6802508 0.5965066 0.6099107


# Estudiamos su relacion con la variable objetivo
h5 <-  Sharedata_final %>% filter(!is.na(backhapp)) %>%  
  dplyr::count(backhapp, sadness) %>% group_by(backhapp)
h5$lab <- paste0(round(prop.table(table(Sharedata_final$sadness, 
                                        Sharedata_final$backhapp), margin=2) * 100, 2), sep="%")
h5$backhapp <- factor(h5$backhapp , levels=c("No data", "Never", "Often", "Rarely", "Sometimes"))

p2 <- h5 %>%
  ggplot(aes(x = factor(backhapp),n, fill=factor(sadness))) + 
  geom_col(color="black") + 
  labs(
       x = "Pasado feliz",
       color = "sadness",
       title = "Pasado feliz vs Tristeza",
       subtitle = "backhapp vs sadness (tras agrupar valores).",
       fill = "Sadness") +
  scale_fill_manual(values=c("#c399a2", "#7d87b2"))+
  geom_text(aes(label=lab),position='stack',vjust=1.5, check_overlap = TRUE)


# ............... Variable fullenergy .............................................


# Indica si el encuestado se siente con energia

Sharedata_final %>% count(fullenergy) %>% mutate(prop = n / sum(n)*100)

# Definimos sus categorias, dejamos los no sabe o se niega como NA al no tener 
# especial relacion con la variable objetivo y tratarse tan solo de un 1.70% de los
# datos

Sharedata_final <- Sharedata_final %>% mutate(fullenergy= categories(fullenergy))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$fullenergy), margin=2)

# Never     Often    Rarely Sometimes
# 0 0.4657702 0.3029107 0.4147465 0.3380884
# 1 0.5342298 0.6970893 0.5852535 0.6619116

# Estudiamos su relacion con la variable objetivo
h6 <-  Sharedata_final %>% filter(!is.na(fullenergy)) %>%  
  dplyr::count(fullenergy, sadness) %>% group_by(fullenergy)
h6$lab <- paste0(round(prop.table(table(Sharedata_final$sadness, 
                                        Sharedata_final$fullenergy), margin=2) * 100, 2), sep="%")
h6$fullenergy <- factor(h6$fullenergy , levels=c("No data", "Never", "Often", "Rarely", "Sometimes"))

p3 <- h6 %>%
  ggplot(aes(x = factor(fullenergy),n, fill=factor(sadness))) + 
  geom_col(color="black") + 
  labs(y = "count",
       x = "Sentirse con energía",
       color = "sadness",
       title = "Sentirse con energía vs Tristeza",
       subtitle = "fullenergy vs sadness (tras agrupar valores).",
       fill = "Sadness") +
  scale_fill_manual(values=c("#c399a2", "#7d87b2"))+
  geom_text(aes(label=lab),position='stack',vjust=1.5, check_overlap = TRUE)


# ............... Variable fulloport .............................................


# Indica si el encuestado se sienete lleno de oportunidades

Sharedata_final %>% count(fulloport) %>% mutate(prop = n / sum(n)*100)

# Definimos sus categorias, dejamos los no sabe o se niega como NA al no tener 
# especial relacion con la variable objetivo y tratarse tan solo de un 1.60% de los
# datos

Sharedata_final <- Sharedata_final %>% mutate(fulloport= categories(fulloport))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$fulloport), margin=2)

# ............... Variable futuregoof .............................................


# Indica si el encuestado cree que su futuro se ve bien

Sharedata_final %>% count(futuregood) %>% mutate(prop = n / sum(n)*100)

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$futuregood), margin=2)

# Definimos sus categorias, dejamos los no sabe o se niega como NA al no tener 
# especial relacion con la variable objetivo y tratarse tan solo de un 2.2% de los
# datos

Sharedata_final <- Sharedata_final %>% mutate(futuregood= categories(futuregood))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$futuregood), margin=2)

# Never     Often    Rarely Sometimes
# 0 0.4481409 0.2981298 0.3939009 0.3483571
# 1 0.5518591 0.7018702 0.6060991 0.6516429


# Estudiamos su relacion con la variable objetivo
h7 <-  Sharedata_final %>% filter(!is.na(futuregood)) %>%  
  dplyr::count(futuregood, sadness) %>% group_by(futuregood)
h7$lab <- paste0(round(prop.table(table(Sharedata_final$sadness, 
                                        Sharedata_final$futuregood), margin=2) * 100, 2), sep="%")
h7$futuregood <- factor(h7$futuregood , levels=c("No data", "Never", "Often", "Rarely", "Sometimes"))

p4 <- h7 %>%
  ggplot(aes(x = factor(futuregood),n, fill=factor(sadness))) + 
  geom_col(color="black") + 
  labs(
       x = "Optimista con el futuro",
       color = "sadness",
       title = "Optimista con el futuro vs Tristeza",
       subtitle = "futuregood vs sadness (tras agrupar valores).",
       fill = "Sadness",
       caption =
         paste0("Autor: Lucía Pascual Martínez | ",
                "Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)")) +
  scale_fill_manual(values=c("#c399a2", "#7d87b2"))+
  geom_text(aes(label=lab),position='stack',vjust=1.5, check_overlap = TRUE)


plot_grid(p1,p2,p3,p4, align = "h", ncol= 2, nrow=2)




# ............... Variable eversomkeddaily .............................................


# Indica si el encuestado ha fumado alguna vez diariamente

Sharedata_final %>% count(eversmokedaily) %>% mutate(prop = n / sum(n)*100)

# Convertimos los No en 0 y los se niega o no sabe en NAs

Sharedata_final <- Sharedata_final %>% mutate(eversmokedaily= depuracion(eversmokedaily))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$eversmokedaily), margin=2)


# 0         1
# 0 0.3620873 0.3710407
# 1 0.6379127 0.6289593

# ............... Variable yearssmoked .............................................


# Indica cauntos años ha fumado el encuestado, se le pone un 1 si es un año o 
# menos y un 0 si nunca ha fumado

Sharedata_final %>% count(yearsmoked) %>% mutate(prop = n / sum(n)*100)


g1 <- ggplot(Sharedata_final, aes(x=yearsmoked))+
  geom_histogram(col='black', fill= "#94b594", binwidth = 5) +
  labs(x = 'Años que ha fumado el encuestado', y = 'Frecuencias',title = 'Histograma variable yearsmoked',
       subtitle = "Distribución variable yearsmoked antes de convertirla en categórica",
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                "Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)"))

g1 <- g1 + labs(x = 'Años que ha fumado el encuestado', y = 'Count',title = 'Histograma variable yearsmoked',
                subtitle = "Distribución variable yearsmoked antes de convertirla en categórica",
                caption =
                  paste0("",
                         ""))


# Como las categorias tienen muy poca representatividad, las agrupamos:

smoke <- function(x) {
  dplyr::case_when((x == 0) ~ "Never smoked", 
                   (x >=1 & x<=20) ~ "Less than 20 years",
                   (x > 20 & x<=40) ~ "Between 20 and 40 years",
                   (x >40) ~ "More than 40 years",
                   (is.na(x)) ~ "No data",
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(yearsmoked= smoke(yearsmoked))

# Porcentaje de cada categoría de la variabe.
yearsm<- Sharedata_final %>% count(yearsmoked) %>% mutate(prop = n / sum(n)*100)
# Diagrama de barras de la variable heating
Sharedatalevels <- Sharedata_final
Sharedatalevels$yearsmoked <- factor(Sharedatalevels$yearsmoked , levels=c("No data", "Never smoked", "Less than 20 years", 
                                                                           "Between 20 and 40 years", "More than 40 years"))
smoke <- ggplot(data=subset(Sharedatalevels, !is.na(yearsmoked)), aes(x = (yearsmoked), fill=(yearsmoked)))+
  geom_bar(color="black") +
  scale_fill_manual(values = c("#D3D3D3", "#224b5e", "#6d2f20","#df7e66", "#edc775"))+
  geom_text(stat='count', 
            aes(label=stat(count)), 
            vjust=-0.5) +
  theme(legend.position="none", axis.text = element_text(size = 13)) +
  labs(x = 'Años que ha fumado el encuestado', y = 'Count',title = 'Histograma variable yearsmoked',
       subtitle = "Distribución variable yearsmoked después de convertirla en categórica",
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                "Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)"))
# Anadimos al diagrama de barras el porcentaje de cada categoría de la variable
g2 <- smoke + annotate(geom = "table",
                  x = 5,
                  y =5000,
                  size=5,
                  table.theme = ttheme_gtlight,
                  label = list(yearsm))

library(gridExtra)
library(cowplot)
plot_grid(g1,g2, align = "h", ncol= 2 , rel_widths = c(1.7/4, 2.3/4))



#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$yearsmoked), margin=2)

# Between 20 and 40 years Less than 20 years More than 40 years Never smoked
# 0               0.3730738          0.3603462          0.3980447    0.3623163
# 1               0.6269262          0.6396538          0.6019553    0.6376837


# ............... Variable drinklastsevendays .............................................


# Indica cauntos años ha bebido algo alcoholico en los ultimos 7 dias

Sharedata_final %>% count(drinklastsevendays) %>% mutate(prop = n / sum(n)*100)

# Convertimos los No en 0 y los se niega o no sabe en NAs

Sharedata_final <- Sharedata_final %>% mutate(drinklastsevendays= depuracion(drinklastsevendays))


#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$drinklastsevendays), margin=2)

# 0         1
# 0 0.3902666 0.3281294
# 1 0.6097334 0.6718706

# ............... Variable sixormoredrinks .............................................


# Indica caunto de a menudo el encuestado se ha tomado mas de 6 bebidas alcoholicas
# en los ultimos 3 meses en una sola ocasion

Sharedata_final %>% dplyr::count(sixormoredrinks) %>% mutate(prop = n / sum(n)*100)

# Agrupamos las categoras


drink <- function(x) {
  dplyr::case_when((x == 1) ~ "Often", 
                   (x == 2) ~ "Often", 
                   (x == 3) ~ "Often", 
                   (x == 4) ~ "Often", 
                   (x == 5) ~ "Often", 
                   (x == 6) ~ "Rarely", 
                   (x == 7) ~ "Never", 
                   (is.na(x)) ~ "No data",
                   TRUE ~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(sixormoredrinks= drink(sixormoredrinks))

#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$sixormoredrinks), margin=2)

# Never     Often    Rarely
# 0 0.3688228 0.3497006 0.3457364
# 1 0.6311772 0.6502994 0.6542636



# ............... Variable partnerinhh .............................................


# Indica si el encuestado convive con su pareja 

Sharedata_final %>% count(partnerinhh_update_ca) %>% mutate(prop = n / sum(n)*100)


# Pasamos el no aplicable a NA

partner <- function(x) {
  dplyr::case_when((x == "Yes") ~ "Yes", 
                   (x == "No") ~ "No", 
                   TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(partnerinhh_update_ca= partner(partnerinhh_update_ca))



#Estudioamos su ralcion con la variable objetivo
prop.table(table(Sharedata_final$sadness, Sharedata_final$partnerinhh_update_ca), margin=2)


# No       Yes
# 0 0.4150842 0.3326670
# 1 0.5849158 0.6673330


# ... Eliminamos variables que no necesitamos mas...................................


Sharedata_final <- Sharedata_final %>% select(-c(exrate,hhid8,coupleid8))



# ....................................................................................
#                       AGRUPACION PAISES
# ...................................................................................


# Vamos a agrupar los paises en cuanto a su GDP en billones de USD. Los agruparemos
# en paises "muy pobres", "pobres", "ricos" y "muy ricos":


# Función para agrupar

Riqueza <- function(country) {
  case_when(country %in% c("Germany", "France","Italy", "Spain") ~ "Very Rich",
            country %in% c("Netherlands", "Switzerland", "Poland", "Sweden",
                                  "Belgium") ~ "Rich",
            country %in% c("Israel", "Denmark", "Finland", "Romania", 
                                  "Czech Republic") ~ "Poor",
            country %in% c("Portugal", "Greece", "Hungary", "Slovak Republic", 
                                  "Luxembourg", "Bulgaria", "Croatia", "Lithuania", 
                                  "Slovenia", "Latvia", "Estonia", "Cyprus", "Malta") ~ "Very Poor",
            TRUE ~ as.character(NA))
}


Sharedata_final <- Sharedata_final %>% mutate(country_wealth = Riqueza(country))


# Vamos a agrupar ahora los paises por partido politico

Politica <- function(country) {
  case_when(country %in% c("Germany", "Sweden", "Denmark", "Finland",
                           "Portugal","Slovenia", "Malta", "Spain") ~ "Left",
            country %in% c("Netherlands",  "Czech Republic","Greece", 
                           "Slovak Republic", "Croatia", "Lithuania", "Cyprus","Israel",
                           "Hungary", "Poland", "Latvia") ~ "Right",
            country %in% c("France", "Belgium", "Luxembourg", "Estonia") ~ "Center",
            country %in% c("Italy", "Switzerland","Romania", "Bulgaria") ~ "Other",
            TRUE ~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(country_politics = Politica(country))

Sharedata_final %>%
  dplyr::count(country_politics, sadness) %>%
  group_by(country_politics) %>%
  mutate(lab = paste0(round(prop.table(table(Sharedata_final$sadness, 
                                             Sharedata_final$country_politics), margin=2) * 100, 2), '%')) %>%
  ggplot(aes(country_politics,n, fill=sadness)) + 
  geom_col(color="black") + 
  labs(y = "count",
       x = "country_politics",
       color = "sadness",
       title = "Ideología política vs Tristeza del Encuestado",
       subtitle = "country politics vs sadness.",
       caption =
         paste0("Autor: Lucía Pascual Martínez | ",
                "Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)")) +
  scale_fill_manual(values=c("#c399a2", "#7d87b2"))+
  geom_text(aes(label=lab),position='stack',vjust=1.5)

# Agrupamos, por úlimo, los países por proximidad

Geografica <- function(country) {
  case_when(country %in% c("Estonia", "Lithuania", "Latvia", "Finland", "Sweden",
                             "Denmark" ) ~ "North",
            country %in% c("Croatia", "Spain", "Greece", "Italy",  "Portugal", "Cyprus",
                           "Malta", "Slovenia", "Israel") ~ "South",
            country %in% c("Bulgaria", "Slovak Republic", "Hungary", "Poland",
                           "Czech Republic", "Romania") ~ "East",
            country %in% c("Germany", "Belgium", "France",  "Luxembourg",
                           "Switzerland", "Netherlands") ~ "West",
            TRUE ~ as.character(NA))
}

Sharedata_final <- Sharedata_final %>% mutate(country_region = Geografica(country))

Sharedata_final %>%
  dplyr::count(country_region, sadness) %>%
  group_by(country_region) %>%
  mutate(lab = paste0(round(prop.table(table(Sharedata_final$sadness, Sharedata_final$country_region), margin=2) * 100, 2), '%')) %>%
  ggplot(aes(country_region,n, fill=sadness)) + 
  geom_col(color="black") + 
  labs(y = "count",
       x = "country_region",
       color = "sadness",
       title = "Ubicación geográfica vs Tristeza del Encuestado",
       subtitle = "country region vs sadness.",
       caption =
         paste0("Autor: Lucía Pascual Martínez | ",
                "Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)")) +
  scale_fill_manual(values=c("#c399a2", "#7d87b2"))+
  geom_text(aes(label=lab),position='stack',vjust=1.5)

# --------------------------------------------------------------------------------------
#                 CONVERTIMOS LAS VARIABLES BINARIAS EN FACTOR
#--------------------------------------------------------------------------------------


cols <- c("seriousillness", "diabetes", "hypertension", "otherdisease", "falldown",
          "dizziness", "fatigue", "prescriptiondrug", "cholesteroldrug", "highbloodpreasuredrug",
          "coronarydiseasedrug", "diabetesdrug", "bronchitisdrug", "lefthome", 
          "handwash", "handsanitizer", "covercoughandsneeze", "drugagainstcovid", "anxiety",
          "sadness", "troublesleeping", "covidsymptom", "positivetest", "covidhospitalized", 
          "coviddeath", "forgotreatment", "postponedappointment", "treatedhospital", 
          "employed", "financialsupport", "giveproducts", "providepersonalcare", "volunteering",
          "receiveproducts", "receivedpersonalcare", "eversmokedaily", "drinklastsevendays",
          "closepositive", "otherpositive", "heartdisease")
Sharedata_final[,cols] <- data.frame(apply(Sharedata_final[cols], 2, as.factor))



# -----------------------------------------------------------------------------------------
#                             DEPURACION
#-----------------------------------------------------------------------------------------

# *********************************** DATOS ATIPICOS O OUTLIERS ***************************
  
# .......................... Variable HHsizeupdate ..........................................
  
library("ggpmisc")
library(modeest)
#media, mediana y varianza 
hhsize <- Sharedata_final %>% summarize(media=mean(hhsize_update_ca,  na.rm=TRUE), 
                                        mediana = median(hhsize_update_ca,  na.rm=TRUE), 
                            variance = var(hhsize_update_ca, na.rm=TRUE), 
                            mode= mlv(hhsize_update_ca,na.rm=TRUE, method='mfv'))

# Histograma de la variable hhsize_update_ca
HH <- ggplot(Sharedata_final, aes(x=hhsize_update_ca)) +
  geom_histogram(col='black', fill="#8cc8bc", binwidth = 1) +
  labs(x = "Índice del hambre global", y = 'Frecuencias', 
       title = 'Histograma variable GHI',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Source: Kaggle (Height of Male and Female by Country 2022)'))  
# Anadimos al diagrama de barras la media, mediana y varianza de la variable
HH + annotate(geom = "table",
               x = 5.5,
               y = 5000,
               size=3,
               table.theme = ttheme_gtlight,
               label = list(hhsize))

# La media y la mediana son parecidas, no detectamos outliers ya que no consideramos
# que la variable los tenga.

# LIMITE SUPERIOR OUTLIERS POR MADS (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_GHI_MADS <- (median(Sharedata_final$hhsize_update_ca, na.rm = TRUE) + 9 * mad(Sharedata_final$hhsize_update_ca, constant = 1, na.rm = TRUE))
# El límite superior es 2 mediante el MADS.
lim_superior_GHI_MADS

# LIMITE SUPERIOR OUTLIERS POR IQR (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_GHI_IQR <- summary(Sharedata_final$hhsize_update_ca, na.rm=TRUE)[5]+3*(summary(Sharedata_final$hhsize_update_ca, na.rm=TRUE)[5]-summary(Sharedata_final$hhsize_update_ca, na.rm=TRUE)[2])
# El límite superior es 5 mediante el IQR.
lim_superior_GHI_IQR

# No hay outliers, se salen del rango.


# .......................... Variable satisfactionlife ..........................................

library("ggpmisc")
library(modeest)
#media, mediana y varianza 
satis <- Sharedata_final %>% summarize(media=mean(satisfactionlife,  na.rm=TRUE), 
                                        mediana = median(satisfactionlife,  na.rm=TRUE), 
                                        variance = var(satisfactionlife, na.rm=TRUE), 
                                        mode= mlv(satisfactionlife,na.rm=TRUE, method='mfv'))

# Histograma de la variable satisfactionlife
SATISLIFE <- ggplot(Sharedata_final, aes(x=satisfactionlife)) +
  geom_histogram(col='black', fill="#8cc8bc", binwidth = 1) +
  labs(x = "Índice del hambre global", y = 'Frecuencias', 
       title = 'Histograma variable GHI',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Source: Kaggle (Height of Male and Female by Country 2022)'))  
# Anadimos al diagrama de barras la media, mediana y varianza de la variable
SATISLIFE + annotate(geom = "table",
              x = 5.5,
              y = 5000,
              size=3,
              table.theme = ttheme_gtlight,
              label = list(satis))


# La media y la mediana son parecidas, no detectamos outliers ya que no consideramos
# que la variable los tenga.


# Asimetria y curtosis
c(skewness(Sharedata_final$satisfactionlife, na.rm=TRUE), kurtosis(Sharedata_final$satisfactionlife, na.rm=TRUE))


# LIMITE INFERIOR
lim_inferior_satisslife_sd <- mean(Sharedata_final$satisfactionlife, na.rm = TRUE) - 
  4 *sd(Sharedata_final$satisfactionlife, na.rm = TRUE)
# El límite inferior es -2.82 mediante la desviación típica. (se sale del rango)
lim_inferior_satisslife_sd
# LIMITE SUPERIOR OUTLIERS POR STDDEV 
lim_superior_satislife_sd <- mean(Sharedata_final$satisfactionlife, na.rm = TRUE) + 
  4 *sd(Sharedata_final$satisfactionlife, na.rm = TRUE)
# El límite superior es 11.21 mediante la desviación típica.
lim_superior_satislife_sd

# LIMITE SUPERIOR OUTLIERS POR IQR (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_satislife_IQR <- summary(Sharedata_final$satisfactionlife, na.rm=TRUE)[5]+
  3*(summary(Sharedata_final$satisfactionlife, na.rm=TRUE)[5]-summary(Sharedata_final$satisfactionlife, na.rm=TRUE)[2])
# El límite superior es 5 mediante el IQR.
lim_superior_satislife_IQR

# No hay outliers, se salen del rango.

# .......................... Variable StringencyIndex ..........................................

library("ggpmisc")
library(modeest)
#media, mediana y varianza 
str <- Sharedata_final %>% summarize(media=mean(StringencyIndex,  na.rm=TRUE), 
                                       mediana = median(StringencyIndex,  na.rm=TRUE), 
                                       variance = var(StringencyIndex, na.rm=TRUE), 
                                       mode= mlv(StringencyIndex,na.rm=TRUE, method='mfv'))

# Histograma de la variable StringencyIndex
STRINDEX <- ggplot(Sharedata_final, aes(x=StringencyIndex)) +
  geom_histogram(col='black', fill="#8cc8bc") +
  labs(x = "Índice del hambre global", y = 'Frecuencias', 
       title = 'Histograma variable GHI',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Source: Kaggle (Height of Male and Female by Country 2022)'))  
# Anadimos al diagrama de barras la media, mediana y varianza de la variable
STRINDEX+ annotate(geom = "table",
                     x = 25,
                     y = 1600,
                     size=3,
                     table.theme = ttheme_gtlight,
                     label = list(str))


# La media y la mediana son parecidas, no detectamos outliers ya que no consideramos
# que la variable los tenga.


# Asimetria y curtosis
c(skewness(Sharedata_final$StringencyIndex, na.rm=TRUE),
  kurtosis(Sharedata_final$StringencyIndex, na.rm=TRUE))

# LIMITE INFERIOR
lim_inferior_stri_sd <- mean(Sharedata_final$StringencyIndex, na.rm = TRUE) -
  4 *sd(Sharedata_final$StringencyIndex, na.rm = TRUE)
# El límite inferior es 6.87 mediante la desviación típica. (se sale del rango)
lim_inferior_stri_sd
# LIMITE SUPERIOR OUTLIERS POR STDDEV 
lim_superior_stri_sd <- mean(Sharedata_final$StringencyIndex, na.rm = TRUE) + 
  4 *sd(Sharedata_final$StringencyIndex, na.rm = TRUE)
# El límite superior es 95.82 mediante la desviación típica.
lim_superior_stri_sd

# LIMITE SUPERIOR OUTLIERS POR IQR (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_stri_IQR <- summary(Sharedata_final$StringencyIndex, na.rm=TRUE)[5]+
  3*(summary(Sharedata_final$StringencyIndex, na.rm=TRUE)[5]-summary(Sharedata_final$StringencyIndex, na.rm=TRUE)[2])
# El límite superior es 107.9 mediante el IQR.
lim_superior_stri_IQR

# No hay outliers, se salen del rango.

# .......................... Variable GovermentResponseIndex ..........................................

library("ggpmisc")
library(modeest)
#media, mediana y varianza 
govresp<- Sharedata_final %>% summarize(media=mean(GovernmentResponseIndex,  na.rm=TRUE), 
                                     mediana = median(GovernmentResponseIndex,  na.rm=TRUE), 
                                     variance = var(GovernmentResponseIndex, na.rm=TRUE), 
                                     mode= mlv(GovernmentResponseIndex,na.rm=TRUE, method='mfv'))

# Histograma de la variable GovernmentResponseIndex
GOVERME <- ggplot(Sharedata_final, aes(x=GovernmentResponseIndex)) +
  geom_histogram(col='black', fill="#8cc8bc") +
  labs(x = "Índice del hambre global", y = 'Frecuencias', 
       title = 'Histograma variable GHI',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Source: Kaggle (Height of Male and Female by Country 2022)'))  
# Anadimos al diagrama de barras la media, mediana y varianza de la variable
GOVERME+ annotate(geom = "table",
                   x = 35,
                   y = 1900,
                   size=3,
                   table.theme = ttheme_gtlight,
                   label = list(govresp))


# La media y la mediana son parecidas, no detectamos outliers ya que no consideramos
# que la variable los tenga.

# Asimetria y curtosis
c(skewness(Sharedata_final$GovernmentResponseIndex, na.rm=TRUE), 
  kurtosis(Sharedata_final$GovernmentResponseIndex, na.rm=TRUE))

# LIMITE INFERIOR
lim_inferior_govresp_sd <- mean(Sharedata_final$GovernmentResponseIndex, na.rm = TRUE) - 
  4 *sd(Sharedata_final$GovernmentResponseIndex, na.rm = TRUE)
# El límite inferior es 16.15 mediante la desviación típica. (se sale del rango)
lim_inferior_govresp_sd
# LIMITE SUPERIOR OUTLIERS POR STDDEV 
lim_superior_govresp_sd <- mean(Sharedata_final$GovernmentResponseIndex, na.rm = TRUE) + 
  4 *sd(Sharedata_final$GovernmentResponseIndex, na.rm = TRUE)
# El límite superior es 90.64 mediante la desviación típica.
lim_superior_govresp_sd

# LIMITE SUPERIOR OUTLIERS POR IQR (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_govresp_IQR <- summary(Sharedata_final$GovernmentResponseIndex, na.rm=TRUE)[5]+
  3*(summary(Sharedata_final$GovernmentResponseIndex, na.rm=TRUE)[5]
     -summary(Sharedata_final$GovernmentResponseIndex, na.rm=TRUE)[2])
# El límite superior es 104.15 mediante el IQR.
lim_superior_govresp_IQR

# No hay outliers, se salen del rango.

# .......................... Variable EconomicSupportIndex ..........................................

library("ggpmisc")
library(modeest)
#media, mediana y varianza 
econo<- Sharedata_final %>% summarize(media=mean(EconomicSupportIndex,  na.rm=TRUE), 
                                        mediana = median(EconomicSupportIndex,  na.rm=TRUE), 
                                        variance = var(EconomicSupportIndex, na.rm=TRUE), 
                                        mode= mlv(EconomicSupportIndex,na.rm=TRUE, method='mfv'))

# Histograma de la variable EconomicSupportIndex
ECONINDEX <- ggplot(Sharedata_final, aes(x=EconomicSupportIndex)) +
  geom_histogram(col='black', fill="#8cc8bc", binwidth = 10) +
  labs(x = "Índice del hambre global", y = 'Frecuencias', 
       title = 'Histograma variable GHI',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Source: Kaggle (Height of Male and Female by Country 2022)'))  
# Anadimos al diagrama de barras la media, mediana y varianza de la variable
ECONINDEX+ annotate(geom = "table",
                  x = 35,
                  y = 3500,
                  size=2.5,
                  table.theme = ttheme_gtlight,
                  label = list(econo))


# La media y la mediana son parecidas, no detectamos outliers ya que no consideramos
# que la variable los tenga.

# Asimetria y curtosis
c(skewness(Sharedata_final$EconomicSupportIndex, na.rm=TRUE), kurtosis(Sharedata_final$EconomicSupportIndex, na.rm=TRUE))

# LIMITE INFERIOR
lim_inferior_ecosup_sd <- mean(Sharedata_final$EconomicSupportIndex, na.rm = TRUE) - 
  4 *sd(Sharedata_final$EconomicSupportIndex, na.rm = TRUE)
# El límite inferior es -0.09 mediante la desviación típica. (se sale del rango)
lim_inferior_ecosup_sd
# LIMITE SUPERIOR OUTLIERS POR STDDEV 
lim_superior_ecosup_sd <- mean(Sharedata_final$EconomicSupportIndex, na.rm = TRUE) + 
  4 *sd(Sharedata_final$EconomicSupportIndex, na.rm = TRUE)
# El límite superior es 140.5 mediante la desviación típica.
lim_superior_ecosup_sd

# LIMITE SUPERIOR OUTLIERS POR IQR (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_ecosup_IQR <- summary(Sharedata_final$EconomicSupportIndex, na.rm=TRUE)[5]+
  3*(summary(Sharedata_final$EconomicSupportIndex, na.rm=TRUE)[5]-summary(Sharedata_final$EconomicSupportIndex, na.rm=TRUE)[2])
# El límite superior es 162.5 mediante el IQR.
lim_superior_ecosup_IQR

# No hay outliers, se salen del rango.

# .......................... Variable ContainmentHEalthIndex ..........................................

library("ggpmisc")
library(modeest)
#media, mediana y varianza 
cont<- Sharedata_final %>% summarize(media=mean(ContainmentHealthIndex,  na.rm=TRUE), 
                                      mediana = median(ContainmentHealthIndex,  na.rm=TRUE), 
                                      variance = var(ContainmentHealthIndex, na.rm=TRUE), 
                                      mode= mlv(ContainmentHealthIndex,na.rm=TRUE, method='mfv'))

# Histograma de la variable ContainmentHealthIndex
CONTINDEX <- ggplot(Sharedata_final, aes(x=ContainmentHealthIndex)) +
  geom_histogram(col='black', fill="#8cc8bc", binwidth = 5) +
  labs(x = "Índice del hambre global", y = 'Frecuencias', 
       title = 'Histograma variable GHI',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Source: Kaggle (Height of Male and Female by Country 2022)'))  
# Anadimos al diagrama de barras la media, mediana y varianza de la variable
CONTINDEX+ annotate(geom = "table",
                    x = 35,
                    y = 3500,
                    size=2.5,
                    table.theme = ttheme_gtlight,
                    label = list(cont))


# La media y la mediana son parecidas, no detectamos outliers ya que no consideramos
# que la variable los tenga.

# Asimetria y curtosis
c(skewness(Sharedata_final$ContainmentHealthIndex, na.rm=TRUE), 
  kurtosis(Sharedata_final$ContainmentHealthIndex, na.rm=TRUE))

# LIMITE INFERIOR
lim_inferior_conthealth_sd <- mean(Sharedata_final$ContainmentHealthIndex, na.rm = TRUE) - 
  4 *sd(Sharedata_final$ContainmentHealthIndex, na.rm = TRUE)
# El límite inferior es 11.79 mediante la desviación típica. (se sale del rango)
lim_inferior_conthealth_sd
# LIMITE SUPERIOR OUTLIERS POR STDDEV 
lim_superior_conthealth_sd <- mean(Sharedata_final$ContainmentHealthIndex, na.rm = TRUE) + 
  4 *sd(Sharedata_final$ContainmentHealthIndex, na.rm = TRUE)
# El límite superior es 90.2 mediante la desviación típica.
lim_superior_conthealth_sd

# LIMITE SUPERIOR OUTLIERS POR IQR (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_conthealth_IQR <- summary(Sharedata_final$ContainmentHealthIndex, na.rm=TRUE)[5]+
  3*(summary(Sharedata_final$ContainmentHealthIndex, na.rm=TRUE)[5]-
       summary(Sharedata_final$ContainmentHealthIndex, na.rm=TRUE)[2])
# El límite superior es 110.83 mediante el IQR.
lim_superior_conthealth_IQR

# No hay outliers, se salen del rango.


# .......................... Variable ConfirmedDeaths ..........................................

library("ggpmisc")
library(modeest)
#media, mediana y varianza 
death<- Sharedata_final %>% summarize(media=mean(ConfirmedDeaths,  na.rm=TRUE), 
                                     mediana = median(ConfirmedDeaths,  na.rm=TRUE), 
                                     variance = var(ConfirmedDeaths, na.rm=TRUE), 
                                     mode= mlv(ConfirmedDeaths,na.rm=TRUE, method='mfv'))

# Histograma de la variable ConfirmedDeaths
DEATHS <- ggplot(Sharedata_final, aes(x=ConfirmedDeaths)) +
  geom_histogram(col='black', fill="#8cc8bc") +
  labs(x = "Índice del hambre global", y = 'Frecuencias', 
       title = 'Histograma variable GHI',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Source: Kaggle (Height of Male and Female by Country 2022)'))  
# Anadimos al diagrama de barras la media, mediana y varianza de la variable
DEATHS+ annotate(geom = "table",
                    x = 35,
                    y = 3500,
                    size=2.5,
                    table.theme = ttheme_gtlight,
                    label = list(death))


# LIMITE SUPERIOR OUTLIERS POR MADS (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_deaths_MADS <- (median(Sharedata_final$ConfirmedDeaths, na.rm = TRUE) + 
                               9 * mad(Sharedata_final$ConfirmedDeaths, constant = 1, na.rm = TRUE))
# El límite superior es 14009 mediante el MADS.
lim_superior_deaths_MADS

# LIMITE SUPERIOR OUTLIERS POR IQR (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_conthealth_IQR <- summary(Sharedata_final$ConfirmedDeaths, na.rm=TRUE)[5]+
  3*(summary(Sharedata_final$ConfirmedDeaths, na.rm=TRUE)[5]-summary(Sharedata_final$ConfirmedDeaths, na.rm=TRUE)[2])
# El límite superior es 38631 mediante el IQR.
lim_superior_conthealth_IQR


# Asimetría y curtosis de las variables anteriores
library(moments)
c(skewness(Sharedata_final$ConfirmedDeaths, na.rm=TRUE), kurtosis(Sharedata_final$ConfirmedDeaths, na.rm=TRUE))



# .......................... Variable ConfirmedCases ..........................................

library("ggpmisc")
library(modeest)
#media, mediana y varianza 
case<- Sharedata_final %>% summarize(media=mean(ConfirmedCases,  na.rm=TRUE), 
                                      mediana = median(ConfirmedCases,  na.rm=TRUE), 
                                      variance = var(ConfirmedCases, na.rm=TRUE), 
                                      mode= mlv(ConfirmedCases,na.rm=TRUE, method='mfv'))

# Histograma de la variable ConfirmedCases
CASES <- ggplot(Sharedata_final, aes(x=ConfirmedCases)) +
  geom_histogram(col='black', fill="#8cc8bc") +
  labs(x = "Índice del hambre global", y = 'Frecuencias', 
       title = 'Histograma variable GHI',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Source: Kaggle (Height of Male and Female by Country 2022)'))  
# Anadimos al diagrama de barras la media, mediana y varianza de la variable
CASES+ annotate(geom = "table",
                 x = 35,
                 y = 3500,
                 size=2.5,
                 table.theme = ttheme_gtlight,
                 label = list(case))

# Asimetría y curtosis
library(moments)
c(skewness(Sharedata_final$ConfirmedCases, na.rm=TRUE), kurtosis(Sharedata_final$ConfirmedCases, na.rm=TRUE))

# LIMITE SUPERIOR OUTLIERS POR MADS (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_cases_MADS <- (median(Sharedata_final$ConfirmedCases, na.rm = TRUE)+ 
                              9 * mad(Sharedata_final$ConfirmedCases, constant = 1, na.rm = TRUE))
# El límite superior es 251799 mediante el MADS.
lim_superior_cases_MADS

# LIMITE SUPERIOR OUTLIERS POR IQR (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_cases_IQR <- summary(Sharedata_final$ConfirmedCases, na.rm=TRUE)[5]+
  3*(summary(Sharedata_final$ConfirmedCases, na.rm=TRUE)[5]-summary(Sharedata_final$ConfirmedCases, na.rm=TRUE)[2])
# El límite superior es 298077 mediante el IQR.
lim_superior_cases_IQR

# ...DETECTAMOS OUTLIERS DE LA VARIABLE ConfirmedCases MEDIANTE IQR....

Sharedata_final <-  Sharedata_final %>% mutate(ConfirmedCases = ifelse(
  ConfirmedCases < summary(ConfirmedCases, na.rm=TRUE)[2]-3*(summary(ConfirmedCases, na.rm=TRUE)[5]-
                                                               summary(ConfirmedCases, na.rm=TRUE)[2]), 
  NA, ifelse(
    ConfirmedCases> summary(ConfirmedCases, na.rm=TRUE)[5]+3*(summary(ConfirmedCases, na.rm=TRUE)[5]-
                                                                summary(ConfirmedCases, na.rm=TRUE)[2]),
    NA , ConfirmedCases)))



# .......................... Variable age ..........................................

library("ggpmisc")
library(modeest)
#media, mediana y varianza 
age<- Sharedata_final %>% summarize(media=mean(age,  na.rm=TRUE), 
                                     mediana = median(age,  na.rm=TRUE), 
                                     variance = var(age, na.rm=TRUE), 
                                     mode= mlv(age,na.rm=TRUE, method='mfv'))

# Histograma de la variable ConfirmedCases
AGEHIST <- ggplot(Sharedata_final, aes(x=age)) +
  geom_histogram(col='black', fill="#8cc8bc") +
  labs(x = "Índice del hambre global", y = 'Frecuencias', 
       title = 'Histograma variable GHI',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Source: Kaggle (Height of Male and Female by Country 2022)'))  
# Anadimos al diagrama de barras la media, mediana y varianza de la variable
AGEHIST+ annotate(geom = "table",
                x = 100,
                y = 1000,
                size=2.5,
                table.theme = ttheme_gtlight,
                label = list(age))

# Asimetría y curtosis
library(moments)
c(skewness(Sharedata_final$age, na.rm=TRUE), kurtosis(Sharedata_final$age, na.rm=TRUE))

# LIMITE INFERIOR
lim_inferior_age_sd <- mean(Sharedata_final$age, na.rm = TRUE) - 4 *sd(Sharedata_final$age, na.rm = TRUE)
# El límite inferior es 33.26  mediante la desviación típica. (se sale del rango)
lim_inferior_age_sd
# LIMITE SUPERIOR OUTLIERS POR STDDEV 
lim_superior_age_sd <- mean(Sharedata_final$age, na.rm = TRUE) + 4 *sd(Sharedata_final$age, na.rm = TRUE)
# El límite superior es 110.33 mediante la desviación típica. (se sale del rango)
lim_superior_age_sd

# LIMITE SUPERIOR OUTLIERS POR IQR (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_age_IQR <- summary(Sharedata_final$age, na.rm=TRUE)[5]+
  3*(summary(Sharedata_final$age, na.rm=TRUE)[5]-summary(Sharedata_final$age, na.rm=TRUE)[2])
# El límite superior es 124 mediante el IQR.
lim_superior_age_IQR

# No hay outliers, se salen del rango.



# .......................... Variable numdrugs ..........................................

library("ggpmisc")
library(modeest)
#media, mediana y varianza 
numdrugs<- Sharedata_final %>% summarize(media=mean(numdrugs,  na.rm=TRUE), 
                                    mediana = median(numdrugs,  na.rm=TRUE), 
                                    variance = var(numdrugs, na.rm=TRUE), 
                                    mode= mlv(numdrugs,na.rm=TRUE, method='mfv'))

# Histograma de la variable ConfirmedCases
DRUGS <- ggplot(Sharedata_final, aes(x=numdrugs)) +
  geom_histogram(col='black', fill="#8cc8bc", binwidth = 1) +
  labs(x = "Índice del hambre global", y = 'Frecuencias', 
       title = 'Histograma variable GHI',
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                'Source: Kaggle (Height of Male and Female by Country 2022)'))  
# Anadimos al diagrama de barras la media, mediana y varianza de la variable
DRUGS+ annotate(geom = "table",
                  x = 5,
                  y = 3000,
                  size=2.5,
                  table.theme = ttheme_gtlight,
                  label = list(numdrugs))

# Asimetría y curtosis
library(moments)
c(skewness(Sharedata_final$numdrugs, na.rm=TRUE), kurtosis(Sharedata_final$numdrugs, na.rm=TRUE))

# LIMITE INFERIOR
lim_inferior_drug_sd <- mean(Sharedata_final$numdrugs, na.rm = TRUE) - 4 *sd(Sharedata_final$numdrugs, na.rm = TRUE)
# El límite inferior es  -3.34  mediante la desviación típica. (se sale del rango)
lim_inferior_drug_sd
# LIMITE SUPERIOR OUTLIERS POR STDDEV 
lim_superior_drug_sd <- mean(Sharedata_final$numdrugs, na.rm = TRUE) + 4 *sd(Sharedata_final$numdrugs, na.rm = TRUE)
# El límite superior es 6.15 mediante la desviación típica. (se sale del rango)
lim_superior_drug_sd

# LIMITE SUPERIOR OUTLIERS POR IQR (LIMITE INFERIOR SE SALE DEL RANGO DE LA VARIABLE, NO HAY)
lim_superior_drug_IQR <- summary(Sharedata_final$numdrugs, na.rm=TRUE)[5]+
  3*(summary(Sharedata_final$numdrugs, na.rm=TRUE)[5]-summary(Sharedata_final$numdrugs, na.rm=TRUE)[2])
# El límite superior es 8 mediante el IQR.
lim_superior_drug_IQR

# No hay outliers, se salen del rango.



# ------------------------------------------------------------------------------
#              IMPUTACION DE NAS (PRUEBA DE MISS FOREST)
# -----------------------------------------------------------------------------




Sharedata_final <- Sharedata_final %>% mutate(across(where(is.character), as.factor))
Sharedata_final <- as.data.frame(Sharedata_final) 
Sharedata_final <- Sharedata_final %>% select(-country, -mergeid)

#contamos el número de misings por observación tras aplicar la receta anterior.
missings <-rowSums(is.na(Sharedata_final))



# Dividimos el conjunto de datos en train/test

# Partición
library(rsample)
set.seed(1234)
split_share <- initial_split(Sharedata_final, prop = 0.8)
train_share <- training(split_share)
train_share <- as.data.frame(train_share)
test_share <- testing(split_share)
test_share <- as.data.frame(test_share)

# Verficamos cuanto de mejor es para predecir la variable objetivo imputar por 
# random forest

missforest_auc <- c()
median_mode_auc <- c()

library(doParallel)
registerDoParallel(cores = 3) # set number of cores to use
library(doRNG)
registerDoRNG(seed = 123)

library(missForest)
library(PRROC)
library(imputeMissings)
detach("package:tidyverse", unload=TRUE)
for (i in 1:15) {
  
  
  ##### ##### ##### missForest() ##### ##### ##### 
  
  # 1) impute train
  imp_train_X <- missForest(train_share,ntree=5, parallelize = "forests")$ximp
  
  # 2) build model
  rf <- randomForest(sadness ~., data=imp_train_X)
  
  # 3) impute test
  train_test_X <- rbind(test_share, imp_train_X)
  imp_test_X <- missForest(train_test_X, ntree=5, parallelize = "forests")$ximp[1:nrow(test_share), ]
  
  # 4) evaluate model
  pred_test <- predict(rf, imp_test_X, type = "prob")
  
  test_scores <- data.frame(event_prob = pred_test[ ,2], labels = test_share$sadness)
  
  test_roc_v1 <- roc.curve(scores.class0 = test_scores[test_scores$labels == "1", ]$event_prob,
                           scores.class1 = test_scores[test_scores$labels == "0", ]$event_prob,
                           curve=T)
  
  missforest_auc[i] <- test_roc_v1$auc
  
  ##### ##### ##### median/mode ##### ##### ##### 
  
  # 1) impute train & test
  imp_train_X <- imputeMissings::impute(train_share, method = "median/mode")
  imp_test_X <- impute(test_share, method = "median/mode")
  
  # 2) build model
  rf <- randomForest(sadness ~., data=imp_train_X)
  
  # 3) evaluate model
  pred_test <- predict(rf, imp_test_X, type = "prob")
  
  test_scores <- data.frame(event_prob = pred_test[ ,2], labels = test_share$sadness)
  
  test_roc_v2 <- roc.curve(scores.class0 = test_scores[test_scores$labels == "1", ]$event_prob,
                           scores.class1 = test_scores[test_scores$labels == "0", ]$event_prob,
                           curve=T)
  
  median_mode_auc[i] <- test_roc_v2$auc
  
  
  print(paste0("i = ", i, " . . . missforest_auc = ", round(missforest_auc[i], 3), 
               ", median_mode_auc = ", round(median_mode_auc[i], 3)))
}

# Guardamos los resultados en un dataframe
auc_df <- data.frame(missforest_auc, median_mode_auc) %>%
  mutate(iteration = 1:nrow(.))

# Los exportamos e importamos.
write.csv(auc_df, "./auc_df.csv", row.names = F)

auc_df <- read.csv("./auc_df.csv")


# ............ Graficamos los resultados .......................................


g1 <- auc_df %>%
  gather("method",
         "AUC",
         -iteration) %>%
  ggplot(aes(x = AUC, fill = method)) +
  geom_vline(aes(xintercept = mean(auc_df$missforest_auc)), size = 1, col = "#c2cae3") +
  geom_vline(aes(xintercept = mean(auc_df$median_mode_auc)), size = 1, col = "#f7dea3") +
  geom_density(alpha = 0.4) + 
  scale_fill_manual(labels = c("median/mode", "missForest"),values = c("#f7dea3", "#c2cae3")) +
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())

g2 <- auc_df %>%
  mutate(diff = missforest_auc - median_mode_auc) %>%
  gather("method",
         "AUC",
         -diff) %>%
  ggplot(aes(x = diff)) +
  geom_vline(xintercept = 0, col = "grey20") +
  geom_vline(aes(xintercept = mean(auc_df$missforest_auc - auc_df$median_mode_auc), col = "#749e89"), size = 1) + 
  scale_color_manual(values = "#749e89", labels = "mean(diff)") +
  geom_density(alpha = 0.4, fill = "#749e89") + 
  scale_x_continuous(breaks = seq(-0.06, 0.06, 0.01)) + 
  labs(x = "diff = (missForest AUC) - (median/mode AUC)") + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())

g3 <- auc_df %>% 
  gather("method", 
         "AUC", 
         -iteration) %>% 
  ggplot(aes(x = method, y = AUC, fill = method)) + 
  geom_boxplot() + 
  scale_fill_manual(labels = c("median/mode", "missForest"), values = c("#f7dea3", "#c2cae3")) +
  coord_flip() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())

g4 <- auc_df %>%
  mutate(missforest_best = factor(missforest_auc - median_mode_auc > 0)) %>%
  ggplot(aes(x = median_mode_auc, y = missforest_auc, col = missforest_best)) + 
  geom_abline(intercept = 0, slope = 1, col = "deepskyblue3", size = 1, alpha = 0.5, linetype = "dashed") + 
  geom_point() + 
  scale_color_manual(values = c("grey30", "#749e89")) +
  labs(x = "median/mode AUC", 
       y = "missForest AUC", 
       col = "missForest best?:") + 
  theme(legend.position = "bottom")

library(gridExtra)
grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)

# ******************************************************************************
#                 IMPUTAMOS ENTONCES TODO EL CONJUNTO POR RF
# ******************************************************************************

library(missForest)
# Realzamos 50 árboles y 10 iteraciones
Sharedata_imp <- missForest(Sharedata_final, ntree=50, parallelize = "forests")


# Imputamos el conjunto train
Sharedatadepurado <- Sharedata_imp$ximp


write.csv(Sharedatadepurado, "./sharedatadepurado.csv", row.names = FALSE)
Sharedatadepurado <- read.csv("./Sharedatadepurado.csv")

# ******************************************************************************
#                       DUMMIES
# *****************************************************************************


# Definimos las variables continuas, categoricas y la variable dependiente

t <- names(Sharedatadepurado)

# sepramos los nombres por comas
a <- c()
for (i in 1:length(t)){
  a <- paste(a,t[i], sep = "','")
}

categoricas <- c('gender','healthbcovid','healthacovid','seriousillness',
                 'diabetes','hypertension','heartdisease','otherdisease','falldown',
                 'dizziness','fatigue','prescriptiondrug','cholesteroldrug','highbloodpreasuredrug',
                 'coronarydiseasedrug','diabetesdrug','bronchitisdrug','lefthome','facemaskout',
                 'keepdistance','handwash','handsanitizer','covercoughandsneeze','drugagainstcovid',
                 'anxiety','anxietylevel','troublesleeping','sleeptroublelevel','loneliness',
                 'lonelinesslevel','covidsymptom','positivetest','covidhospitalized','coviddeath',
                 'forgotreatment','postponedappointment','treatedhospital','employed','lostjob',
                 'workplace','safeworkplace','reducedworkinghours','incomebeforecorona',
                 'financialsupport','lowestincomesincecorona','makeendsmeet','dipsavings',
                 'personalcontactchildren','personalcontactparent','personalcontactfamily',
                 'personalcontactnonrelatives','onlinecontactchildren','onlinecontactparents',
                 'onlinecontactrelatives','onlinecontactnonrelatives','giveproducts','providepersonalcare',
                 'volunteering','receiveproducts','receivedpersonalcare',
                 'partnerinhh_update_ca','satisfactionlife','leftout','lookforwardeachday',
                 'lifemeaning','backhapp','fullenergy','fulloport','futuregood','eversmokedaily',
                 'yearsmoked','drinklastsevendays','sixormoredrinks',
                 'numberillness','closepositive','otherpositive','country_wealth',
                 'country_politics','country_region')
continuas <- c('hhsize_update_ca','StringencyIndex','GovernmentResponseIndex',
                'EconomicSupportIndex','ContainmentHealthIndex','ConfirmedDeaths',
                'ConfirmedCases','age','numdrugs')
dependiente <- c('sadness')


# Obtener dummies. Las dummies sustituyen a las variables originales,
# con lo que es mejor crear un archivo nuevo por si queremos utilizar las 
# originales en algún momento

library(dummies)
Sharedata_dummies <- dummy.data.frame(Sharedatadepurado, categoricas, sep =".")

# Eliminamos las dummies que se repiten, por cada variable con n categorias queremos
# n-1 dummies.

t <- names(Sharedata_dummies)

# sepramos los nombres por comas
a <- c()
for (i in 1:length(t)){
  a <- paste(a,t[i], sep = "','")
}

a

Sharedata_dummies <- Sharedata_dummies %>% dplyr::select(-c(gender.Female, `healthbcovid.Very Good`,
                                                     `healthacovid.Improved or the same`, 
                                                     seriousillness.0, diabetes.0, hypertension.0,
                                                     heartdisease.0, otherdisease.0, falldown.1,
                                                     dizziness.0, fatigue.0, prescriptiondrug.0,
                                                     cholesteroldrug.0, highbloodpreasuredrug.0, 
                                                     coronarydiseasedrug.0, diabetesdrug.0,
                                                     bronchitisdrug.0, lefthome.0, `facemaskout.Has not left home`,
                                                     `keepdistance.Has not left home`, handwash.0,
                                                     handsanitizer.0, covercoughandsneeze.0, drugagainstcovid.0,
                                                     anxiety.0, `anxietylevel.Not nervous`, troublesleeping.1,
                                                     `sleeptroublelevel.Not trouble sleeping`, loneliness.Sometimes,
                                                     `lonelinesslevel.Less or the same`, covidsymptom.0, 
                                                     positivetest.0, covidhospitalized.0, coviddeath.0, 
                                                     forgotreatment.0,postponedappointment.0, treatedhospital.0,
                                                     employed.1, `lostjob.Not working`, `workplace.home and or the usual work place`,
                                                     `safeworkplace.Not working or working just from home`, 
                                                     reducedworkinghours.No, incomebeforecorona.Average,
                                                     financialsupport.0, lowestincomesincecorona.Average, 
                                                     `makeendsmeet.Fairly easily`, `dipsavings.Not difficulty making ends meet`,
                                                     `personalcontactchildren.About once a week`, `personalcontactparent.No parents`,
                                                     `personalcontactfamily.No relatives`, `personalcontactnonrelatives.Daily`,
                                                     `onlinecontactchildren.Less often`,`onlinecontactparents.Sometimes`,
                                                     `onlinecontactrelatives.About once a week`, `onlinecontactnonrelatives.About once a week`,
                                                     giveproducts.0, providepersonalcare.0, volunteering.0, receiveproducts.0, receivedpersonalcare.0,
                                                     partnerinhh_update_ca.No, satisfactionlife.Medium, leftout.Sometimes, lookforwardeachday.Rarely, 
                                                     lifemeaning.Rarely, backhapp.Rarely, fullenergy.Rarely, fulloport.Rarely, futuregood.Rarely, 
                                                     eversmokedaily.0, `yearsmoked.Between 20 and 40 years`, drinklastsevendays.0, sixormoredrinks.Often, 
                                                     `numberillness.More than two`, closepositive.0, otherpositive.0, `country_wealth.Very Poor`,
                                                     country_politics.Center, country_region.West
                                                     ))


# Cambiamos los nombres a las variables

Sharedata_dummies <-  dplyr::rename(Sharedata_dummies, gender = gender.Male,
                                seriousillness = seriousillness.1,
                                diabetes = diabetes.1,
                                hypertension = hypertension.1,
                                heartdisease = heartdisease.1,
                                otherdisease = otherdisease.1,
                                falldown = falldown.0,
                                dizziness = dizziness.1,
                                fatigue = fatigue.1,
                                prescriptiondrug = prescriptiondrug.1,
                                highbloodpreasuredrug = highbloodpreasuredrug.1,
                                coronarydiseasedrug = coronarydiseasedrug.1,
                                diabetesdrug = diabetesdrug.1,
                                bronchitisdrug = bronchitisdrug.1,
                                lefthome = lefthome.1,
                                handwash = handwash.1,
                                handsanitizer = handsanitizer.1,
                                covercoughandsneeze = covercoughandsneeze.1,
                                drugagainstcovid = drugagainstcovid.1,
                                anxiety = anxiety.1,
                                anxietylevel.Lessorthesame = `anxietylevel.Less or the same`,
                                anxietylevel.Moreso = `anxietylevel.More so`,
                                troublesleeping = troublesleeping.0,
                                sleeptroublelevel.Lessorthesame = `sleeptroublelevel.Less or the same`,
                                sleeptroublelevel.Moreso = `sleeptroublelevel.More so`,
                                lonelinesslevel.Moreso = `lonelinesslevel.More so`,
                                lonelinesslevel.Notlonely = `lonelinesslevel.Not lonely`,
                                covidsymptom = covidsymptom.1,
                                positivetest = positivetest.1,
                                covidhospitalized = covidhospitalized.1,
                                coviddeath = coviddeath.1,
                                forgotreatment = forgotreatment.1,
                                postponedappointment = postponedappointment.1,
                                treatedhospital = treatedhospital.1,
                                employed = employed.0,
                                workplace.Noneofthese = `workplace.None of these`,
                                workplace.Notworking = `workplace.Not working`,
                                workplace.Usualworkplaceonly = `workplace.Usual work place only`,
                                safeworkplace.Somewhatsafe = `safeworkplace.Somewhat safe`,
                                safeworkplace.Verysafe = `safeworkplace.Very safe`,
                                reducedworkinghours.Notworking = `reducedworkinghours.Not working`,
                                financialsupport = financialsupport.1,
                                makeendsmeet.Greatdifficulty = `makeendsmeet.Great difficulty`,
                                makeendsmeet.Somedifficulty = `makeendsmeet.Some difficulty`,
                                personalcontactchildren.Lessoften = `personalcontactchildren.Less often`,
                                personalcontactchildren.Nochildren = `personalcontactchildren.No children`,
                                personalcontactchildren.Severaltimesaweek = `personalcontactchildren.Several tiemes a week`,
                                personalcontactnonrelatives.Aboutonceaweek = `personalcontactnonrelatives.About once a week`,
                                personalcontactnonrelatives.Lessoften = `personalcontactnonrelatives.Less often`,
                                personalcontactnonrelatives.Nononrelatives = `personalcontactnonrelatives.No non relatives`,
                                personalcontactnonrelatives.Severaltimesaweek = `personalcontactnonrelatives.Several times a week`,
                                onlinecontactchildren.Aboutonceaweek = `onlinecontactchildren.About once a week`,
                                onlinecontactchildren.Nochildren = `onlinecontactchildren.No children`,
                                onlinecontactchildren.Severaltimesaweek = `onlinecontactchildren.Several times a week`,
                                onlinecontactparents.Noparents = `onlinecontactparents.No parents`,
                                onlinecontactrelatives.Lessoften = `onlinecontactrelatives.Less often`,
                                onlinecontactnonrelatives.Nononrelatives = `onlinecontactnonrelatives.No non relatives`,
                                onlinecontactnonrelatives.Severaltimesaweek = `onlinecontactnonrelatives.Several times a week`,
                                giveproducts = giveproducts.1,
                                providepersonalcare = providepersonalcare.1,
                                volunteering = volunteering.1,
                                receiveproducts = receiveproducts.1,
                                receivedpersonalcare = receivedpersonalcare.1,
                                partnerinhh_update_ca = partnerinhh_update_ca.Yes,
                                satisfactionlife.Nodata = `satisfactionlife.No data`,
                                leftout.Nodata = `leftout.No data`,
                                lookforwardeachday.Nodata = `lookforwardeachday.No data`,
                                lifemeaning.Nodata = `lifemeaning.No data`,
                                backhapp.Nodata = `backhapp.No data`,
                                fullenergy.Nodata = `fullenergy.No data`,
                                fulloport.Nodata = `fulloport.No data`,
                                futuregood.Nodata = `futuregood.No data`,
                                eversmokedaily = eversmokedaily.1,
                                yearsmoked.Lessthan20years = `yearsmoked.Less than 20 years`,
                                yearsmoked.Morethan40years = `yearsmoked.More than 40 years`,
                                yearsmoked.Neversmoked = `yearsmoked.Never smoked`,
                                yearsmoked.Nodata = `yearsmoked.No data`,
                                drinklastsevendays = drinklastsevendays.1,
                                sixormoredrinks.Nodata = `sixormoredrinks.No data`,
                                closepositive = closepositive.1,
                                otherpositive = otherpositive.1,
                                country_wealth.VeryRich = `country_wealth.Very Rich`,
                                onlinecontactrelatives.Severaltimesaweek = `onlinecontactrelatives.Several times a week`
)


# Make Valid Column Names 
colnames(Sharedata_dummies) <- make.names(colnames(Sharedata_dummies))


write.csv(Sharedata_dummies, "./Sharedata_dummies.csv", row.names= FALSE)


# Mirmaos la correlación entre las variables dummies.
df_correl <-  Sharedata_dummies
matriz_cor <- round(df_correl %>% cor(),3)

for (i in 1:nrow(matriz_cor)) {
  k=i+1
  if (k!=nrow(matriz_cor)){
    for (j in k:nrow(matriz_cor)) {
      if (abs(matriz_cor[i,j])>0.80) {
        print(c((matriz_cor[i,j]), 
                names(matriz_cor[1,])[i], 
                names(matriz_cor[1,])[j]))
      }
    }
  }
  else break
}

# *****************************************************************************
#                     DIVIDIMOS EN CONJUNTOS DE TRAIN Y TEST
# *****************************************************************************

# Partición
library(rsample)
set.seed(1234)
split_train <- initial_split(Sharedata_dummies, prop = 0.8, strata = sadness)
Share_train<- training(split_train)
Share_test <- testing(split_train)

write.csv(Share_train, "./Share_train.csv", row.names= FALSE)
write.csv(Share_test, "./Share_test.csv", row.names= FALSE)

