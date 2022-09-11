library(rpart)
library(rpart.plot)
library(rattle)
library(pROC)

library(dplyr)
library(caret)
library(dummies)
library(MASS)
library(reshape)
library(rsample)

library(skimr) #resumen numérico
library(ggthemes) #temas para personalizar gráficas.
library(tidymodels) #depuración de datos
library(tidyverse) #modelos
library(lubridate) #fechas
library(kknn) #ajuste knn
library(themis) #oversampling
library(glue) #pegar texto + variables facilmente
library(parallel) #paralelizar
library(foreach)
library(iterators)
library(dplyr)
library(foreach)
library(iterators)
library(dplyr)
library(C50) # motores
library(rpart) # motores
library(rpart.plot)  # visualizar árbol
library(vip) # importancia de variables


# Borramos variables del environment
rm(list = ls())
# Importamos la función para validación cruzada con repetición para Árboles Binarios
library(plyr)
source("cruzada arbolbin.R")



# Importamos los conjuntos de datos necesarios
Share_train <- read.csv("./Share_train.csv")
Share_test <- read.csv("./Share_test.csv")


# Cambiamos
Share_train$sadness<-ifelse(Share_train$sadness==1,"Yes","No")
Share_train$sadness <- as.factor(Share_train$sadness)

Share_test$sadness<-ifelse(Share_test$sadness==1,"Yes","No")
Share_test$sadness <- as.factor(Share_test$sadness)



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
  axis.title.y = element_text(size = 14, angle = 90,  family = "Roboto Mono"),
  axis.title.x = element_text(size = 14,  family = "Roboto Mono"),
  axis.line = element_line(colour = "grey50"),
  # Fondo
  plot.background =
    element_rect(fill = "white", color = "#fbf9f4"))


# ******************************************************************************
#             ARBOL DE REGRESION
# ******************************************************************************

# CON ARBOL: con rpart se puede tunear el cp, pero no lo hacemos por 
# ´no tener extensión a otros paquetes y modelos de árboles
# En su lugar hacemos pruebas con diferentes valores de minbucket

# *********** TUNEAMOS LOS PARAMETROS ******************************************



# Realizamos una validación cruzada simple para elegir el mejor tamaño de minbucket para nuestros datos
simple_cross_validation <- function(list_of_minbucket) {
  # Dataframe vacío con la estructura indicada
  statistics <- data.frame(minbucket   = integer(),
                           accuracy    = double(),
                           auc         = double(),
                           sensitivity = double(),
                           specificity = double()
  )
  # Validación cruzada simple con 10 grupos
  set.seed(12345)
  control<-trainControl(method = "cv",number=10, classProbs=TRUE,savePredictions = "all")
  # Fijamos el parámetro cp a cero
  arbolgrid <-  expand.grid(cp=c(0))
  
  # Comienza el bucle
  for (bucket in list_of_minbucket)
  {
    # Arbol
    arbolcaret<- train(sadness ~ .,data=Share_train,
                       method="rpart",
                       trControl=control,
                       tuneGrid=arbolgrid,
                       control = rpart.control(minbucket = bucket))
    sal<-arbolcaret$pred
    # Matriz de Confusión
    salconfu<-confusionMatrix(sal$pred,sal$obs,positive = "Yes")
    # Curva Roc
    curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
    # Estadisticos
    auc<-round(curvaroc$auc,4)
    accuracy <- round(salconfu[["overall"]][["Accuracy"]],4)
    specificity  <- round(salconfu[["byClass"]][["Specificity"]],4)
    sensitivity  <- round(salconfu[["byClass"]][["Sensitivity"]],4)
    #Insertamos en Dataframe
    statistics[nrow(statistics) + 1,] <- c(bucket,accuracy,auc,sensitivity,specificity)
  }
  return(statistics)
}


# Miramos el minbucket desde un 1% hasta un 20% de las observaciones de 30 en 30.
set.seed(12345)
list_minbucket <- seq(from=113, to=2266, by=30)
# Realizamos el bucle
cross_val_simple <- simple_cross_validation(list_minbucket)
cross_val_simple <- cross_val_simple  %>% arrange(desc(auc))

datos2 <- head(cross_val_simple,n=10)
knitr::kable(datos2, col.names = gsub("[.]", " ", names(cross_val_simple)))

# | minbucket| accuracy|    auc| sensitivity| specificity|
#   |---------:|--------:|------:|-----------:|-----------:|
#   |       113|   0.7916| 0.8137|      0.8513|      0.6879|
#   |       213|   0.7915| 0.8129|      0.8552|      0.6809|
#   |       313|   0.7915| 0.8125|      0.8719|      0.6519|
#   |       263|   0.7911| 0.8123|      0.8480|      0.6925|
#   |       513|   0.7887| 0.8120|      0.8386|      0.7021|
#   |       163|   0.7897| 0.8113|      0.8513|      0.6828|
#   |       363|   0.7892| 0.8113|      0.8499|      0.6838|
#   |       463|   0.7893| 0.8112|      0.8395|      0.7021|
#   |       413|   0.7896| 0.8110|      0.8395|      0.7031|
#   |       563|   0.7794| 0.8108|      0.8951|      0.5786|


# Tamaño óptimo encontrado
# ==============================================================================
size_optimo <- rev(cross_val_simple$minbucket)[which.max(rev(cross_val_simple$auc))]
paste("Tamaño óptimo encontrado:", size_optimo)
"Tamaño óptimo encontrado: 113"

ggplot(data = cross_val_simple, aes(x = minbucket, y = auc)) +
  geom_line() + 
  geom_point() +
  geom_vline(xintercept = size_optimo, color = "red") +
  labs(title = "AUC vs minbucket") 



t <- names(Share_train)

# sepramos los nombres por comas
a <- c()
for (i in 1:length(t)){
  a <- paste(a,t[i], sep = "','")
}
a


# ************ Elegimos tres de los mejores minbuckets ************************


source("./cruzada arbolbin.R")
medias_113<-cruzadaarbolbin(data=Share_train,
                            vardep='sadness',
                            listconti=c('gender','healthbcovid.Fair','healthbcovid.Good',
                            'healthbcovid.Poor','healthacovid.Worsened','seriousillness',
                            'diabetes','hypertension','heartdisease','otherdisease','falldown',
                            'dizziness','fatigue','prescriptiondrug','cholesteroldrug.1','highbloodpreasuredrug',
                            'coronarydiseasedrug','diabetesdrug','bronchitisdrug','lefthome','facemaskout.Always',
                            'facemaskout.Never','facemaskout.Often','facemaskout.Sometimes','keepdistance.Always',
                            'keepdistance.Often','keepdistance.Sometimes','handwash','handsanitizer','covercoughandsneeze',
                            'drugagainstcovid','anxiety','anxietylevel.Lessorthesame','anxietylevel.Moreso',
                            'troublesleeping','sleeptroublelevel.Lessorthesame','sleeptroublelevel.Moreso',
                            'loneliness.Never','loneliness.Often','lonelinesslevel.Moreso','lonelinesslevel.Notlonely',
                            'covidsymptom','positivetest','covidhospitalized','coviddeath','forgotreatment',
                            'postponedappointment','treatedhospital','employed','lostjob.No','lostjob.Yes',
                            'workplace.Noneofthese','workplace.Notworking','workplace.Usualworkplaceonly',
                            'safeworkplace.Somewhatsafe','safeworkplace.Unsafe','safeworkplace.Verysafe',
                            'reducedworkinghours.Notworking','reducedworkinghours.Yes','incomebeforecorona.High',
                            'incomebeforecorona.Low','incomebeforecorona.Refusal','financialsupport',
                            'lowestincomesincecorona.High','lowestincomesincecorona.Low','lowestincomesincecorona.Refusal',
                            'makeendsmeet.Easily','makeendsmeet.Greatdifficulty','makeendsmeet.Somedifficulty',
                            'dipsavings.No','dipsavings.Yes','personalcontactchildren.Daily','personalcontactchildren.Lessoften',
                            'personalcontactchildren.Never','personalcontactchildren.Nochildren',
                            'personalcontactchildren.Severaltimesaweek','personalcontactparent.Never',
                            'personalcontactparent.Often','personalcontactparent.Sometimes','personalcontactfamily.Never',
                            'personalcontactfamily.Often','personalcontactfamily.Sometimes',
                            'personalcontactnonrelatives.Aboutonceaweek','personalcontactnonrelatives.Lessoften',
                            'personalcontactnonrelatives.Never','personalcontactnonrelatives.Nononrelatives',
                            'personalcontactnonrelatives.Severaltimesaweek','onlinecontactchildren.Aboutonceaweek',
                            'onlinecontactchildren.Daily','onlinecontactchildren.Never','onlinecontactchildren.Nochildren',
                            'onlinecontactchildren.Severaltimesaweek','onlinecontactparents.Daily','onlinecontactparents.Never',
                            'onlinecontactparents.Noparents','onlinecontactrelatives.Daily','onlinecontactrelatives.Lessoften',
                            'onlinecontactrelatives.Never','onlinecontactrelatives.No.relatives',
                            'onlinecontactrelatives.Severaltimesaweek','onlinecontactnonrelatives.Daily',
                            'onlinecontactnonrelatives.Less.often','onlinecontactnonrelatives.Never',
                            'onlinecontactnonrelatives.Nononrelatives','onlinecontactnonrelatives.Severaltimesaweek',
                            'giveproducts','providepersonalcare','volunteering','receiveproducts','receivedpersonalcare',
                            'hhsize_update_ca','partnerinhh_update_ca','satisfactionlife.High','satisfactionlife.Low',
                            'satisfactionlife.Nodata','leftout.Never','leftout.Nodata','leftout.Often','leftout.Rarely',
                            'lookforwardeachday.Never','lookforwardeachday.Nodata','lookforwardeachday.Often',
                            'lookforwardeachday.Sometimes','lifemeaning.Never','lifemeaning.Nodata','lifemeaning.Often',
                            'lifemeaning.Sometimes','backhapp.Never','backhapp.Nodata','backhapp.Often','backhapp.Sometimes',
                            'fullenergy.Never','fullenergy.Nodata','fullenergy.Often','fullenergy.Sometimes','fulloport.Never',
                            'fulloport.Nodata','fulloport.Often','fulloport.Sometimes','futuregood.Never','futuregood.Nodata',
                            'futuregood.Often','futuregood.Sometimes','eversmokedaily','yearsmoked.Lessthan20years',
                            'yearsmoked.Morethan40years','yearsmoked.Neversmoked','yearsmoked.Nodata','drinklastsevendays',
                            'sixormoredrinks.Never','sixormoredrinks.Nodata','sixormoredrinks.Rarely','StringencyIndex',
                            'GovernmentResponseIndex','EconomicSupportIndex','ContainmentHealthIndex','ConfirmedDeaths',
                            'ConfirmedCases','age','numberillness.None','numberillness.One','numberillness.Two',
                            'numdrugs','closepositive','otherpositive','country_wealth.Poor','country_wealth.Rich',
                            'country_wealth.VeryRich','country_politics.Left','country_politics.Other',
                            'country_politics.Right','country_region.East','country_region.North','country_region.South'),
                            listclass=c(""),grupos=10,sinicio=12345,repe=200, cp=c(0),minbucket =113)

medias_313<-cruzadaarbolbin(data=Share_train,
                            vardep='sadness',
                            listconti=c('gender','healthbcovid.Fair','healthbcovid.Good',
                                        'healthbcovid.Poor','healthacovid.Worsened','seriousillness',
                                        'diabetes','hypertension','heartdisease','otherdisease','falldown',
                                        'dizziness','fatigue','prescriptiondrug','cholesteroldrug.1','highbloodpreasuredrug',
                                        'coronarydiseasedrug','diabetesdrug','bronchitisdrug','lefthome','facemaskout.Always',
                                        'facemaskout.Never','facemaskout.Often','facemaskout.Sometimes','keepdistance.Always',
                                        'keepdistance.Often','keepdistance.Sometimes','handwash','handsanitizer','covercoughandsneeze',
                                        'drugagainstcovid','anxiety','anxietylevel.Lessorthesame','anxietylevel.Moreso',
                                        'troublesleeping','sleeptroublelevel.Lessorthesame','sleeptroublelevel.Moreso',
                                        'loneliness.Never','loneliness.Often','lonelinesslevel.Moreso','lonelinesslevel.Notlonely',
                                        'covidsymptom','positivetest','covidhospitalized','coviddeath','forgotreatment',
                                        'postponedappointment','treatedhospital','employed','lostjob.No','lostjob.Yes',
                                        'workplace.Noneofthese','workplace.Notworking','workplace.Usualworkplaceonly',
                                        'safeworkplace.Somewhatsafe','safeworkplace.Unsafe','safeworkplace.Verysafe',
                                        'reducedworkinghours.Notworking','reducedworkinghours.Yes','incomebeforecorona.High',
                                        'incomebeforecorona.Low','incomebeforecorona.Refusal','financialsupport',
                                        'lowestincomesincecorona.High','lowestincomesincecorona.Low','lowestincomesincecorona.Refusal',
                                        'makeendsmeet.Easily','makeendsmeet.Greatdifficulty','makeendsmeet.Somedifficulty',
                                        'dipsavings.No','dipsavings.Yes','personalcontactchildren.Daily','personalcontactchildren.Lessoften',
                                        'personalcontactchildren.Never','personalcontactchildren.Nochildren',
                                        'personalcontactchildren.Severaltimesaweek','personalcontactparent.Never',
                                        'personalcontactparent.Often','personalcontactparent.Sometimes','personalcontactfamily.Never',
                                        'personalcontactfamily.Often','personalcontactfamily.Sometimes',
                                        'personalcontactnonrelatives.Aboutonceaweek','personalcontactnonrelatives.Lessoften',
                                        'personalcontactnonrelatives.Never','personalcontactnonrelatives.Nononrelatives',
                                        'personalcontactnonrelatives.Severaltimesaweek','onlinecontactchildren.Aboutonceaweek',
                                        'onlinecontactchildren.Daily','onlinecontactchildren.Never','onlinecontactchildren.Nochildren',
                                        'onlinecontactchildren.Severaltimesaweek','onlinecontactparents.Daily','onlinecontactparents.Never',
                                        'onlinecontactparents.Noparents','onlinecontactrelatives.Daily','onlinecontactrelatives.Lessoften',
                                        'onlinecontactrelatives.Never','onlinecontactrelatives.No.relatives',
                                        'onlinecontactrelatives.Severaltimesaweek','onlinecontactnonrelatives.Daily',
                                        'onlinecontactnonrelatives.Less.often','onlinecontactnonrelatives.Never',
                                        'onlinecontactnonrelatives.Nononrelatives','onlinecontactnonrelatives.Severaltimesaweek',
                                        'giveproducts','providepersonalcare','volunteering','receiveproducts','receivedpersonalcare',
                                        'hhsize_update_ca','partnerinhh_update_ca','satisfactionlife.High','satisfactionlife.Low',
                                        'satisfactionlife.Nodata','leftout.Never','leftout.Nodata','leftout.Often','leftout.Rarely',
                                        'lookforwardeachday.Never','lookforwardeachday.Nodata','lookforwardeachday.Often',
                                        'lookforwardeachday.Sometimes','lifemeaning.Never','lifemeaning.Nodata','lifemeaning.Often',
                                        'lifemeaning.Sometimes','backhapp.Never','backhapp.Nodata','backhapp.Often','backhapp.Sometimes',
                                        'fullenergy.Never','fullenergy.Nodata','fullenergy.Often','fullenergy.Sometimes','fulloport.Never',
                                        'fulloport.Nodata','fulloport.Often','fulloport.Sometimes','futuregood.Never','futuregood.Nodata',
                                        'futuregood.Often','futuregood.Sometimes','eversmokedaily','yearsmoked.Lessthan20years',
                                        'yearsmoked.Morethan40years','yearsmoked.Neversmoked','yearsmoked.Nodata','drinklastsevendays',
                                        'sixormoredrinks.Never','sixormoredrinks.Nodata','sixormoredrinks.Rarely','StringencyIndex',
                                        'GovernmentResponseIndex','EconomicSupportIndex','ContainmentHealthIndex','ConfirmedDeaths',
                                        'ConfirmedCases','age','numberillness.None','numberillness.One','numberillness.Two',
                                        'numdrugs','closepositive','otherpositive','country_wealth.Poor','country_wealth.Rich',
                                        'country_wealth.VeryRich','country_politics.Left','country_politics.Other',
                                        'country_politics.Right','country_region.East','country_region.North','country_region.South'),
                            listclass=c(""),grupos=10,sinicio=12345,repe=200, cp=c(0),minbucket =313)

medias_263<-cruzadaarbolbin(data=Share_train,
                            vardep='sadness',
                            listconti=c('gender','healthbcovid.Fair','healthbcovid.Good',
                                        'healthbcovid.Poor','healthacovid.Worsened','seriousillness',
                                        'diabetes','hypertension','heartdisease','otherdisease','falldown',
                                        'dizziness','fatigue','prescriptiondrug','cholesteroldrug.1','highbloodpreasuredrug',
                                        'coronarydiseasedrug','diabetesdrug','bronchitisdrug','lefthome','facemaskout.Always',
                                        'facemaskout.Never','facemaskout.Often','facemaskout.Sometimes','keepdistance.Always',
                                        'keepdistance.Often','keepdistance.Sometimes','handwash','handsanitizer','covercoughandsneeze',
                                        'drugagainstcovid','anxiety','anxietylevel.Lessorthesame','anxietylevel.Moreso',
                                        'troublesleeping','sleeptroublelevel.Lessorthesame','sleeptroublelevel.Moreso',
                                        'loneliness.Never','loneliness.Often','lonelinesslevel.Moreso','lonelinesslevel.Notlonely',
                                        'covidsymptom','positivetest','covidhospitalized','coviddeath','forgotreatment',
                                        'postponedappointment','treatedhospital','employed','lostjob.No','lostjob.Yes',
                                        'workplace.Noneofthese','workplace.Notworking','workplace.Usualworkplaceonly',
                                        'safeworkplace.Somewhatsafe','safeworkplace.Unsafe','safeworkplace.Verysafe',
                                        'reducedworkinghours.Notworking','reducedworkinghours.Yes','incomebeforecorona.High',
                                        'incomebeforecorona.Low','incomebeforecorona.Refusal','financialsupport',
                                        'lowestincomesincecorona.High','lowestincomesincecorona.Low','lowestincomesincecorona.Refusal',
                                        'makeendsmeet.Easily','makeendsmeet.Greatdifficulty','makeendsmeet.Somedifficulty',
                                        'dipsavings.No','dipsavings.Yes','personalcontactchildren.Daily','personalcontactchildren.Lessoften',
                                        'personalcontactchildren.Never','personalcontactchildren.Nochildren',
                                        'personalcontactchildren.Severaltimesaweek','personalcontactparent.Never',
                                        'personalcontactparent.Often','personalcontactparent.Sometimes','personalcontactfamily.Never',
                                        'personalcontactfamily.Often','personalcontactfamily.Sometimes',
                                        'personalcontactnonrelatives.Aboutonceaweek','personalcontactnonrelatives.Lessoften',
                                        'personalcontactnonrelatives.Never','personalcontactnonrelatives.Nononrelatives',
                                        'personalcontactnonrelatives.Severaltimesaweek','onlinecontactchildren.Aboutonceaweek',
                                        'onlinecontactchildren.Daily','onlinecontactchildren.Never','onlinecontactchildren.Nochildren',
                                        'onlinecontactchildren.Severaltimesaweek','onlinecontactparents.Daily','onlinecontactparents.Never',
                                        'onlinecontactparents.Noparents','onlinecontactrelatives.Daily','onlinecontactrelatives.Lessoften',
                                        'onlinecontactrelatives.Never','onlinecontactrelatives.No.relatives',
                                        'onlinecontactrelatives.Severaltimesaweek','onlinecontactnonrelatives.Daily',
                                        'onlinecontactnonrelatives.Less.often','onlinecontactnonrelatives.Never',
                                        'onlinecontactnonrelatives.Nononrelatives','onlinecontactnonrelatives.Severaltimesaweek',
                                        'giveproducts','providepersonalcare','volunteering','receiveproducts','receivedpersonalcare',
                                        'hhsize_update_ca','partnerinhh_update_ca','satisfactionlife.High','satisfactionlife.Low',
                                        'satisfactionlife.Nodata','leftout.Never','leftout.Nodata','leftout.Often','leftout.Rarely',
                                        'lookforwardeachday.Never','lookforwardeachday.Nodata','lookforwardeachday.Often',
                                        'lookforwardeachday.Sometimes','lifemeaning.Never','lifemeaning.Nodata','lifemeaning.Often',
                                        'lifemeaning.Sometimes','backhapp.Never','backhapp.Nodata','backhapp.Often','backhapp.Sometimes',
                                        'fullenergy.Never','fullenergy.Nodata','fullenergy.Often','fullenergy.Sometimes','fulloport.Never',
                                        'fulloport.Nodata','fulloport.Often','fulloport.Sometimes','futuregood.Never','futuregood.Nodata',
                                        'futuregood.Often','futuregood.Sometimes','eversmokedaily','yearsmoked.Lessthan20years',
                                        'yearsmoked.Morethan40years','yearsmoked.Neversmoked','yearsmoked.Nodata','drinklastsevendays',
                                        'sixormoredrinks.Never','sixormoredrinks.Nodata','sixormoredrinks.Rarely','StringencyIndex',
                                        'GovernmentResponseIndex','EconomicSupportIndex','ContainmentHealthIndex','ConfirmedDeaths',
                                        'ConfirmedCases','age','numberillness.None','numberillness.One','numberillness.Two',
                                        'numdrugs','closepositive','otherpositive','country_wealth.Poor','country_wealth.Rich',
                                        'country_wealth.VeryRich','country_politics.Left','country_politics.Other',
                                        'country_politics.Right','country_region.East','country_region.North','country_region.South'),
                            listclass=c(""),grupos=10,sinicio=12345,repe=200, cp=c(0),minbucket =263)


medias_513<-cruzadaarbolbin(data=Share_train,
                            vardep='sadness',
                            listconti=c('gender','healthbcovid.Fair','healthbcovid.Good',
                                        'healthbcovid.Poor','healthacovid.Worsened','seriousillness',
                                        'diabetes','hypertension','heartdisease','otherdisease','falldown',
                                        'dizziness','fatigue','prescriptiondrug','cholesteroldrug.1','highbloodpreasuredrug',
                                        'coronarydiseasedrug','diabetesdrug','bronchitisdrug','lefthome','facemaskout.Always',
                                        'facemaskout.Never','facemaskout.Often','facemaskout.Sometimes','keepdistance.Always',
                                        'keepdistance.Often','keepdistance.Sometimes','handwash','handsanitizer','covercoughandsneeze',
                                        'drugagainstcovid','anxiety','anxietylevel.Lessorthesame','anxietylevel.Moreso',
                                        'troublesleeping','sleeptroublelevel.Lessorthesame','sleeptroublelevel.Moreso',
                                        'loneliness.Never','loneliness.Often','lonelinesslevel.Moreso','lonelinesslevel.Notlonely',
                                        'covidsymptom','positivetest','covidhospitalized','coviddeath','forgotreatment',
                                        'postponedappointment','treatedhospital','employed','lostjob.No','lostjob.Yes',
                                        'workplace.Noneofthese','workplace.Notworking','workplace.Usualworkplaceonly',
                                        'safeworkplace.Somewhatsafe','safeworkplace.Unsafe','safeworkplace.Verysafe',
                                        'reducedworkinghours.Notworking','reducedworkinghours.Yes','incomebeforecorona.High',
                                        'incomebeforecorona.Low','incomebeforecorona.Refusal','financialsupport',
                                        'lowestincomesincecorona.High','lowestincomesincecorona.Low','lowestincomesincecorona.Refusal',
                                        'makeendsmeet.Easily','makeendsmeet.Greatdifficulty','makeendsmeet.Somedifficulty',
                                        'dipsavings.No','dipsavings.Yes','personalcontactchildren.Daily','personalcontactchildren.Lessoften',
                                        'personalcontactchildren.Never','personalcontactchildren.Nochildren',
                                        'personalcontactchildren.Severaltimesaweek','personalcontactparent.Never',
                                        'personalcontactparent.Often','personalcontactparent.Sometimes','personalcontactfamily.Never',
                                        'personalcontactfamily.Often','personalcontactfamily.Sometimes',
                                        'personalcontactnonrelatives.Aboutonceaweek','personalcontactnonrelatives.Lessoften',
                                        'personalcontactnonrelatives.Never','personalcontactnonrelatives.Nononrelatives',
                                        'personalcontactnonrelatives.Severaltimesaweek','onlinecontactchildren.Aboutonceaweek',
                                        'onlinecontactchildren.Daily','onlinecontactchildren.Never','onlinecontactchildren.Nochildren',
                                        'onlinecontactchildren.Severaltimesaweek','onlinecontactparents.Daily','onlinecontactparents.Never',
                                        'onlinecontactparents.Noparents','onlinecontactrelatives.Daily','onlinecontactrelatives.Lessoften',
                                        'onlinecontactrelatives.Never','onlinecontactrelatives.No.relatives',
                                        'onlinecontactrelatives.Severaltimesaweek','onlinecontactnonrelatives.Daily',
                                        'onlinecontactnonrelatives.Less.often','onlinecontactnonrelatives.Never',
                                        'onlinecontactnonrelatives.Nononrelatives','onlinecontactnonrelatives.Severaltimesaweek',
                                        'giveproducts','providepersonalcare','volunteering','receiveproducts','receivedpersonalcare',
                                        'hhsize_update_ca','partnerinhh_update_ca','satisfactionlife.High','satisfactionlife.Low',
                                        'satisfactionlife.Nodata','leftout.Never','leftout.Nodata','leftout.Often','leftout.Rarely',
                                        'lookforwardeachday.Never','lookforwardeachday.Nodata','lookforwardeachday.Often',
                                        'lookforwardeachday.Sometimes','lifemeaning.Never','lifemeaning.Nodata','lifemeaning.Often',
                                        'lifemeaning.Sometimes','backhapp.Never','backhapp.Nodata','backhapp.Often','backhapp.Sometimes',
                                        'fullenergy.Never','fullenergy.Nodata','fullenergy.Often','fullenergy.Sometimes','fulloport.Never',
                                        'fulloport.Nodata','fulloport.Often','fulloport.Sometimes','futuregood.Never','futuregood.Nodata',
                                        'futuregood.Often','futuregood.Sometimes','eversmokedaily','yearsmoked.Lessthan20years',
                                        'yearsmoked.Morethan40years','yearsmoked.Neversmoked','yearsmoked.Nodata','drinklastsevendays',
                                        'sixormoredrinks.Never','sixormoredrinks.Nodata','sixormoredrinks.Rarely','StringencyIndex',
                                        'GovernmentResponseIndex','EconomicSupportIndex','ContainmentHealthIndex','ConfirmedDeaths',
                                        'ConfirmedCases','age','numberillness.None','numberillness.One','numberillness.Two',
                                        'numdrugs','closepositive','otherpositive','country_wealth.Poor','country_wealth.Rich',
                                        'country_wealth.VeryRich','country_politics.Left','country_politics.Other',
                                        'country_politics.Right','country_region.East','country_region.North','country_region.South'),
                            listclass=c(""),grupos=10,sinicio=12345,repe=200, cp=c(0),minbucket =513)
#-------------------------------------------------------
medias_113$modelo  = "Árbol 113"
medias_213$modelo  = "Árbol 213"
medias_263$modelo  = "Árbol 263"
medias_513$modelo  = "Árbol 513"
#-------------------------------------------------------
union<-rbind(medias_113, medias_213, medias_263, medias_513)


# Los representamos
par(cex.axis=0.5)
library(viridis)
ggplot(union, aes(x=modelo, y=auc, fill=modelo)) +
  scale_fill_manual(values = met.brewer("Hokusai1", 4))+
  geom_boxplot(outlier.colour="black", outlier.shape=1,
               outlier.size=2) +
  labs(x = "Modelos de árbol", y = 'AUC', title = "Boxplot vc repetida árboles")  



# ------------------- Ploteamos el arbol del modelo ganador -------------------


set.seed(12345)
arbol_ganador <- rpart(sadness ~ ., 
                       data = Share_train,
                       minbucket =113,
                       method = "class",
                       parms=list(split="gini"),
                       maxsurrogate = 0)

arbol_ganador2 <- rpart(sadness ~ ., 
                       data = Share_train,
                       minbucket =263,
                       method = "class",
                       parms=list(split="gini"),
                       maxsurrogate = 0)


rpart.plot(arbol_ganador,extra=105,nn=TRUE) 

# ------------------ VEmos la importancia de las variables --------------------

par(cex=1.2)
imp <- arbol_ganador$variable.importance
imp_data <- as.data.frame(imp)
imp_data <- tibble::rownames_to_column(imp_data, "Variables") %>% arrange(desc(imp))

library(forcats)
imp_data %>%
  mutate(Variables = fct_reorder(as.factor(Variables), imp)) %>%
  ggplot( aes(x=Variables, y=imp)) +
  geom_bar(stat="identity", fill="#df7e66", width=.4)+
  labs(title =  "Importancia variables arbol ganador")+
  coord_flip() 

# ---------------- Predicciones en el conjunto test ----------------------------

# Predicciones en Test
prediccion1 <- predict(arbol_ganador, newdata = Share_test, type = "prob")
prediccion1 <- as.data.frame(prediccion1) 
prediccion_yes1 <- prediccion1 %>% select(Yes)
# Valor del AUC
roc_curve <- roc(response=Share_test$sadness,predictor=prediccion1$Yes)
roc_curve[["auc"]]
# 0.8325

# Matriz de confusion
prediccion <- predict(arbol_ganador, newdata = Share_test, type = "class")
conf_mat <- confusionMatrix(prediccion, Share_test[["sadness"]], positive = "Yes")
conf_mat <- as.data.frame(conf_mat[["table"]])
conf_mat
# 
# Prediction Reference Freq
# 1         No        No  786
# 2        Yes        No  250
# 3         No       Yes  336
# 4        Yes       Yes 1462


prediccion_yes1$model <- "Arbol113"

prediccion2 <- predict(arbol_ganador2, newdata = Share_test, type = "prob")
prediccion2 <- as.data.frame(prediccion2) 
prediccion_yes2 <- prediccion2 %>% select(Yes)
# Valor del AUC
roc_curve <- roc(response=Share_test$sadness,predictor=prediccion2$Yes)
roc_curve[["auc"]]
# 0.8664

prediccion_yes2$model <- "Arbol213"

fresa <- cbind(Share_test[, c(1:175)], prediccion_yes1)
fresa2 <- cbind(Share_test[, c(1:175)], prediccion_yes2)

fresauni <- rbind(fresa,fresa2)
fresauni <- fresauni %>% select(sadness, Yes, model)


library(ggplot2)
library(plotROC)
library(Tableau)
ggplot(fresauni, aes(d = sadness, m = Yes, color = model)) + geom_roc(labels= FALSE, cutoffs.at = FALSE) +
  # coord_equal() + # cuadrada
  scale_color_manual(values = c("#e09351","#94b594")) +
  # Límites de coordenadas y escalas de los ejes
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     expand = c(0.005, 0.005)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     expand = c(0.005, 0.005)) + 
  geom_abline(intercept = 0, slope = 1, lty = 4, lwd = 0.8)+
  theme(panel.grid.major = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(size=20), 
        plot.background =
          element_rect(fill = "#fbf9f4", color = "#fbf9f4"))+
      labs(x = "1 - especificidad (proporción falsos positivos)",
       y = "sensibilidad (proporción verdaderos positivos)",
       color = "Modelos árboles",
       title = "COMPARATIVA DE MODELOS ARBOLES CLASIFICACION",
       subtitle = glue("Métricas usadas: AUC, accuracy, sensib., especif."),
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                "Datos: Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)"))



# ******************************************************************************
#                 MODELO DE RANDOM FOREST
# ******************************************************************************



# Primero vemos como varía el error a medida que aumenta el nçumero de arboles
# para ver cuándo se estabiliza dicho error
# Establecemos el minbucket que sacamos del apartado anterior y utilizamos el tamaño
# todal del dataset



# Validación empleando k-cross-validation (root mean squared error)
# ==============================================================================

# Valores evaluados
num_trees_range <- seq(2, 5000, 100)

# Bucle para entrenar un modelo con cada valor de num_trees y extraer su error
# de entrenamiento y de Out-of-Bag.

train_errors <- rep(NA, times = length(num_trees_range))
cv_errors    <- rep(NA, times = length(num_trees_range))
library(parsnip)
library(rsample)
library(recipes)
library(workflows)
library(dplyr)
library(tune)
library(yardstick)

# bucle for que recorrre cada número de árboles establecido anteriormente

Met <- metric_set(roc_auc, yardstick::accuracy, yardstick::sensitivity, specificity)

control<-trainControl(method = "cv",number=5,savePredictions = "all", classProbs=TRUE)

statistics <-   data.frame(n_tree = integer(),
                           accuracy    = double(),
                           auc         = double(),
                           sensitivity = double())

# Valores evaluados


for (n_tree in seq(from=1, to=5000, by=1000)){
    set.seed(12345)
    # Ajustamos
    rf<- train(data=Share_train,
               sadness ~.,
               method="rf",trControl=control,
               linout = FALSE,ntree=n_tree,nodesize=113,replace=TRUE)
    # Predicciones para  obtener métricas
    sal<-rf$pred
    salconfu<-confusionMatrix(sal$pred,sal$obs,positive = "Yes")
    curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
    auc<-curvaroc$auc
    # Estadisticos
    auc<-round(curvaroc$auc,4)
    accuracy <- round(salconfu[["overall"]][["Accuracy"]],4)
    sensitivity  <- round(salconfu[["byClass"]][["Sensitivity"]],4)
    
    #Insertamos en Dataframe
    statistics[nrow(statistics) + 1,] <- c(n_tree,accuracy,auc,sensitivity)}


# Gráfico con la evolución de los errores
library(ggplot2)
ggplot(data = statistics) +
  geom_line(aes(x = n_tree, y = auc, color = "AUC"))+
  geom_vline(xintercept = 1000,
             color = "firebrick",
             linetype = "dashed") +
  labs(
    title = "Evolución del AUC vs número árboles",
    x     = "número de árboles",
    y     = "AUC",
    color = ""
  ) +
  theme(legend.position = "bottom")


# Sacamos el valor óptimo numéricamente
paste("Valor óptimo de num.trees:", n_tree[which.max(auc)])
# [1] "Valor óptimo de num.trees: 4001

#Vemos que a partir de las 1000 iteraciones se estabiliza, pondremos 1200 para asegurarnos.

# ********************* Tuneamos los parámetros del Random Forest ***************

# Realizamos validación cruzada para modelizar el tamaño de la muestra  y el minbucket.


# Validación cruzada simple 
set.seed(12345)
control<-trainControl(method = "cv",number=4, classProbs=TRUE,savePredictions = "all")
# Generamos dataset vacío con los estadísticos que queremos
statistics <- data.frame(samplesize = integer(),
                         nodesize   = integer(),
                         accuracy    = double(),
                         auc         = double(),
                         sensitivity = double())

# Lista con el top de sample_size que emplearemos
sample_size_size <- as.integer(trunc((3/4)*nrow(Share_train)))

# Iteramos el tamaño de la muestra y el nodesize (luego tunearemos el mtry según los mejores valores 
# sacados en este bucle).
# s_size es el tamaño de la muestra y va de 3000 hasta  3/4 del número de filas de 1000 en 1000
# n_size es el nodesize y va desde el 2% hasta un 20% de las observaciones de 1000 en 1000.
for (sample_size in seq(from=3000, to=sample_size_size, by=1000))
{
  # Lista con el top de minbucket que emplearemos
  for (node_size in seq(from=round(sample_size*0.02), to=round(sample_size*0.2), by=500))
  {
    rfgrid<-expand.grid(mtry=c(175))
    set.seed(12345)
    print(node_size)
    print(sample_size)
    rf<- train(data=Share_train,sadness ~.,
               method="rf",
               trControl=control,
               tuneGrid=rfgrid,
               linout = FALSE,
               sample_size=sample_size,
               ntree=1200,
               nodesize=node_size,
               replace=TRUE)
    
    sal<-rf$pred
    
    salconfu<-confusionMatrix(sal$pred,sal$obs,positive = "Yes")
    curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
    # Estadisticos
    auc<-round(curvaroc$auc,4)
    accuracy <- round(salconfu[["overall"]][["Accuracy"]],4)
    sensitivity  <- round(salconfu[["byClass"]][["Sensitivity"]],4)
    #Insertamos en Dataframe
    statistics[nrow(statistics) + 1,] <- c(sample_size,node_size,accuracy,auc,sensitivity)
  }
}

# Reordenamos Dataset
statistics <- statistics %>% arrange(desc(auc))

datos2 <- head(statistics,n=5)

knitr::kable(datos2, col.names = gsub("[.]", " ", names(statistics)))


# | samplesize| nodesize| accuracy|    auc| sensitivity|
#   |----------:|--------:|--------:|------:|-----------:|
#   |       3000|       30|   0.8004| 0.8496|      0.8612|
#   |       3000|       60|   0.8014| 0.8493|      0.8666|
#   |       4000|       80|   0.8021| 0.8485|      0.8698|
#   |       5000|      100|   0.7999| 0.8459|      0.8688|
#   |       6000|      120|   0.8011| 0.8450|      0.8681|


# Hacemos un bucle teniendo esto en cuenta y tuneamos el parametro mtry
# cogemos tambien el minbucket ganador de arboles 113.

set.seed(12345)
control<-trainControl(method = "cv",number=5, classProbs=TRUE,savePredictions = "all")
# Dataframe
statisticsrf <- data.frame(samplesize = integer(),
                         nodesize   = integer(),
                         mtry = integer(),
                         accuracy    = double(),
                         auc         = double(),
                         sensitivity = double())
# Triple bucle
for (s_size in c(4000,3000,5000))
{
  for (n_size in c(30,60,100,113,213))
  {
    for (n_mtry in seq(from =35, to=174, by=20))
    {
      rfgrid<-expand.grid(mtry=c(n_mtry))
      print(s_size)
      print(n_size)
      print(n_mtry)
      set.seed(12345)
      rf<- train(data=Share_train ,
                 sadness ~.,
                 method="rf",trControl=control,tuneGrid=rfgrid,
                 linout = FALSE,sample_size=s_size,
                 ntree=1200,nodesize=n_size,replace=TRUE)
      #Predicciones para estadísticos
      sal<-rf$pred
      salconfu<-confusionMatrix(sal$pred,sal$obs,positive = "Yes")
      curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
      auc<-curvaroc$auc
      # Estadisticos
      auc<-round(curvaroc$auc,4)
      accuracy <- round(salconfu[["overall"]][["Accuracy"]],4)
      sensitivity  <- round(salconfu[["byClass"]][["Sensitivity"]],4)
      #Insertamos en Dataframe
      statisticsrf[nrow(statisticsrf) + 1,] <- c(s_size,n_size,n_mtry,accuracy,auc,sensitivity)
    }
  }
}


# Reordenamos Dataset
statisticsrf <- statisticsrf %>% arrange(desc(auc))

datosrf <- head(statisticsrf,n=10)

knitr::kable(datosrf, col.names = gsub("[.]", " ", names(statisticsrf)))

# | samplesize| nodesize| mtry| accuracy|    auc| sensitivity|
#   |----------:|--------:|----:|--------:|------:|-----------:|
#   |       4000|       30|   35|   0.8022| 0.8550|      0.8627|
#   |       3000|       30|   35|   0.8022| 0.8550|      0.8627|
#   |       4000|       40|   30|   0.8037| 0.8549|      0.8631|
#   |       4000|       60|   35|   0.8027| 0.8540|      0.8610|
#   |       3000|       60|   35|   0.8027| 0.8540|      0.8610|
#   |       5000|       60|   35|   0.8027| 0.8540|      0.8610|
#   |       4000|       30|   55|   0.8026| 0.8539|      0.8615|
#   |       3000|       30|   55|   0.8026| 0.8539|      0.8615|
#   |       5000|       30|   55|   0.8026| 0.8539|      0.8615|



# Escogemos los 3 mejores modelos y realizamos validación cruzada repetida.

source("./cruzada rf binaria.R")
mediasrf_4000<-cruzadarfbin(data=Share_train,
                            vardep='sadness',
                            listconti=c('gender','healthbcovid.Fair','healthbcovid.Good',
                                        'healthbcovid.Poor','healthacovid.Worsened','seriousillness',
                                        'diabetes','hypertension','heartdisease','otherdisease','falldown',
                                        'dizziness','fatigue','prescriptiondrug','cholesteroldrug.1','highbloodpreasuredrug',
                                        'coronarydiseasedrug','diabetesdrug','bronchitisdrug','lefthome','facemaskout.Always',
                                        'facemaskout.Never','facemaskout.Often','facemaskout.Sometimes','keepdistance.Always',
                                        'keepdistance.Often','keepdistance.Sometimes','handwash','handsanitizer','covercoughandsneeze',
                                        'drugagainstcovid','anxiety','anxietylevel.Lessorthesame','anxietylevel.Moreso',
                                        'troublesleeping','sleeptroublelevel.Lessorthesame','sleeptroublelevel.Moreso',
                                        'loneliness.Never','loneliness.Often','lonelinesslevel.Moreso','lonelinesslevel.Notlonely',
                                        'covidsymptom','positivetest','covidhospitalized','coviddeath','forgotreatment',
                                        'postponedappointment','treatedhospital','employed','lostjob.No','lostjob.Yes',
                                        'workplace.Noneofthese','workplace.Notworking','workplace.Usualworkplaceonly',
                                        'safeworkplace.Somewhatsafe','safeworkplace.Unsafe','safeworkplace.Verysafe',
                                        'reducedworkinghours.Notworking','reducedworkinghours.Yes','incomebeforecorona.High',
                                        'incomebeforecorona.Low','incomebeforecorona.Refusal','financialsupport',
                                        'lowestincomesincecorona.High','lowestincomesincecorona.Low','lowestincomesincecorona.Refusal',
                                        'makeendsmeet.Easily','makeendsmeet.Greatdifficulty','makeendsmeet.Somedifficulty',
                                        'dipsavings.No','dipsavings.Yes','personalcontactchildren.Daily','personalcontactchildren.Lessoften',
                                        'personalcontactchildren.Never','personalcontactchildren.Nochildren',
                                        'personalcontactchildren.Severaltimesaweek','personalcontactparent.Never',
                                        'personalcontactparent.Often','personalcontactparent.Sometimes','personalcontactfamily.Never',
                                        'personalcontactfamily.Often','personalcontactfamily.Sometimes',
                                        'personalcontactnonrelatives.Aboutonceaweek','personalcontactnonrelatives.Lessoften',
                                        'personalcontactnonrelatives.Never','personalcontactnonrelatives.Nononrelatives',
                                        'personalcontactnonrelatives.Severaltimesaweek','onlinecontactchildren.Aboutonceaweek',
                                        'onlinecontactchildren.Daily','onlinecontactchildren.Never','onlinecontactchildren.Nochildren',
                                        'onlinecontactchildren.Severaltimesaweek','onlinecontactparents.Daily','onlinecontactparents.Never',
                                        'onlinecontactparents.Noparents','onlinecontactrelatives.Daily','onlinecontactrelatives.Lessoften',
                                        'onlinecontactrelatives.Never','onlinecontactrelatives.No.relatives',
                                        'onlinecontactrelatives.Severaltimesaweek','onlinecontactnonrelatives.Daily',
                                        'onlinecontactnonrelatives.Less.often','onlinecontactnonrelatives.Never',
                                        'onlinecontactnonrelatives.Nononrelatives','onlinecontactnonrelatives.Severaltimesaweek',
                                        'giveproducts','providepersonalcare','volunteering','receiveproducts','receivedpersonalcare',
                                        'hhsize_update_ca','partnerinhh_update_ca','satisfactionlife.High','satisfactionlife.Low',
                                        'satisfactionlife.Nodata','leftout.Never','leftout.Nodata','leftout.Often','leftout.Rarely',
                                        'lookforwardeachday.Never','lookforwardeachday.Nodata','lookforwardeachday.Often',
                                        'lookforwardeachday.Sometimes','lifemeaning.Never','lifemeaning.Nodata','lifemeaning.Often',
                                        'lifemeaning.Sometimes','backhapp.Never','backhapp.Nodata','backhapp.Often','backhapp.Sometimes',
                                        'fullenergy.Never','fullenergy.Nodata','fullenergy.Often','fullenergy.Sometimes','fulloport.Never',
                                        'fulloport.Nodata','fulloport.Often','fulloport.Sometimes','futuregood.Never','futuregood.Nodata',
                                        'futuregood.Often','futuregood.Sometimes','eversmokedaily','yearsmoked.Lessthan20years',
                                        'yearsmoked.Morethan40years','yearsmoked.Neversmoked','yearsmoked.Nodata','drinklastsevendays',
                                        'sixormoredrinks.Never','sixormoredrinks.Nodata','sixormoredrinks.Rarely','StringencyIndex',
                                        'GovernmentResponseIndex','EconomicSupportIndex','ContainmentHealthIndex','ConfirmedDeaths',
                                        'ConfirmedCases','age','numberillness.None','numberillness.One','numberillness.Two',
                                        'numdrugs','closepositive','otherpositive','country_wealth.Poor','country_wealth.Rich',
                                        'country_wealth.VeryRich','country_politics.Left','country_politics.Other',
                                        'country_politics.Right','country_region.East','country_region.North','country_region.South'),
                            listclass=c(""),grupos=4, sinicio=12345,repe=20,nodesize=30,replace=TRUE,ntree=1200,mtry=35, sampsize = 4000)


mediasrf_3000<-cruzadarfbin(data=Share_train,
                            vardep='sadness',
                            listconti=c('gender','healthbcovid.Fair','healthbcovid.Good',
                                        'healthbcovid.Poor','healthacovid.Worsened','seriousillness',
                                        'diabetes','hypertension','heartdisease','otherdisease','falldown',
                                        'dizziness','fatigue','prescriptiondrug','cholesteroldrug.1','highbloodpreasuredrug',
                                        'coronarydiseasedrug','diabetesdrug','bronchitisdrug','lefthome','facemaskout.Always',
                                        'facemaskout.Never','facemaskout.Often','facemaskout.Sometimes','keepdistance.Always',
                                        'keepdistance.Often','keepdistance.Sometimes','handwash','handsanitizer','covercoughandsneeze',
                                        'drugagainstcovid','anxiety','anxietylevel.Lessorthesame','anxietylevel.Moreso',
                                        'troublesleeping','sleeptroublelevel.Lessorthesame','sleeptroublelevel.Moreso',
                                        'loneliness.Never','loneliness.Often','lonelinesslevel.Moreso','lonelinesslevel.Notlonely',
                                        'covidsymptom','positivetest','covidhospitalized','coviddeath','forgotreatment',
                                        'postponedappointment','treatedhospital','employed','lostjob.No','lostjob.Yes',
                                        'workplace.Noneofthese','workplace.Notworking','workplace.Usualworkplaceonly',
                                        'safeworkplace.Somewhatsafe','safeworkplace.Unsafe','safeworkplace.Verysafe',
                                        'reducedworkinghours.Notworking','reducedworkinghours.Yes','incomebeforecorona.High',
                                        'incomebeforecorona.Low','incomebeforecorona.Refusal','financialsupport',
                                        'lowestincomesincecorona.High','lowestincomesincecorona.Low','lowestincomesincecorona.Refusal',
                                        'makeendsmeet.Easily','makeendsmeet.Greatdifficulty','makeendsmeet.Somedifficulty',
                                        'dipsavings.No','dipsavings.Yes','personalcontactchildren.Daily','personalcontactchildren.Lessoften',
                                        'personalcontactchildren.Never','personalcontactchildren.Nochildren',
                                        'personalcontactchildren.Severaltimesaweek','personalcontactparent.Never',
                                        'personalcontactparent.Often','personalcontactparent.Sometimes','personalcontactfamily.Never',
                                        'personalcontactfamily.Often','personalcontactfamily.Sometimes',
                                        'personalcontactnonrelatives.Aboutonceaweek','personalcontactnonrelatives.Lessoften',
                                        'personalcontactnonrelatives.Never','personalcontactnonrelatives.Nononrelatives',
                                        'personalcontactnonrelatives.Severaltimesaweek','onlinecontactchildren.Aboutonceaweek',
                                        'onlinecontactchildren.Daily','onlinecontactchildren.Never','onlinecontactchildren.Nochildren',
                                        'onlinecontactchildren.Severaltimesaweek','onlinecontactparents.Daily','onlinecontactparents.Never',
                                        'onlinecontactparents.Noparents','onlinecontactrelatives.Daily','onlinecontactrelatives.Lessoften',
                                        'onlinecontactrelatives.Never','onlinecontactrelatives.No.relatives',
                                        'onlinecontactrelatives.Severaltimesaweek','onlinecontactnonrelatives.Daily',
                                        'onlinecontactnonrelatives.Less.often','onlinecontactnonrelatives.Never',
                                        'onlinecontactnonrelatives.Nononrelatives','onlinecontactnonrelatives.Severaltimesaweek',
                                        'giveproducts','providepersonalcare','volunteering','receiveproducts','receivedpersonalcare',
                                        'hhsize_update_ca','partnerinhh_update_ca','satisfactionlife.High','satisfactionlife.Low',
                                        'satisfactionlife.Nodata','leftout.Never','leftout.Nodata','leftout.Often','leftout.Rarely',
                                        'lookforwardeachday.Never','lookforwardeachday.Nodata','lookforwardeachday.Often',
                                        'lookforwardeachday.Sometimes','lifemeaning.Never','lifemeaning.Nodata','lifemeaning.Often',
                                        'lifemeaning.Sometimes','backhapp.Never','backhapp.Nodata','backhapp.Often','backhapp.Sometimes',
                                        'fullenergy.Never','fullenergy.Nodata','fullenergy.Often','fullenergy.Sometimes','fulloport.Never',
                                        'fulloport.Nodata','fulloport.Often','fulloport.Sometimes','futuregood.Never','futuregood.Nodata',
                                        'futuregood.Often','futuregood.Sometimes','eversmokedaily','yearsmoked.Lessthan20years',
                                        'yearsmoked.Morethan40years','yearsmoked.Neversmoked','yearsmoked.Nodata','drinklastsevendays',
                                        'sixormoredrinks.Never','sixormoredrinks.Nodata','sixormoredrinks.Rarely','StringencyIndex',
                                        'GovernmentResponseIndex','EconomicSupportIndex','ContainmentHealthIndex','ConfirmedDeaths',
                                        'ConfirmedCases','age','numberillness.None','numberillness.One','numberillness.Two',
                                        'numdrugs','closepositive','otherpositive','country_wealth.Poor','country_wealth.Rich',
                                        'country_wealth.VeryRich','country_politics.Left','country_politics.Other',
                                        'country_politics.Right','country_region.East','country_region.North','country_region.South'),
                            listclass=c(""),grupos=4,sinicio=12345,repe=20,nodesize=30,replace=TRUE,ntree=500,mtry=35, sampsize = 3000)


mediasrf_nsize40<-cruzadarfbin(data=Share_train,
                            vardep='sadness',
                            listconti=c('gender','healthbcovid.Fair','healthbcovid.Good',
                                        'healthbcovid.Poor','healthacovid.Worsened','seriousillness',
                                        'diabetes','hypertension','heartdisease','otherdisease','falldown',
                                        'dizziness','fatigue','prescriptiondrug','cholesteroldrug.1','highbloodpreasuredrug',
                                        'coronarydiseasedrug','diabetesdrug','bronchitisdrug','lefthome','facemaskout.Always',
                                        'facemaskout.Never','facemaskout.Often','facemaskout.Sometimes','keepdistance.Always',
                                        'keepdistance.Often','keepdistance.Sometimes','handwash','handsanitizer','covercoughandsneeze',
                                        'drugagainstcovid','anxiety','anxietylevel.Lessorthesame','anxietylevel.Moreso',
                                        'troublesleeping','sleeptroublelevel.Lessorthesame','sleeptroublelevel.Moreso',
                                        'loneliness.Never','loneliness.Often','lonelinesslevel.Moreso','lonelinesslevel.Notlonely',
                                        'covidsymptom','positivetest','covidhospitalized','coviddeath','forgotreatment',
                                        'postponedappointment','treatedhospital','employed','lostjob.No','lostjob.Yes',
                                        'workplace.Noneofthese','workplace.Notworking','workplace.Usualworkplaceonly',
                                        'safeworkplace.Somewhatsafe','safeworkplace.Unsafe','safeworkplace.Verysafe',
                                        'reducedworkinghours.Notworking','reducedworkinghours.Yes','incomebeforecorona.High',
                                        'incomebeforecorona.Low','incomebeforecorona.Refusal','financialsupport',
                                        'lowestincomesincecorona.High','lowestincomesincecorona.Low','lowestincomesincecorona.Refusal',
                                        'makeendsmeet.Easily','makeendsmeet.Greatdifficulty','makeendsmeet.Somedifficulty',
                                        'dipsavings.No','dipsavings.Yes','personalcontactchildren.Daily','personalcontactchildren.Lessoften',
                                        'personalcontactchildren.Never','personalcontactchildren.Nochildren',
                                        'personalcontactchildren.Severaltimesaweek','personalcontactparent.Never',
                                        'personalcontactparent.Often','personalcontactparent.Sometimes','personalcontactfamily.Never',
                                        'personalcontactfamily.Often','personalcontactfamily.Sometimes',
                                        'personalcontactnonrelatives.Aboutonceaweek','personalcontactnonrelatives.Lessoften',
                                        'personalcontactnonrelatives.Never','personalcontactnonrelatives.Nononrelatives',
                                        'personalcontactnonrelatives.Severaltimesaweek','onlinecontactchildren.Aboutonceaweek',
                                        'onlinecontactchildren.Daily','onlinecontactchildren.Never','onlinecontactchildren.Nochildren',
                                        'onlinecontactchildren.Severaltimesaweek','onlinecontactparents.Daily','onlinecontactparents.Never',
                                        'onlinecontactparents.Noparents','onlinecontactrelatives.Daily','onlinecontactrelatives.Lessoften',
                                        'onlinecontactrelatives.Never','onlinecontactrelatives.No.relatives',
                                        'onlinecontactrelatives.Severaltimesaweek','onlinecontactnonrelatives.Daily',
                                        'onlinecontactnonrelatives.Less.often','onlinecontactnonrelatives.Never',
                                        'onlinecontactnonrelatives.Nononrelatives','onlinecontactnonrelatives.Severaltimesaweek',
                                        'giveproducts','providepersonalcare','volunteering','receiveproducts','receivedpersonalcare',
                                        'hhsize_update_ca','partnerinhh_update_ca','satisfactionlife.High','satisfactionlife.Low',
                                        'satisfactionlife.Nodata','leftout.Never','leftout.Nodata','leftout.Often','leftout.Rarely',
                                        'lookforwardeachday.Never','lookforwardeachday.Nodata','lookforwardeachday.Often',
                                        'lookforwardeachday.Sometimes','lifemeaning.Never','lifemeaning.Nodata','lifemeaning.Often',
                                        'lifemeaning.Sometimes','backhapp.Never','backhapp.Nodata','backhapp.Often','backhapp.Sometimes',
                                        'fullenergy.Never','fullenergy.Nodata','fullenergy.Often','fullenergy.Sometimes','fulloport.Never',
                                        'fulloport.Nodata','fulloport.Often','fulloport.Sometimes','futuregood.Never','futuregood.Nodata',
                                        'futuregood.Often','futuregood.Sometimes','eversmokedaily','yearsmoked.Lessthan20years',
                                        'yearsmoked.Morethan40years','yearsmoked.Neversmoked','yearsmoked.Nodata','drinklastsevendays',
                                        'sixormoredrinks.Never','sixormoredrinks.Nodata','sixormoredrinks.Rarely','StringencyIndex',
                                        'GovernmentResponseIndex','EconomicSupportIndex','ContainmentHealthIndex','ConfirmedDeaths',
                                        'ConfirmedCases','age','numberillness.None','numberillness.One','numberillness.Two',
                                        'numdrugs','closepositive','otherpositive','country_wealth.Poor','country_wealth.Rich',
                                        'country_wealth.VeryRich','country_politics.Left','country_politics.Other',
                                        'country_politics.Right','country_region.East','country_region.North','country_region.South'),
                            listclass=c(""),grupos=4,sinicio=12345,repe=20,nodesize=40,replace=TRUE,ntree=500,mtry=30, sampsize = 4000)



mediasrf_nsize60<-cruzadarfbin(data=Share_train,
                               vardep='sadness',
                               listconti=c('gender','healthbcovid.Fair','healthbcovid.Good',
                                           'healthbcovid.Poor','healthacovid.Worsened','seriousillness',
                                           'diabetes','hypertension','heartdisease','otherdisease','falldown',
                                           'dizziness','fatigue','prescriptiondrug','cholesteroldrug.1','highbloodpreasuredrug',
                                           'coronarydiseasedrug','diabetesdrug','bronchitisdrug','lefthome','facemaskout.Always',
                                           'facemaskout.Never','facemaskout.Often','facemaskout.Sometimes','keepdistance.Always',
                                           'keepdistance.Often','keepdistance.Sometimes','handwash','handsanitizer','covercoughandsneeze',
                                           'drugagainstcovid','anxiety','anxietylevel.Lessorthesame','anxietylevel.Moreso',
                                           'troublesleeping','sleeptroublelevel.Lessorthesame','sleeptroublelevel.Moreso',
                                           'loneliness.Never','loneliness.Often','lonelinesslevel.Moreso','lonelinesslevel.Notlonely',
                                           'covidsymptom','positivetest','covidhospitalized','coviddeath','forgotreatment',
                                           'postponedappointment','treatedhospital','employed','lostjob.No','lostjob.Yes',
                                           'workplace.Noneofthese','workplace.Notworking','workplace.Usualworkplaceonly',
                                           'safeworkplace.Somewhatsafe','safeworkplace.Unsafe','safeworkplace.Verysafe',
                                           'reducedworkinghours.Notworking','reducedworkinghours.Yes','incomebeforecorona.High',
                                           'incomebeforecorona.Low','incomebeforecorona.Refusal','financialsupport',
                                           'lowestincomesincecorona.High','lowestincomesincecorona.Low','lowestincomesincecorona.Refusal',
                                           'makeendsmeet.Easily','makeendsmeet.Greatdifficulty','makeendsmeet.Somedifficulty',
                                           'dipsavings.No','dipsavings.Yes','personalcontactchildren.Daily','personalcontactchildren.Lessoften',
                                           'personalcontactchildren.Never','personalcontactchildren.Nochildren',
                                           'personalcontactchildren.Severaltimesaweek','personalcontactparent.Never',
                                           'personalcontactparent.Often','personalcontactparent.Sometimes','personalcontactfamily.Never',
                                           'personalcontactfamily.Often','personalcontactfamily.Sometimes',
                                           'personalcontactnonrelatives.Aboutonceaweek','personalcontactnonrelatives.Lessoften',
                                           'personalcontactnonrelatives.Never','personalcontactnonrelatives.Nononrelatives',
                                           'personalcontactnonrelatives.Severaltimesaweek','onlinecontactchildren.Aboutonceaweek',
                                           'onlinecontactchildren.Daily','onlinecontactchildren.Never','onlinecontactchildren.Nochildren',
                                           'onlinecontactchildren.Severaltimesaweek','onlinecontactparents.Daily','onlinecontactparents.Never',
                                           'onlinecontactparents.Noparents','onlinecontactrelatives.Daily','onlinecontactrelatives.Lessoften',
                                           'onlinecontactrelatives.Never','onlinecontactrelatives.No.relatives',
                                           'onlinecontactrelatives.Severaltimesaweek','onlinecontactnonrelatives.Daily',
                                           'onlinecontactnonrelatives.Less.often','onlinecontactnonrelatives.Never',
                                           'onlinecontactnonrelatives.Nononrelatives','onlinecontactnonrelatives.Severaltimesaweek',
                                           'giveproducts','providepersonalcare','volunteering','receiveproducts','receivedpersonalcare',
                                           'hhsize_update_ca','partnerinhh_update_ca','satisfactionlife.High','satisfactionlife.Low',
                                           'satisfactionlife.Nodata','leftout.Never','leftout.Nodata','leftout.Often','leftout.Rarely',
                                           'lookforwardeachday.Never','lookforwardeachday.Nodata','lookforwardeachday.Often',
                                           'lookforwardeachday.Sometimes','lifemeaning.Never','lifemeaning.Nodata','lifemeaning.Often',
                                           'lifemeaning.Sometimes','backhapp.Never','backhapp.Nodata','backhapp.Often','backhapp.Sometimes',
                                           'fullenergy.Never','fullenergy.Nodata','fullenergy.Often','fullenergy.Sometimes','fulloport.Never',
                                           'fulloport.Nodata','fulloport.Often','fulloport.Sometimes','futuregood.Never','futuregood.Nodata',
                                           'futuregood.Often','futuregood.Sometimes','eversmokedaily','yearsmoked.Lessthan20years',
                                           'yearsmoked.Morethan40years','yearsmoked.Neversmoked','yearsmoked.Nodata','drinklastsevendays',
                                           'sixormoredrinks.Never','sixormoredrinks.Nodata','sixormoredrinks.Rarely','StringencyIndex',
                                           'GovernmentResponseIndex','EconomicSupportIndex','ContainmentHealthIndex','ConfirmedDeaths',
                                           'ConfirmedCases','age','numberillness.None','numberillness.One','numberillness.Two',
                                           'numdrugs','closepositive','otherpositive','country_wealth.Poor','country_wealth.Rich',
                                           'country_wealth.VeryRich','country_politics.Left','country_politics.Other',
                                           'country_politics.Right','country_region.East','country_region.North','country_region.South'),
                               listclass=c(""),grupos=4,sinicio=12345,repe=20,nodesize=60,replace=TRUE,ntree=500,mtry=35, sampsize = 4000)


#-------------------------------------------------------
mediasrf_3000$modelo  = "Rf 3000"
mediasrf_4000$modelo  = "Rf 4000"
mediasrf_nsize40$modelo  = "Rf nsize40"
mediasrf_nsize60$modelo  = "Rf nsize60"
#-------------------------------------------------------
unionrf<-rbind(mediasrf_3000, mediasrf_4000, mediasrf_nsize40, mediasrf_nsize60, medias_113)


# Los representamos
par(cex.axis=0.5)
library(viridis)
ggplot(unionrf, aes(x=modelo, y=auc, fill=modelo)) +
  scale_fill_manual(values = met.brewer("Hokusai1", 5))+
  geom_boxplot(outlier.colour="black", outlier.shape=1,
               outlier.size=2) +
  labs(x = "Modelos de Random Forest", y = 'AUC', title = "Boxplot vc repetida Random Forest")  



# ------------------- Ploteamos el arbol del modelo ganador -------------------

library(doParallel)
detectCores()
registerDoParallel(cores = 7) # set number of cores to use
library(doRNG)
registerDoRNG(seed = 123)


set.seed(12345)
best_rf_4000<- train(sadness~.,
                    data=Share_train,
                    method="rf",
                    tuneGrid=expand.grid(mtry=c(35)),
                    linout = FALSE,
                    ntree=1200,
                    sampsize=4000,
                    nodesize=30,
                    replace=TRUE,
                    importance=TRUE
)


other_rf_nsize40<- train(sadness~.,
                     data=Share_train,
                     method="rf",
                     tuneGrid=expand.grid(mtry=c(30)),
                     linout = FALSE,
                     ntree=500,
                     sampsize=4000,
                     nodesize=40,
                     replace=TRUE,
                     importance=TRUE
)




# ----------------- Vemos la importancia de las variables ---------------------

final<-best_rf_4000$finalModel
imp <- as.data.frame(final[["importance"]])
imp_data <- tibble::rownames_to_column(imp, "Variables") %>% arrange(desc(imp))
imp_data <- head(imp_data)

library(forcats)
imp_data %>%
  dplyr::mutate(Variables = fct_reorder(as.factor(Variables), MeanDecreaseGini)) %>%
  ggplot( aes(x=Variables, y=MeanDecreaseGini)) +
  labs(title = "Importancia de las variables Rf", y ="Importancia") +
  geom_bar(stat="identity", fill="#df7e66", width=.4)+
  coord_flip() 


# ----------------- Arbol con el mismo minbucket del rf ------------------------

set.seed(12345)
tree_rf <- rpart(sadness ~ ., 
                 data = Share_train,
                 minbucket =30,
                 method = "class",
                 parms=list(split="gini"),
                 maxsurrogate = 0)



rpart.plot(tree_rf,extra=105,nn=TRUE) 

# Importancia de las variables de este arbol

par(cex=1.2)
imp <- tree_rf$variable.importance
imp_data <- as.data.frame(imp)
imp_data <- tibble::rownames_to_column(imp_data, "Variables") %>% arrange(desc(imp))

library(forcats)
imp_data %>%
  mutate(Variables = fct_reorder(as.factor(Variables), imp)) %>%
  ggplot( aes(x=Variables, y=imp)) +
  labs(title = "Importancia de las variables árbol simple") +
  geom_bar(stat="identity", fill="#edc775", width=.4)+
  coord_flip()


# ---------------- Predicciones en el conjunto test ----------------------------

# Predicciones en Test
prediccion1 <- predict(best_rf_4000, newdata = Share_test, type = "prob")
prediccion1 <- as.data.frame(prediccion1) 
prediccion_yes1 <- prediccion1 %>% select(Yes)
# Valor del AUC
roc_curve <- roc(response=Share_test$sadness,predictor=prediccion1$Yes)
roc_curve[["auc"]]
# 0.8646

# Matriz de confusion
prediccion <- predict(best_rf_4000, newdata = Share_test, type = "raw")
conf_mat <- confusionMatrix(prediccion, Share_test[["sadness"]], positive = "Yes")
conf_mat <- as.data.frame(conf_mat[["table"]])
conf_mat
# 
# Prediction Reference Freq
# 1         No        No  745
# 2        Yes        No  291
# 3         No       Yes  260
# 4        Yes       Yes 1538


prediccion_yes1$model <- "Rf4000"

prediccion2 <- predict(other_rf_nsize40, newdata = Share_test, type = "prob")
prediccion2 <- as.data.frame(prediccion2) 
roc_curve <- roc(response=Share_test$sadness,predictor=prediccion2$Yes)
roc_curve[["auc"]]
prediccion_yes2 <- prediccion2 %>% select(Yes)
auc(Share_test$sadness, prediccion2$Yes)

prediccion_yes2$model <- "rfnsize40"

fresa3 <- cbind(Share_test[, c(1:175)], prediccion_yes1)
fresa4 <- cbind(Share_test[, c(1:175)], prediccion_yes2)

fresauni <- rbind(fresa,fresa2, fresa3, fresa4)
fresauni <- fresauni %>% select(sadness, Yes, model)


library(ggplot2)
library(plotROC)
library(Tableau)
ggplot(fresauni, aes(d = sadness, m = Yes, color = model)) + geom_roc(labels= FALSE, cutoffs.at = FALSE) +
  # coord_equal() + # cuadrada
  scale_color_manual(values = c("#e09351","#94b594", "#edc775", "#df7e66")) +
  # Límites de coordenadas y escalas de los ejes
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     expand = c(0.005, 0.005)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     expand = c(0.005, 0.005)) + 
  geom_abline(intercept = 0, slope = 1, lty = 4, lwd = 0.8)+
  theme(panel.grid.major = element_line(color = "gray90", size = 0.5),
        plot.title = element_text(size=20), 
        plot.background =
          element_rect(fill = "#fbf9f4", color = "#fbf9f4"))+
  labs(x = "1 - especificidad (proporción falsos positivos)",
       y = "sensibilidad (proporción verdaderos positivos)",
       color = "Modelos árboles",
       title = "COMPARATIVA MODELOS ARBOLES Y RANDOM FOREST",
       subtitle = glue("Métricas usadas: AUC, accuracy, sensib., especif."),
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                "Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)"))


