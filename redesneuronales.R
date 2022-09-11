#-------------------------------------METODOS DE SELECCION DE VARIABLES----------------------
# variable dependiente y

dependiente <- c("sadness")

y<-Share_train_std[,dependiente]
# variables input
nombres <- c('gender','healthbcovid.Fair','healthbcovid.Good',
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
             'country_politics.Right','country_region.East','country_region.North','country_region.South')
x<-Share_train_std[,nombres]
#.................................................. FILTROS
# SBF
library(caret)
# Realizamos el filtro con validación cruzada repetida, 3 grupos repetida 5 veces.
filtro<-sbf(x,y,sbfControl = sbfControl(functions = rfSBF,
                                        method = "repeatedcv", number=3, repeats=5, verbose = FALSE))
a<-dput(filtro$optVariables)
length(a)
#Escoge 72 variables
#
# "gender", "healthbcovid.Poor", "healthacovid.Worsened", "seriousillness", 
# "otherdisease", "falldown", "dizziness", "fatigue", "prescriptiondrug", 
# "highbloodpreasuredrug", "coronarydiseasedrug", "diabetesdrug", 
# "lefthome", "facemaskout.Always", "keepdistance.Always", "keepdistance.Often", 
# "keepdistance.Sometimes", "handwash", "handsanitizer", "covercoughandsneeze", 
# "anxiety", "anxietylevel.Lessorthesame", "anxietylevel.Moreso", 
# "troublesleeping", "sleeptroublelevel.Lessorthesame", "sleeptroublelevel.Moreso", 
# "loneliness.Often", "lonelinesslevel.Moreso", "covidsymptom", 
# "positivetest", "covidhospitalized", "coviddeath", "forgotreatment", 
# "postponedappointment", "treatedhospital", "employed", "lostjob.No", 
# "lostjob.Yes", "workplace.Noneofthese", "workplace.Notworking", 
# "safeworkplace.Somewhatsafe", "reducedworkinghours.Notworking", 
# "reducedworkinghours.Yes", "incomebeforecorona.High", "incomebeforecorona.Low", 
# "financialsupport", "lowestincomesincecorona.High", "lowestincomesincecorona.Low", 
# "makeendsmeet.Easily", "makeendsmeet.Somedifficulty", "dipsavings.No", 
# "personalcontactchildren.Daily", "personalcontactchildren.Lessoften", 
# "personalcontactchildren.Never", "personalcontactchildren.Nochildren", 
# "personalcontactparent.Never", "personalcontactparent.Sometimes", 
# "personalcontactfamily.Never", "personalcontactfamily.Often", 
# "personalcontactfamily.Sometimes", "personalcontactnonrelatives.Aboutonceaweek", 
# "personalcontactnonrelatives.Never", "personalcontactnonrelatives.Nononrelatives", 
# "onlinecontactchildren.Aboutonceaweek", "onlinecontactchildren.Daily", 
# "onlinecontactchildren.Never", "onlinecontactchildren.Nochildren", 
# "onlinecontactparents.Daily", "onlinecontactparents.Noparents", 
# "onlinecontactrelatives.Daily", "onlinecontactrelatives.Lessoften", 
# "onlinecontactrelatives.Never", "onlinecontactrelatives.No.relatives", 
# "onlinecontactrelatives.Severaltimesaweek", "onlinecontactnonrelatives.Daily", 
# "onlinecontactnonrelatives.Less.often", "onlinecontactnonrelatives.Never", 
# "onlinecontactnonrelatives.Nononrelatives", "onlinecontactnonrelatives.Severaltimesaweek", 
# "giveproducts", "volunteering", "receivedpersonalcare", "hhsize_update_ca", 
# "partnerinhh_update_ca", "satisfactionlife.High", "satisfactionlife.Low", 
# "leftout.Never", "leftout.Often", "lookforwardeachday.Never", 
# "lookforwardeachday.Often", "lifemeaning.Never", "lifemeaning.Often", 
# "backhapp.Never", "backhapp.Often", "backhapp.Sometimes", "fullenergy.Never", 
# "fullenergy.Often", "fullenergy.Sometimes", "fulloport.Never", 
# "fulloport.Often", "futuregood.Never", "futuregood.Often", "futuregood.Sometimes", 
# "drinklastsevendays", "StringencyIndex", "GovernmentResponseIndex", 
# "EconomicSupportIndex", "ContainmentHealthIndex", "ConfirmedDeaths", 
# "ConfirmedCases", "age", "numberillness.None", "numberillness.One", 
# "numdrugs", "closepositive", "otherpositive", "country_wealth.Poor", 
# "country_wealth.VeryRich", "country_politics.Other", "country_politics.Right", 
# "country_region.East", "country_region.South"




#..............................................WRAPPERS


# ***BORUTA ****

library(Boruta)
out.boruta <- Boruta(sadness~., data = Share_train_std)
# Vemos lo que sale del algoritmo
print(out.boruta)

# bathrooms and 14 more;
# No attributes deemed unimportant.
summary(out.boruta)
# Extraemos la decisión final
sal<-data.frame(out.boruta$finalDecision)
# De la selección final extraemos los confirmados (las variables con las que nos quedamos)
sal2<-sal[which(sal$out.boruta.finalDecision=="Confirmed"),,drop=FALSE]
# Nobres de las variables
dput(row.names(sal2))
# c("hhsize_update_ca", "StringencyIndex", "GovernmentResponseIndex", 
#   "EconomicSupportIndex", "ContainmentHealthIndex", "ConfirmedDeaths", 
#   "ConfirmedCases", "age", "numdrugs", "healthbcovid.Poor", "healthacovid.Worsened", 
#   "dizziness", "fatigue", "coronarydiseasedrug", "lefthome", "facemaskout.Always", 
#   "facemaskout.Never", "keepdistance.Always", "handwash", "handsanitizer", 
#   "anxiety", "anxietylevel.Lessorthesame", "anxietylevel.Moreso", 
#   "troublesleeping", "sleeptroublelevel.Lessorthesame", "sleeptroublelevel.Moreso", 
#   "loneliness.Never", "loneliness.Often", "lonelinesslevel.Moreso", 
#   "lonelinesslevel.Notlonely", "covidsymptom", "positivetest", 
#   "forgotreatment", "employed", "lostjob.Yes", "workplace.Notworking", 
#   "reducedworkinghours.Notworking", "incomebeforecorona.High", 
#   "incomebeforecorona.Low", "lowestincomesincecorona.High", "lowestincomesincecorona.Low", 
#   "personalcontactfamily.Never", "personalcontactnonrelatives.Never", 
#   "onlinecontactchildren.Daily", "onlinecontactrelatives.Severaltimesaweek", 
#   "receivedpersonalcare", "partnerinhh_update_ca", "satisfactionlife.High", 
#   "satisfactionlife.Low", "leftout.Never", "lookforwardeachday.Often", 
#   "lifemeaning.Nodata", "lifemeaning.Often", "backhapp.Often", 
#   "fullenergy.Never", "fullenergy.Often", "fulloport.Never", "fulloport.Often", 
#   "futuregood.Never", "futuregood.Often", "drinklastsevendays", 
#   "sixormoredrinks.Never", "otherpositive", "country_wealth.Poor", 
#   "country_wealth.Rich", "country_wealth.VeryRich", "country_politics.Left", 
#   "country_politics.Other", "country_politics.Right", "country_region.East", 
#   "country_region.North", "country_region.South")
length(dput(row.names(sal2)))





# -------------------TUNEO RED: PRIMER CONJUNTO VARIABLES----------------------------------------


# Variables
# c("anxietylevel.Moreso", "anxietylevel.Lessorthesame", "lonelinesslevel.Moreso" y "anxiety")

# Esta parte es para que todo vaya más rapido usando parallel computing
library(parallel)
library(doParallel)
GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing



library(caret)
# como Share_train tiene  11332 obs, y tenemos 4 variables, a 30 obs por parametro
# tendríasmo maximo 251 parametros.
# como par=h(k+1)+h+1,
# con k=45 tendriamos par: 45h+1=251, así que h=5 maximo.


# Por tanto podemos tunear el numero de nodos entre tener 30 obs por parámetro y 35:


# Fijjamos semilla
set.seed(1234)
completo<-data.frame(iter = integer(),
                     size    = integer(),
                     decay = double(),
                     auc         = double(),
                     sensitivity = double())


# Tuneo con Validación cruzada repetida con 3 grupos repetidos 3 veces con avNNet.
control<-trainControl(method = "repeatedcv",
                      number=3, repeats=3, savePredictions = "all",
                      classProbs = TRUE)

for (iter in c(50,100,500))
{
  for (dec in c(0.01,0.1,0.001, 0.2))
    {
    for (siz in c(30,40,50))
    {
      set.seed(12345)
      print(iter)
      print(dec)
      print(siz)
      #Definimos el grid de parámetros
      nnetgrid <- expand.grid(size=siz,decay=dec,bag=F)
      rednnet<- train(sadness~
                        anxietylevel.Moreso + anxietylevel.Lessorthesame 
                        + lonelinesslevel.Moreso + anxiety,
                      data=Share_train_std, 
                      method="avNNet",linout = TRUE,maxit=iter,
                      trControl=control,repeats=5,tuneGrid=nnetgrid,trace=TRUE)
      # Predicciones para  obtener métricas
      sal<-rednnet$pred
      # matriz de confusion
      salconfu<-confusionMatrix(sal$pred,sal$obs,positive = "Yes")
      # curva roc
      curvaroc<-roc(response=sal$obs,predictor=sal$Yes)
      # Estadisticos
      auc<-curvaroc$auc
      auc<-round(curvaroc$auc,4)
      accuracy <- round(salconfu[["overall"]][["Accuracy"]],4)
      sensitivity  <- round(salconfu[["byClass"]][["Sensitivity"]],4)
      #Insertamos en Dataframe
      completo[nrow(completo) + 1,] <- c(iter, siz,dec,auc,accuracy, sensitivity)
    }
  }
}

completop<-completo[order(completo$auc),]
# Graficamos el w
ggplot(completop, aes(x=factor(iter), y=auc,
                     color=factor(decay),pch=factor(size))) +
  geom_point(position=position_dodge(width=0.5),size=3)






# -------------------TUNEO RED: SEGUNDO CONJUNTO VARIABLES----------------------------------------

# # c(anxietylevel.Moreso
# anxietylevel.Lessorthesame
# lonelinesslevel.Moreso
# anxiety
# sleeptroublelevel.Moreso
# sleeptroublelevel.Lessorthesame
# loneliness.Never
# Confirmeddeaths
# )
# # Esta parte es para que todo vaya más rapido usando parallel computing
library(parallel)
library(doParallel)
GS_T0 <- Sys.time()
cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
registerDoParallel(cluster) # register the parallel processing
library(caret)
# Tuneo con Validación cruzada repetida con 5 grupos repetidos 6 veces con avNNet.
control2<-trainControl(method = "repeatedcv",
                       number=3, repeats=3, savePredictions = "all")



# como Share tiene 11.332 obs, y tenemos 8 variables, a 30 obs por parametro
# tendríasmo maximo 251 parametros.

# como par=h(k+1)+h+1,
# con k=8 tendriamos par:10h+1=25, así que h=25 maximo.



# Fijjamos semilla
set.seed(1234)
completobic<-data.frame(iter = integer(),
                     size    = integer(),
                     decay = double(),
                     auc         = double(),
                     sensitivity = double())


# Tuneo con Validación cruzada repetida con 3 grupos repetidos 3 veces con avNNet.
control2<-trainControl(method = "repeatedcv",
                      number=3, repeats=3, savePredictions = "all",
                      classProbs = TRUE)

for (iter in c(10,20,50,500))
{
  for (dec in c(0.01,0.1,0.001, 0.2))
  {
    for (siz in c(10,15,25))
    {
      set.seed(12345)
      print(iter)
      print(dec)
      print(siz)
      #Definimos el grid de parámetros
      nnetgrid2 <- expand.grid(size=siz,decay=dec,bag=F)
      rednnet2<- train(sadness~ anxietylevel.Moreso +
                       anxietylevel.Lessorthesame+
                       lonelinesslevel.Moreso+
                       anxiety+
                       sleeptroublelevel.Moreso+
                       sleeptroublelevel.Lessorthesame+
                       loneliness.Never+
                       ConfirmedDeaths
                       ,
                       data=Share_train_std,
                       method="avNNet",linout = TRUE,maxit=iter,
                       trControl=control2,repeats=5,tuneGrid=nnetgrid2,trace=TRUE)
      # Predicciones para  obtener métricas
      sal2<-rednnet2$pred
      # matriz de confusion
      salconfu2<-confusionMatrix(sal2$pred,sal2$obs,positive = "Yes")
      # curva roc
      curvaroc2<-roc(response=sal2$obs,predictor=sal2$Yes)
      # Estadisticos
      auc<-curvaroc2$auc
      auc<-round(curvaroc2$auc,4)
      accuracy<- round(salconfu2[["overall"]][["Accuracy"]],4)
      sensitivity  <- round(salconfu2[["byClass"]][["Sensitivity"]],4)
      #Insertamos en Dataframe
      completobic[nrow(completobic) + 1,] <- c(iter, siz,dec,auc,accuracy, sensitivity)
    }
  }
}

completobic<-completobic[order(completobic$auc),]
# Graficamos el w
ggplot(completobic, aes(x=factor(iter), y=auc,
                     color=factor(decay),pch=factor(size))) +
  geom_point(position=position_dodge(width=0.5),size=3)




#-----------------------------------REDES VS REGRESION--------------------------------------
# Añadimos la caja de la red del primer conjunto
mediasredfinal1<-cruzadaavnnetbin(data=data,
                        vardep="sadness",listconti=
                          c(c("anxietylevel.Moreso", "anxietylevel.Lessorthesame", "lonelinesslevel.Moreso", "anxiety")),
                        listclass=c(""),grupos=10,sinicio=12345,repe=10,repeticiones=1,itera=20,
                        size=c(8),decay=c(0.01))
mediasredfinal1$modelo="Red1"


# Añadimos la caja de la red del segundo conjunto
mediaredfinal24<-cruzadaavnnetbin(data=data,
                        vardep="sadness",listconti=
                          c("anxietylevel.Moreso", 
                            "anxietylevel.Lessorthesame",
                            "lonelinesslevel.Moreso",
                            "anxiety",
                            "sleeptroublelevel.Moreso",
                            "sleeptroublelevel.Lessorthesame",
                            "loneliness.Never",
                            "ConfirmedDeaths"),
                        listclass=c(""),grupos=10,sinicio=12345,repe=10,repeticiones=1,itera=50,
                        size=c(10),decay=c(0.01))
mediaredfinal24$modelo="Redconjunto2"


unionfinal<-rbind(mediaredfinal24, mediasredfinal1)
library(viridis)
ggplot(unionfinal, aes(x=modelo, y=auc, fill=modelo)) +
  scale_fill_manual(values = met.brewer("Hokusai1", 4))+
  geom_boxplot(outlier.colour="black", outlier.shape=1,
               outlier.size=2) +
  labs(x = "Modelos de Redes", y = 'AUC', title = "Boxplot vc repetida Redes") 


nnetgrid2 <- expand.grid(size=10,decay=0.01,bag=F)
redfinal<- train(sadness~ anxietylevel.Moreso +
                   anxietylevel.Lessorthesame+
                   lonelinesslevel.Moreso+
                   anxiety+
                   sleeptroublelevel.Moreso+
                   sleeptroublelevel.Lessorthesame+
                   loneliness.Never+
                   ConfirmedDeaths,
                 data=Share_train_std,
                 method="avNNet",linout = TRUE,maxit=50,tunegrid=nnetgrid2,trace=TRUE)


# Predicciones en Test
prediccionaic <- predict(redfinal, newdata = Share_test_std, type = "prob") %>%  mutate(odds = Yes / No, log.odds = log(odds))
prediccionaic

prediccionaic <- as.data.frame(prediccionaic) 
prediccion_yesaic <- prediccionaic %>% dplyr::select(Yes)
# Valor del AUC
roc_curve <- roc(response=Share_test_std$sadness,predictor=prediccionaic$Yes)
roc_curve[["auc"]]
# 0.5843

# Matriz de confusion
prediccionaic2 <- predict(modelo, newdata = Share_test_std, type = "raw")

conf_mataic <- confusionMatrix(prediccionaic2, Share_test_std[["sadness"]], positive = "Yes")
conf_mataic <- as.data.frame(conf_mataic[["table"]])
conf_mataic
# Prediction Reference Freq
# 1         No        No  763
# 2        Yes        No  273
# 3         No       Yes  271
# 4        Yes       Yes 1527


modeloaic <- cbind(Share_test_std[, c(1:175)], prediccion_yesaic)


library(ggplot2)
library(plotROC)
library(Tableau)
ggplot(modeloaic, aes(d = sadness, m = Yes)) + geom_roc(labels= FALSE, cutoffs.at = FALSE) +
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
       color = "Modelos redes",
       title = "CURVA ROC REDES",
       subtitle = glue("Métricas usadas: AUC, accuracy, sensib., especif."),
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                "Datos: Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)"))

