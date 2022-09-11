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
library(dplyr)
source("cruzada arbolbin.R")



# Importamos los conjuntos de datos necesarios
Share_train <- read.csv("./Share_train.csv")
Share_test <- read.csv("./Share_test.csv")


# Cambiamos
Share_train$sadness<-ifelse(Share_train$sadness==1,"Yes","No")
Share_train$sadness <- as.factor(Share_train$sadness)

Share_test$sadness<-ifelse(Share_test$sadness==1,"Yes","No")
Share_test$sadness <- as.factor(Share_test$sadness)


continuas <- c("hhsize_update_ca", "StringencyIndex", "GovernmentResponseIndex",
               "EconomicSupportIndex", "ContainmentHealthIndex", "ConfirmedDeaths",
               "ConfirmedCases", "age", "numdrugs")

# Estandarizamos las variables continuas (tan solo para los metodos de regularizacion),
# para ello primer calulamos la media y desviación típica
# de cada variable continua
# Medida de cada variable continua
means <- apply(Share_train[,continuas],2,mean,na.rm=TRUE)
#Desviacion tipica de cada variable continua
sds <-sapply(Share_train[,continuas],sd,na.rm=TRUE)
# ESTANDARIZO: RESTO LA MEDIA Y DIVIDO POR DESVIACION TIPICA
Share_train_std <-scale(Share_train[,continuas], center = means, scale = sds)
# Uno las variables estandarizadas a las variables categoricas y la variable respuesta
ss <- Share_train %>% dplyr::select(-c("hhsize_update_ca", "StringencyIndex", "GovernmentResponseIndex",
                                "EconomicSupportIndex", "ContainmentHealthIndex", "ConfirmedDeaths",
                                "ConfirmedCases", "age", "numdrugs"))
Share_train_std <- cbind(Share_train_std, ss)

# Estandarizamos las variables continuas, para ello primer calulamos la media y desviación típica
# de cada variable continua
# Medida de cada variable continua
means <- apply(Share_test[,continuas],2,mean,na.rm=TRUE)
#Desviacion tipica de cada variable continua
sds <-sapply(Share_test[,continuas],sd,na.rm=TRUE)
# ESTANDARIZO: RESTO LA MEDIA Y DIVIDO POR DESVIACION TIPICA
Share_test_std <-scale(Share_test[,continuas], center = means, scale = sds)
# Uno las variables estandarizadas a las variables categoricas y la variable respuesta
sst <- Share_test %>% dplyr::select(-c("hhsize_update_ca", "StringencyIndex", "GovernmentResponseIndex",
                                "EconomicSupportIndex", "ContainmentHealthIndex", "ConfirmedDeaths",
                                "ConfirmedCases", "age", "numdrugs"))
Share_test_std <- cbind(Share_test_std, sst)

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
  axis.text = element_text(size = 10, color = "black"),
  axis.title.y = element_text(size = 13, angle = 90,  family = "Roboto Mono"),
  axis.title.x = element_text(size = 13,  family = "Roboto Mono"),
  axis.line = element_line(colour = "grey50"),
  # Fondo
  plot.background =
    element_rect(fill = "white", color = "#fbf9f4"))



# *****************************************************************************
#                       SELECCIONAMOS VARIABLES 
# *****************************************************************************



# ****************************** STEP AIC Y BIC *******************************


library(MASS)
#AIC
full <- glm(sadness~., data=Share_train, family = binomial(link = "logit"))
null <- glm(sadness~1, data=Share_train, family = binomial(link = "logit"))

#Partimos de una lista vacia y vamos metiendo y sacando
selec_AIC <- stepAIC(null, scope=list(upper=full), direction='both', trace=TRUE)
#saveRDS(selec_AIC, file = "selec_AIC.rds")
selec_AIC <- readRDS("selec_AIC.rds")

summary(selec_AIC)
vec <- (names(selec_AIC[[1]]))
length(vec)
#46 variables
dput(vec)


# c("(Intercept)", "anxietylevel.Moreso", "lonelinesslevel.Moreso", 
#   "sleeptroublelevel.Lessorthesame", "anxiety", "loneliness.Never", 
#   "troublesleeping", "handsanitizer", "partnerinhh_update_ca", 
#   "healthacovid.Worsened", "country_politics.Right", "country_politics.Left", 
#   "fatigue", "onlinecontactchildren.Daily", "satisfactionlife.High", 
#   "leftout.Nodata", "coviddeath", "onlinecontactrelatives.Severaltimesaweek", 
#   "personalcontactchildren.Lessoften", "personalcontactfamily.Never", 
#   "coronarydiseasedrug", "EconomicSupportIndex", "country_wealth.Rich", 
#   "forgotreatment", "lostjob.Yes", "onlinecontactchildren.Severaltimesaweek", 
#   "leftout.Never", "otherpositive", "dipsavings.No", "fullenergy.Never", 
#   "country_region.South", "diabetesdrug", "country_politics.Other", 
#   "facemaskout.Sometimes", "onlinecontactnonrelatives.Daily", "onlinecontactparents.Noparents", 
#   "personalcontactnonrelatives.Aboutonceaweek", "receiveproducts", 
#   "reducedworkinghours.Yes", "sixormoredrinks.Rarely", "backhapp.Never", 
#   "keepdistance.Sometimes", "facemaskout.Never", "seriousillness", 
#   "gender", "dizziness")


#BIC
#Partimos de una lista vacia y vamos metiendo y sacando
selec_BIC <- stepAIC(null, scope=list(upper=full), direction='both', trace=TRUE, k=log(nrow(Share_train)))
#saveRDS(selec_BIC, file = "selec_BIC.rds")
selec_BIC <- readRDS("selec_BIC.rds")

summary(selec_BIC)
vec <- (names(selec_BIC[[1]]))
length(vec)
#20 variables
dput(vec)

# 
# c("(Intercept)", "anxietylevel.Moreso", "lonelinesslevel.Moreso", 
#   "sleeptroublelevel.Lessorthesame", "anxiety", "loneliness.Never", 
#   "troublesleeping", "country_region.East", "handsanitizer", "partnerinhh_update_ca", 
#   "healthacovid.Worsened", "country_politics.Right", "country_politics.Left", 
#   "fatigue", "onlinecontactchildren.Daily", "satisfactionlife.High", 
#   "leftout.Nodata", "coviddeath", "onlinecontactrelatives.Severaltimesaweek", 
#   "personalcontactchildren.Lessoften")



# *********************COMPARACION VIA CV REPETIDA Y BOXPLOT


source("./cruzadas avnnet y log binaria.R")

# MODELO STEP AIC
mediasaic<-cruzadalogistica(data=Share_train,
                    vardep="sadness",listconti=
                      c("anxietylevel.Moreso", "lonelinesslevel.Moreso", 
                          "sleeptroublelevel.Lessorthesame", "anxiety", "loneliness.Never", 
                           "troublesleeping", "handsanitizer", "partnerinhh_update_ca", 
                           "healthacovid.Worsened", "country_politics.Right", "country_politics.Left", 
                          "fatigue", "onlinecontactchildren.Daily", "satisfactionlife.High", 
                           "leftout.Nodata", "coviddeath", "onlinecontactrelatives.Severaltimesaweek", 
                           "personalcontactchildren.Lessoften", "personalcontactfamily.Never", 
                           "coronarydiseasedrug", "EconomicSupportIndex", "country_wealth.Rich", 
                           "forgotreatment", "lostjob.Yes", "onlinecontactchildren.Severaltimesaweek", 
                           "leftout.Never", "otherpositive", "dipsavings.No", "fullenergy.Never", 
                           "country_region.South", "diabetesdrug", "country_politics.Other", 
                           "facemaskout.Sometimes", "onlinecontactnonrelatives.Daily", "onlinecontactparents.Noparents", 
                           "personalcontactnonrelatives.Aboutonceaweek", "receiveproducts", 
                            "reducedworkinghours.Yes", "sixormoredrinks.Rarely", "backhapp.Never", 
                           "keepdistance.Sometimes", "facemaskout.Never", "seriousillness", 
                           "gender", "dizziness"),
                    listclass=c(""),grupos=10,sinicio=12345,repe=50)
mediasaic$modelo="STEPAIC"

# MODELO STEP BIC
mediasBIC<-cruzadalogistica(data=Share_train,
                    vardep="sadness",listconti= c("anxietylevel.Moreso", "lonelinesslevel.Moreso", 
                                                 "sleeptroublelevel.Lessorthesame", "anxiety", "loneliness.Never", 
                                                   "troublesleeping", "country_region.East", "handsanitizer", "partnerinhh_update_ca", 
                                                   "healthacovid.Worsened", "country_politics.Right", "country_politics.Left", 
                                                   "fatigue", "onlinecontactchildren.Daily", "satisfactionlife.High", 
                                                   "leftout.Nodata", "coviddeath", "onlinecontactrelatives.Severaltimesaweek", 
                                                   "personalcontactchildren.Lessoften"),
                    listclass=c(""),grupos=10,sinicio=12345,repe=50)
mediasBIC$modelo="STEPBIC"


# REALIZAMOS EL BOXPLOT
unionregresion<-rbind(mediasaic, mediasBIC)
par(cex.axis=0.5)
library(viridis)
ggplot(unionregresion, aes(x=modelo, y=auc, fill=modelo)) +
  scale_fill_manual(values = met.brewer("Hokusai1", 4))+
  geom_boxplot(outlier.colour="black", outlier.shape=1,
               outlier.size=2) +
  labs(x = "Modelos de Regresión", y = 'AUC', title = "Boxplot vc repetida Regresión")  




# ---------------- Predicciones en el conjunto test ----------------------------


set.seed(12345)
# Tuneo con Validación cruzada repetida con 5 grupos repetidos 6 veces con avNNet.
controlreg<-trainControl(method = "repeatedcv",
                         number=4, repeats=3, savePredictions = "all")
modelo <- train(sadness~
                  anxietylevel.Moreso+lonelinesslevel.Moreso+ 
                  sleeptroublelevel.Lessorthesame + anxiety + loneliness.Never+ 
                  troublesleeping + country_region.East + handsanitizer + partnerinhh_update_ca + 
                  healthacovid.Worsened + country_politics.Right + country_politics.Left + 
                  fatigue + onlinecontactchildren.Daily + satisfactionlife.High +
                  leftout.Nodata + coviddeath + onlinecontactrelatives.Severaltimesaweek + 
                  personalcontactchildren.Lessoften, data=Share_train,
                method="glm", trControl = controlreg)
summary(modelo)

# Predicciones en Test
prediccionaic <- predict(modelo, newdata = Share_test_std, type = "prob") %>%  mutate(odds = Yes / No, log.odds = log(odds))
prediccionaic

prediccionaic <- as.data.frame(prediccionaic) 
prediccion_yesaic <- prediccionaic %>% select(Yes)
# Valor del AUC
roc_curve <- roc(response=Share_test_std$sadness,predictor=prediccionaic$Yes)
roc_curve[["auc"]]
# 0.8663

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
       color = "Modelos árboles",
       title = "CURVA ROC REGRESIÓN LOGÍSTICA",
       subtitle = glue("Métricas usadas: AUC, accuracy, sensib., especif."),
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                "Datos: Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)"))


# ************ REGULARIZACION **************************************************


set.seed(0)  
d=mlbench::mlbench.2dnormals(100, 2, r=1)

x = d$x
y = ifelse(d$classes==1, 1, 0)

logistic_loss <- function(w){
  p    = plogis(x %*% w)
  L    = -y*log(p) - (1-y)*log(1-p)
  LwR2 = sum(L) + lambda*t(w) %*% w
  return(c(LwR2))
}

logistic_loss_gr <- function(w){
  p = plogis(x %*% w)
  v = t(x) %*% (p - y)
  return(c(v) + 2*lambda*w)
}

w_grid_v = seq(-10, 10, 0.1)
w_grid   = expand.grid(w_grid_v, w_grid_v)

lambda = 0
opt1   = optimx::optimx(c(1,1), fn=logistic_loss, gr=logistic_loss_gr, method="BFGS")
z1     = matrix(apply(w_grid,1,logistic_loss), ncol=length(w_grid_v))

lambda = 5
opt2   = optimx::optimx(c(1,1), fn=logistic_loss, method="BFGS")
z2     = matrix(apply(w_grid,1,logistic_loss), ncol=length(w_grid_v))

plot(d, xlim=c(-3,3), ylim=c(-3,3))
abline(0, -opt1$p2/opt1$p1, col='blue',  lwd=2)
abline(0, -opt2$p2/opt2$p1, col='black', lwd=2)
contour(w_grid_v, w_grid_v, z1, col='blue',  lwd=2, nlevels=8)
contour(w_grid_v, w_grid_v, z2, col='black', lwd=2, nlevels=8, add=T)
points(opt1$p1, opt1$p2, col='blue',  pch=19)
points(opt2$p1, opt2$p2, col='black', pch=19)


library(glmnet)

# *************** Metodo Ridge  *************************************************


x <- as.matrix(Share_train_std[, -44])
y <- Share_train_std$sadness



# Ajustamos los valores

fit.ridge <- glmnet(x, y, family= "binomial", type.measure="class", alpha = 0, nlambda=100)
plot(fit.ridge, xvar = "Lambda", label = TRUE)

# Evolución de los coeficientes en función de lambda
# ==============================================================================
regularizacion <- fit.ridge$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = fit.ridge$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme(legend.position = "none")


# validacion cruzada
set.seed(1)
cv.ridge <- cv.glmnet(x, y, family= "binomial", type.measure="auc", alpha = 0)
plot(cv.ridge)

# Mejor valor lambda encontrado
# ==============================================================================
paste("Mejor valor de lambda encontrado:", cv.ridge$lambda.min)
# [1] "Mejor valor de lambda encontrado: 0.0866563820608222"


# Mejor valor lambda encontrado + 1sd
# ==============================================================================
# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv.ridge$lambda.1se)
# [1] "Mejor valor de lambda encontrado + 1 desviación estándar: 0.166199164393863"


# Mejor modelo lambda óptimo + 1sd
# ==============================================================================
modelo <- glmnet(
  x           = x,
  y           = y,
  alpha       = 0,
  family= "binomial", 
  type.measure="class",
  lambda      = cv.ridge$lambda.1se,
  standardize = TRUE
)



# Coeficientes del modelo
# ==============================================================================
df_coeficientes <- coef(modelo) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo Ridge") +
  theme(axis.text.x = element_text(size = 6, angle = 45))

# AUC PARA TODAS LAS VARIABLES

#predicting with the training set
SRL_pred_train <- predict(modelo, x, type="response", s=cv.ridge$lambda.1se)
pred_stdtrain <- prediction(SRL_pred_train, y)
perf_stdtrain <- performance(pred_stdtrain, measure = "tpr", x.measure = "fpr")

#true positive rate
tpr.points1 <- attr(perf_stdtrain, "y.values")[[1]]
#tpr.points

#false positive rate
fpr.points1 <- attr(perf_stdtrain,"x.values")[[1]]
#fpr.points

#area under the curve
auc1 <- attr(performance(pred_stdtrain, "auc"), "y.values")[[1]]
formatted_auc1 <- signif(auc1, digits=3)
formatted_auc1

#report mean error rate (fraction of incorrect labels)
confusion_matrix_train <- table(y, SRL_pred_train)
confusion_matrix_train

x_test <- as.matrix(Share_test_std[, -44])
y_test <- Share_test_std$sadness


# ****************** Conjunto test ******************************************
library(ROCR)
library(ggplot2)
#ROC curve for standardized data
prob_std <- predict(modelo, x_test, type="class", s=cv.ridge$lambda.1se)
pred_std <- prediction(prob_std, y_test)
perf_std <- performance(pred_std, measure = "tpr", x.measure = "fpr")

#report mean error rate (fraction of incorrect labels)
confusion_matrix_train <- table(y_test, prob_std)
confusion_matrix_train

#true positive rate
tpr.points1 <- attr(perf_std, "y.values")[[1]]
#tpr.points

#false positive rate
fpr.points1 <- attr(perf_std,"x.values")[[1]]
#fpr.points

#area under the curve
auc1 <- attr(performance(pred_std, "auc"), "y.values")[[1]]
formatted_auc1 <- signif(auc1, digits=3)


roc.data1 <- data.frame(fpr=fpr.points1, tpr=tpr.points1, model="GLM")


ggplot(roc.data1, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_line(aes(y=tpr)) +
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
       subtitle = glue("Métricas usadas: AUC, accuracy, sensib., especif."),
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                "Datos: Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)")) +
  ggtitle(paste0("ROC Curve Logistic Regression Ridge w/ AUC=", formatted_auc1)) 



# *************** Metodo Lasso  *************************************************


x <- as.matrix(Share_train_std[, -44])
y <- Share_train_std$sadness



# Creación y entrenamiento del modelo
# ==============================================================================
# Para obtener un ajuste con regularización Lasso se indica argumento alpha=1.
# Si no se especifica valor de lambda, se selecciona un rango automático.
modelo <- glmnet(
  x           = x,
  y           = y,
  alpha       = 1,
  family= "binomial", 
  type.measure="auc",
  nlambda     = 50,
  standardize = TRUE
)


# Evolución de los coeficientes en función de lambda
# ==============================================================================
regularizacion <- modelo$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )
library(scales)
regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme(legend.position = "none")


# Evolución del error en función de lambda
# ==============================================================================
set.seed(123)
cv_error <- cv.glmnet(
  x      = x,
  y      = y,
  family= "binomial", 
  type.measure="auc",
  alpha  = 1,
  nfolds = 10,
  standardize  = TRUE
)

plot(cv_error)

# Mejor valor lambda encontrado
# ==============================================================================
paste("Mejor valor de lambda encontrado:", cv_error$lambda.min)
# [1] "Mejor valor de lambda encontrado: 0.00952673421871858"


# Mejor valor lambda encontrado + 1sd
# ==============================================================================
# Mayor valor de lambda con el que el test-error no se aleja más de 1sd del mínimo.
paste("Mejor valor de lambda encontrado + 1 desviación estándar:", cv_error$lambda.1se)
# [1] "Mejor valor de lambda encontrado + 1 desviación estándar: 0.0109347952165059"

# Mejor modelo lambda óptimo + 1sd
# ==============================================================================
modelo <- glmnet(
  x           = x,
  y           = y,
  alpha       = 1,
  family= "binomial", 
  type.measure="auc",
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)


# Coeficientes del modelo
# ==============================================================================
coef(modelo) # tiene menos variables explicativas

df_coeficientes <- coef(modelo) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(
    predictor != "(Intercept)",
    coeficiente != 0
  ) 


# # A tibble: 24 x 2
# predictor                       coeficiente
# <chr>                                 <dbl>
#   1 EconomicSupportIndex                0.0125 
# 2 numdrugs                           -0.00848
# 3 healthbcovid.Poor                  -0.267  
# 4 healthacovid.Worsened               0.282  
# 5 keepdistance.Always                 0.0827 
# 6 keepdistance.Sometimes             -0.128  
# 7 handwash                            0.0327 
# 8 anxietylevel.Lessorthesame         -0.955  
# 9 anxietylevel.Moreso                 1.60   
# 10 sleeptroublelevel.Lessorthesame    -0.310  
# # ... with 14 more rows



# AUC PARA TODAS LAS VARIABLES

#predicting with the training set
SRL_pred_train <- predict(modelo, x, type="response", s=cv.ridge$lambda.1se)
pred_stdtrain <- prediction(SRL_pred_train, y)
perf_stdtrain <- performance(pred_stdtrain, measure = "tpr", x.measure = "fpr")

#area under the curve
auc1 <- attr(performance(pred_stdtrain, "auc"), "y.values")[[1]]
formatted_auc1 <- signif(auc1, digits=3)

#report mean error rate (fraction of incorrect labels)
confusion_matrix_train <- table(y, SRL_pred_train)
confusion_matrix_train

x_test <- as.matrix(Share_test_std[, -44])
y_test <- Share_test_std$sadness
# ****************** Conjunto test ******************************************
library(ROCR)
library(ggplot2)
#ROC curve for standardized data
prob_std <- predict(modelo, x_test, type="response", s=cv.ridge$lambda.1se)
pred_std <- prediction(prob_std, y_test)
perf_std <- performance(pred_std, measure = "tpr", x.measure = "fpr")

prob_std <- predict(SRL_pred_train, x, type="class", s=cv.ridge$lambda.1se)
#report mean error rate (fraction of incorrect labels)
confusion_matrix_train <- table(y, SRL_pred_train)
confusion_matrix_train


#true positive rate
tpr.points1 <- attr(perf_std, "y.values")[[1]]
#tpr.points

#false positive rate
fpr.points1 <- attr(perf_std,"x.values")[[1]]
#fpr.points

#area under the curve
auc1 <- attr(performance(pred_std, "auc"), "y.values")[[1]]
formatted_auc1 <- signif(auc1, digits=3)


roc.data1 <- data.frame(fpr=fpr.points1, tpr=tpr.points1, model="GLM")


ggplot(roc.data1, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_line(aes(y=tpr)) +
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
       subtitle = glue("Métricas usadas: AUC, accuracy, sensib., especif."),
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                "Datos: Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)")) +
  ggtitle(paste0("ROC Curve Logistic Regression Lasso w/ AUC=", formatted_auc1)) 


# ****************** ELASTIC NET REGRESSION ************************************

x <- as.matrix(Share_train_std[, -44])
y <- Share_train_std$sadness


set.seed(1)
# Se podría emplear train(fidelida ~ ., data = train, ...)
caret.glmnet <- train(x, y, method = "glmnet",
                      preProc = c("zv", "center", "scale"),
                      trControl = trainControl(method = "cv", number = 5),
                      tuneLength = 5)


caret.glmnet

ggplot(caret.glmnet, highlight = TRUE) +labs(title = "AUC para los diferentes valores parámetros", 
                                             y = "AUC (Cross-Validation)")
  

caret.glmnet <- glmnet(
  x           = x,
  y           = y,
  alpha       = 0.775,
  family= "binomial", 
  type.measure="class",
  lambda      = 0.001015096,
  standardize = TRUE
)


df_coeficientes <- coef(caret.glmnet) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(
    predictor != "(Intercept)",
    coeficiente != 0
  ) 


# A tibble: 131 x 2
# predictor             coeficiente
# <chr>                       <dbl>
#   1 StringencyIndex            0.0229
# 2 EconomicSupportIndex       0.0665
# 3 ConfirmedDeaths            0.0362
# 4 age                        0.0268
# 5 gender                    -0.0645
# 6 healthbcovid.Fair         -0.0783
# 7 healthbcovid.Good         -0.0652
# 8 healthbcovid.Poor         -0.102 
# 9 healthacovid.Worsened      0.422 
# 10 seriousillness             0.195 
# # ... with 121 more rows

#  **************** Predicciones

x_test <- as.matrix(Share_test_std[, -44])
y_test <- Share_test_std$sadness
#ROC curve for standardized data
prob_std <- predict(caret.glmnet, x, type="response")
pred_std <- prediction(prob_std, y)
perf_std <- performance(pred_std, measure = "tpr", x.measure = "fpr")



prob_std <- predict(caret.glmnet, x, type="class")
#report mean error rate (fraction of incorrect labels)
confusion_matrix_train <- table(y, prob_std)
confusion_matrix_train

prob_std <- predict(caret.glmnet, x_test, type="class")
#report mean error rate (fraction of incorrect labels)
confusion_matrix_train <- table(y_test, prob_std)
confusion_matrix_train

#true positive rate
tpr.points1 <- attr(perf_std, "y.values")[[1]]
#tpr.points

#false positive rate
fpr.points1 <- attr(perf_std,"x.values")[[1]]
#fpr.points

#area under the curve
auc1 <- attr(performance(pred_std, "auc"), "y.values")[[1]]
formatted_auc1 <- signif(auc1, digits=3)


roc.data1 <- data.frame(fpr=fpr.points1, tpr=tpr.points1, model="GLM")


ggplot(roc.data1, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_line(aes(y=tpr)) +
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
       subtitle = glue("Métricas usadas: AUC, accuracy, sensib., especif."),
       caption =
         paste0("Autora: Lucía Pascual Martínez | ",
                "Datos: Datos: SHARE (Survey of Health, Ageing and Retirement in Europe)")) +
  ggtitle(paste0("ROC Curve Logistic Regression Elastic Net w/ AUC=", formatted_auc1)) 



