---
title: "TFG"
author: "Jonathan Soto Botina"
date: "2023-03-28"
output: html_document
---
Este es el documento de R para el TFG. 


PARA HACER UNA MATRIZ DE CORRELACOINES DE VARIABLES ORDINALES LO PUEDO HACER CON RADIANT
O CON LA LIBRERIA PSYCH LA FUNCION 
```{r}
library(haven)
library(psych)
library(corrplot)
library(polycor)
library(ggplot2)
datosnum <- read_sav("dermatology.sav")

#correlacion de pearson entre las histopatologicas (12-33)

histopat<- datosnum[12:33]
Rh<- cor(histopat)
cor.plot(histopat, numbers=T, upper=FALSE, main = "Pearson Correlation (H)", show.legend = FALSE)

#correlacion de pearson entre los atributos clinicos (1-11)
clinicos<- datosnum[1:11]
Rc<- cor(clinicos)
cor.plot(clinicos, numbers=T, upper=FALSE, main = "Pearson Correlation (C)", show.legend = FALSE)

```


```{r}
#CORRELACION POLYCORICA
poly_cor<- polychoric(histopat)
rho<- poly_cor$rho
cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation (H)", show.legend = FALSE)

#ATRIBUTOS CLINICOS
poly_cor1<- polychoric(clinicos)
rho1<- poly_cor1$rho
cor.plot(poly_cor1$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation (C)", show.legend = FALSE)


#tetrachoric(datosnum$erythema,datosnum$scaling,correct=.5,smooth=TRUE,global=TRUE,weight=NULL,na.rm=TRUE,
#     delete = TRUE)
#tetrachoric(datosnum)
```


# VAMOS A IMPORTAR EL SPSS YA DEPURADO.

quiero poner los tipos de variables bien en la base de datos para luego hacer las tecnicas tambien aqui. Tengo problemas para convertir las variables en factor
## FALTA DICOTOMIZAR ALGUNAS VARIABLES MAS 
```{r}

library(haven)
datosps <- read_sav("dermatology.sav")
#como no se como cambiar los tipos de variables a factor lo hago a mano 

datosps$erythema<- factor(datosps$erythema,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$scaling<- factor(datosps$scaling,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$definiteborders<- factor(datosps$definiteborders,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$itching<- factor(datosps$itching,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$koebnerphenomenon<- factor(datosps$koebnerphenomenon,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)


datosps$polygonalpapules<- factor(datosps$polygonalpapules,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$follicularpapules<- factor(datosps$follicularpapules,
                          labels = c("Ausencia", "Presencia"),
                          ordered=FALSE)

datosps$oralmucosalinvolvement<- factor(datosps$oralmucosalinvolvement,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$kneeandelbowinvolvement<- factor(datosps$kneeandelbowinvolvement,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$scalpinvolvement<- factor(datosps$scalpinvolvement,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$familyhistory<- factor(datosps$familyhistory,
                          labels = c("No", "Si"),
                          ordered=FALSE)

datosps$melaninincontinence<- factor(datosps$melaninincontinence,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$eosinophilsintheinfiltrate<- factor(datosps$eosinophilsintheinfiltrate,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$PNLinfiltrate<- factor(datosps$PNLinfiltrate,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$fibrosisofthepapillarydermis<- factor(datosps$fibrosisofthepapillarydermis,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$exocytosis<- factor(datosps$exocytosis,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$acanthosis<- factor(datosps$acanthosis,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$hyperkeratosis<- factor(datosps$hyperkeratosis,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$parakeratosis<- factor(datosps$parakeratosis,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$clubbingofthereteridges<- factor(datosps$clubbingofthereteridges,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$elongationofthereteridges<- factor(datosps$elongationofthereteridges,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$thinningofthesuprapapillaryepidermis<- factor(datosps$thinningofthesuprapapillaryepidermis,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$spongiformpustule<- factor(datosps$spongiformpustule,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$munromicroabcess<- factor(datosps$munromicroabcess,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$focalhypergranulosis<- factor(datosps$focalhypergranulosis,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$disappearanceofthegranularlayer<- factor(datosps$disappearanceofthegranularlayer,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$vacuolisationanddamageofbasallayer<- factor(datosps$vacuolisationanddamageofbasallayer,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$spongiosis<- factor(datosps$spongiosis,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$sawtoothappearanceofretes<- factor(datosps$sawtoothappearanceofretes,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$follicularhornplug<- factor(datosps$follicularhornplug,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$perifollicularparakeratosis<- factor(datosps$perifollicularparakeratosis,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)


datosps$inflammatorymonoluclearinflitrate<- factor(datosps$inflammatorymonoluclearinflitrate,
                          labels = c("Ausencia","Poco Presente","Algo Presente","PresenciaMax"),
                          ordered=TRUE)

datosps$bandlikeinfiltrate<- factor(datosps$bandlikeinfiltrate,
                          labels = c("Ausencia","Presencia"),
                          ordered=FALSE)

datosps$desease<- factor(datosps$desease,
                          labels = c("psoriasis","sebderm","lichplan","prosea","crodemr","prubra"),
                          ordered=FALSE)



```
**ANÁLISIS DE CORRELACIONES **
Visualizamos la variable dependiente
Tengo que mirar que variables son las que mejor correalcionana con la dependiente

```{r}

plot(datosps$desease, col=c("red","green","purple","yellow","blue","cyan"))
plot(datosps$follicularpapules)
plot(datosps$eosinophilsintheinfiltrate)
plot(datosps$fibrosisofthepapillarydermis)
plot(datosps)

```

```{r}
tabla1<-table(datosps$erythema,datosps$desease)
tabla1
chisq.test(tabla1)
mosaicplot(tabla1, color=c("red","green","purple","yellow","blue","cyan"), main="deseaseXerythema")
```


```{r}
tabla2<- table(datosps$familyhistory,datosps$desease)
mosaicplot(tabla2,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXhisfam")
chisq.test(tabla2)
```
```{r}
tabla3<-table(datosps$melaninincontinence,datosps$desease)
chisq.test(tabla3)
mosaicplot(tabla3,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXmelincont")
plot(datosps$melaninincontinence)
```

```{r}
tabla4<- table(datosps$parakeratosis,datosps$desease)
chisq.test(tabla4)
mosaicplot(tabla4,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXparak")


```

```{r}
tabla5<- table(datosps$itching,datosps$desease)
chisq.test(tabla5)
mosaicplot(tabla5,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXitching")


```
Como vemos esta distribucion del grafico no nos dice gran cosa sobre las relaciones que hay entre las enfermedades. 
```{r}
tabla6<- table(datosps$exocytosis,datosps$desease)
chisq.test(tabla6)
mosaicplot(tabla6,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXexocy")


```
Segun el pvalor de estos resultados aparece que hay una relacion pero si nos fijamos en el grafico no vemos claramente como es la relacion. No nos podemos hacer a la idea de la relacion

```{r}
tabla7<- table(datosps$hyperkeratosis,datosps$desease)
chisq.test(tabla7)
mosaicplot(tabla7,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXhyperk")


```
```{r}
ggplot(data=datosps,aes(x=Age,fill=as.factor(desease)))+
  geom_density(alpha=0.4)+ggtitle("20MID0042")
```
```{r}
ggplot(data=datosps, aes(desease,Age)) +
  geom_boxplot()
```

VEMOS COMO LAS EDADES MAS JOVENES ESTAN ASOCIADAS A CONTRAER LA ENFERMEDAD DE PRUBRA.
Podemos ver que todas las variables nos salen con relacionadas y nos podemos hacer una idea de su relacion mediante el grafico cruzado de las dos variables. 



*VAMOS A HACER UN SUMMARY DE LOS DATOS PARA VER COMO ESTAN DISTRIBUIDOS Y SI TIENEN DATOS FALTANTES*
```{r}
str(datos)
summary(datos)
```



la ultima variable es desease que es nuestra varaible a predecir. tiene 6 niveles cada uno indicando
el tipo de enfermedad:
  - 1:psoriasis
  - 2:seboreic dermatitis
  - 3:lichen planus
  - 4:pityriasis rosea 
  - 5:cronic dermatitis
  - 6:pityriasis rubra pilaris
  
Pasamos el Data Frame a excel para después pasarlo a SPSS.
  
```{r}
install.packages("openxlsx")
library(openxlsx)
write.xlsx(datos,"dermatology.xlsx")
```

## realizamos una particion aleatoria de datos 

```{r}
table(datos$desease)/nrow(datos)
```


```{r}

install.packages("carData")
library(caret)
library(pROC)
library(car)



set.seed(12345)
trainIndex <- createDataPartition(datosps$desease, p=0.8, list=FALSE)
data_trainf <- datosps[trainIndex,]
data_testf <- datosps[-trainIndex,]

set.seed(12345)
trainIndex1 <- createDataPartition(datosnum$desease, p=0.8, list=FALSE)
data_train <- datosnum[trainIndex1,]
data_test <- datosnum[-trainIndex1,]
```
# REALIZAMOS UNA REGRESION LOGISTICA MULTINOMIAL
```{r}
str(datosps)
```
realizamos un diagrama de barras para ver mejor la variable dependiente
```{r}
plot(datosps$desease)
table(datosps$desease)/nrow(datosps)
```
Como vemos, la categoria mas predominante es la psoriasis con un 31% de las observacoines y la menos es la prubra con un 5% de observaciones. las dems estan mas igualadas entre si.     

Para la logistica multinomial r toma de referencia la primera categoria que en nuestro caso es *psoriasis*



## modelo inicial
```{r}
library(nnet)
library(stargazer)
library(car)
library(caret)
library(pROC)

modelo_inicial<- multinom(desease ~., data=data_trainf)

stargazer(modelo_inicial,type="text")
```

No me sale ningun parametro significativo . Lo que tengo que preguntar es por qué sale despues de cada categoria de las variables explicativas un .L o .Q o .C ????





_Solucionado los problemas de tipos de datos en la variable respuesta y en la edad (tengo que preguntar a ver si en los demas tipos de datos es correcto asumirlas factoras. ), vemos el resumen del modelo con todas las variables y la significatividad de las variables. Como es un modelo multinomial, las conclusiones se toman en base a la primera categoria ( 1 es la categoria de referencia). Un problema con este tipo de modelos es que algunas variables tienen parametros no significativos para algunas de sus categorias. A primera vista, todas las variables en las que todas sus categorias no son ninguna. Es complejo ya que la variable dependiente tiene muchas categorias._

ANALISIS DE TIPO II:
```{r}
Anova(modelo_inicial,type="II")
```
Hacemos esto para ver qué variables tienen mas importancia a la hora de predecir las distintas categorias de la variable "desease". Las variables con las que nos quedariamos a priori para hacer la regresión. Vemos como ninguna variable sale significativa para predecir la varible desease. !!!! 

 MI ME DA QUE ESTOY TOMANDO MAL EL TIPO DE VARIABLE O LA ESCALA DE LAS VARIABLES REGRESORAS. PREGUNTARRRR
Como con el modelo inicial ( con todas las variables ) no nos quedamos con ningun modelo provamos con otros

## AHORA VAMOS A PROBAR CON LOS MODELOS AUTOMATICOS DE SELECCION DE VARIABLES 
```{r,results='hide',message=FALSE}
null<-multinom(desease ~ 1, data=data_trainf,trace=F)
full<-multinom(desease ~ ., data=data_trainf,trace=F)

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",
k=log(nrow(data_trainf)),trace=F)

modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both",trace=F)
modeloForwBIC<-step(null, scope=list(lower=null, upper=full), direction="forward",
                    k=log(nrow(data_trainf)),trace=F)

modeloForwAIC<-step(null, scope=list(lower=null, upper=full), direction="forward",trace=F)

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",
k=log(nrow(data_trainf)),trace=F)
modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward",trace=F)

modelos<-list(modeloStepBIC,modeloStepAIC,modeloForwBIC,modeloForwAIC,modeloBackBIC,
modeloBackAIC)
sapply(modelos,function(x) x$call)
```
```{r}
sapply(modelos,function(x) x$edf)
```
Como podemos observar, los 6 modelos con el backward y el fordward con el AIC y el BIC nos dan 3 modelos. uno con 50 parametros,otro con 60 y otro con 55. 


Vamos a recurrir a la validacion cruzada repetida para ver que modelo de los 3 es el mejor. Evaluamos los modelos mediante el indice Kappa el AUC one vs all
```{r}
modelos<- list(modeloStepBIC, modeloStepAIC,modeloBackBIC)
vcrTodosModelos<-list()
for (i in 1:length(modelos)){
set.seed(1712)
vcr<-train(formula(modelos[[i]]), data = data_trainf,
method = "multinom",
tuneGrid=expand.grid(decay=0), trace=FALSE,
trControl = trainControl(method="repeatedcv", number=5, repeats=20,
summaryFunction=multiClassSummary, classProbs=TRUE)
)
vcrTodosModelos[[i]]<-vcr
}
bwplot(resamples(vcrTodosModelos),metric=c("AUC", "Kappa", "Accuracy"),
scales = list(x = list(relation = "free")))
```
Aparentemente el modelo 2 es el mejor en cuanto a AUC y a Kappa que son los que nos interesan. Tiene valores mas altos y presenta menos variabilidad en los diagramas de cajas. 
```{r}
summary(resamples(vcrTodosModelos),metric=c("AUC", "Kappa", "Accuracy"))
sapply(modelos,function(x) x$edf)

```
Es importante saber que en los casos en la que la variable dependiente esta desbalanceada tenemos que dar mas credibilidad al AUC y el kappa. 
Con estos resultados tenemos que quedarnos con el modelo 2. El inconveniente es que es el modelo que tiene mas parametros . El que le sigue es el modelo 1 que tiene un AUC del 0.98 que es un porciento peor que el modelo 2. Tenemos que valorar cuanto ganamos en simplicidad para aplicar el principio de parsimonia. Por no ser tan estrictos, vamos a quedarnos con el modelo 1 ya que es el mas simple y nos arroja resultados mejores. 

para concluir con la regresion obtendremos los ODDS RATIO para su interpretacion ( pagina 13)
```{r}
stargazer(modeloStepBIC, type="text", coef=list(exp(coef(modeloStepBIC))), p.auto=FALSE)

```
No se ve muy bien el stargazer con las tablas xddd
```{r}
Anova(modeloStepBIC,type="II")
```
Con este analisis de tipo II las variables mas significativas para el modelo elegido son elongationoftheridges seguido por sawtoothapperaranceofretes y asi...-

FINALIZAMOS CON LA CURVA AUC PARA CADA UNA DE LAS CATEGORIAS. A VER QUE CATEGORIA QUEDA MEJOR EXPLICADA CON ESTE MODELO
```{r}
input<-data.frame(real=data_testf$desease, predict(modeloStepBIC, data_testf, type='probs'))
count<-0
salidaAUC<-c()
for (class in levels(data_testf$desease)){
count<-count+1
obs <- ifelse(input[, "real"] == class, 1, 0)
prob <- input[,class]
curvaRoc<-roc(obs, prob, direction="<")
salidaAUC<-c(salidaAUC,curvaRoc$auc)
if (count==1)
plot(curvaRoc)
else
plot(curvaRoc,add=T,col=count)
}
legend("bottomright", legend=paste0(levels(data_testf$desease),":",round(salidaAUC,3)),
col=1:4, lty=1, cex=0.8)


```
```{r}
mean(salidaAUC)
```
vemos como las enfermedades que mejor estan modelizadas son psoriasis, prubra y crodemr con un auc de 1. Muy raro. Las que son mas normales son el resto que no tienen un auc de 1. Eso que este modelo es el que tenia menos auc porque el modelo2 tenia mas parametros pero modelizaba aun mejor. Preguntar a AIDA.

TENEMOS QUE MIRAR COMO INTERPRETAR LOS ODDS RATIO DE ESTE MODELO Y PREGUNTAR SI ESTA BIEN PORQUE LOS RESULTADOS QUE ESTA DANDO SON MUY SOSPECHOSOS. 
## MATRIZ DE CONFUSION
```{r}
cm_test<-confusionMatrix(table(predict(modeloStepBIC,newdata=data_testf),data_testf$desease))
cm_test$table
cm_test$overall[1:2]
cm_test$byClass[,1:2]

```
Estos resultados no son posibles ya que no es matematicamente posible maximizar sensibilidad y especificidad al mismo tiempo como pasa en las dos ultimas categorias. esto se puede deber a las pocas observaciones de las categorias. 

# NAIVE BAYES
```{r}
install.packages("foreach")
install.packages("iterators")
install.packages("parallel")
library(caret)
library(naivebayes)
library(pROC)
# Para paralelizar
library(doParallel)
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)


```
tenemos que poner como entera la variable edad para el naive bayes:
```{r}
datosps$Age <- as.integer(datosps$Age)
data_trainf<-datosps[trainIndex,]
data_testf<-datosps[-trainIndex,]
class(datosps$Age)

```
## selecion de variables en NB
```{r}
#la variable dependiente está en la cuarta columna y por eso la excluimos
salida<-filterVarImp(x = data_trainf[,-35], y = data_trainf$desease)
ranking<-sort(apply(salida, 1, mean), decreasing =T)
# Para ajustar el margen inferior del gráfico y que así quepan los nombres
par(mar=c(8.1, 4.1, 4.1, 2.1))
bb<-barplot(ranking, las=2, ylim=c(0,1))
text(bb, ranking+0.03, labels = round(ranking, 2))
```
Como vemos, las variables explicativas estan ordenadas segun el AUC que tengan respecto a la variable dependiente. Una vez que las tenemos ordenadas por el AUC debemos determinar cuantas de las variables mas importantes se incluyen en el modelo

```{r}
dataVCR<-data_trainf
nbFuncs$summary <- multiClassSummary
set.seed(12345)
results <- rfe(dataVCR[,-35],dataVCR[,35],
sizes=c(1:(ncol(dataVCR)-1)), metric = "AUC",
rfeControl = rfeControl(functions = nbFuncs,
method="cv", number=10),
)
# Para aplicar r
plot(results)
```
NO SE POR QUE MESALE ESTE ERROR 

# ARBOL DE CLASIFICACION
```{r}
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(rattle)
```


```{r}
set.seed(12345)
modgini<-rpart(desease~., data=data_trainf, method = "class",
minbucket=ceiling(0.02*nrow(data_trainf)),cp=0,
parms=list(split="gini"),maxsurrogate = 0)

rpart.plot(modgini,extra=105,nn=TRUE,tweak=1.2)


```
ESTE ARBOL TIENE BUENA PINTA, ME SACA LAS 6 CATEGORIAS PERO FALTA LA INTERPRETACIÓN.
Vemos como segun el color del nodo es una categoria de la variable desease.
En el nodo de la izquierda del todo, vemos como clasifica a la enfermedad 1 que corresponde a psoriasis. Los individuos que tienen *adelgazamiento de la epidermis suprapapilar* en cualquier medida ( 1,2 0 3) son mas probables a desencadenar psoriasis. Asi con todos los nodos. 

```{r}
asRules(modgini)
```
Ahora vamos a crear el arbol de clasificacion pero con el criterio de entropía:
```{r}
set.seed(12345)
modEnt<-rpart(desease~., data=data_trainf, method = "class",
minbucket=ceiling(0.02*nrow(data_trainf)),cp=0,
parms=list(split="information"),maxsurrogate = 0)


rpart.plot(modEnt,extra=105,nn=TRUE,tweak=1.2)


```
En relacion al arbol con el indice de gini,vemos como este se compone de mas hojas y mas variables de decisión. las variables que se mantienen son *vacuolisation and damage of basal layer* y *fibrosis of the papillary dermis* lo que nos puede indicar la importancia de esas variables ya que nos aparecen en los distintos arboles con distintas reglas.

## DISTINTOS ARBOLES PARA VER CUAL ES EL MEJOR 

Ahora variamos el minbucket ( el tamño de hoja) y el criterio correspondiente
```{r}
set.seed(12345)

modeloGini1<-rpart(desease~., data=data_trainf, method = "class",
minbucket=ceiling(0.01*nrow(data_trainf)),cp=0,
parms=list(split="gini"),maxsurrogate = 0)

set.seed(12345)
modeloGini5<-rpart(desease~., data=data_trainf, method = "class",
minbucket=ceiling(0.05*nrow(data_trainf)),cp=0,
parms=list(split="gini"),maxsurrogate = 0)

set.seed(12345)
modeloEnt1<-rpart(desease~., data=data_trainf, method = "class",
minbucket=ceiling(0.01*nrow(data_trainf)),cp=0,
parms=list(split="information"),maxsurrogate = 0)

set.seed(12345)
modeloEnt5<-rpart(desease~., data=data_trainf, method = "class",
minbucket=ceiling(0.05*nrow(data_trainf)),cp=0,
parms=list(split="information"),maxsurrogate = 0)

modelos<-list(modeloGini1,modgini,modeloGini5,modeloEnt1,modEnt,modeloEnt5)

#sapply(modelos,
##function(x) roc(data_trainf$desease,
#predict(x,data_trainf,type="prob")[,2], direction="<")$auc)


#
sapply(modelos,function(x)multiclass.roc(data_trainf$desease,
predict(x,data_trainf,type="prob"))$auc)
#
sapply(modelos,function(x)multiclass.roc(data_testf$desease,
predict(x,data_testf,type="prob"))$auc)
#sapply(modelos,
#function(x) roc(data_test1$desease,
#predict(x,data_test1,type="prob")[,2], direction="<")$auc)

sapply(modelos,function(x) sum(x$frame$var == "<leaf>"))
```
Vemos que todos los modelos que hemos probado el que mas estable es y mejor AUC tiene es el tercer modelo que es el modelogini5. Escogemos el tercero porque a pesar que tiene menor auc, es mas simple. porque tiene 6 hojas. 
# AHORA VAMOS A HACER UNA PODA DE ARBOLES
```{r}
modEnt$cptable
plotcp(modEnt)
```

