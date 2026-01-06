---
title: "TFG"
author: "Jonathan Soto Botina"
date: "2023-03-28"
output: html_document
---
#Este es el documento de R para el TFG. 

 

library(haven)
library(psych)
library(corrplot)
library(polycor)
library(ggplot2)
datosnum <- read_sav("dermatology.sav")



histopat<- datosnum[12:33]
Rh<- cor(histopat)
cor.plot(histopat, numbers=T, upper=FALSE, main = "Pearson Correlation (H)", show.legend = FALSE)

clinicos<- datosnum[1:11]
Rc<- cor(clinicos)
cor.plot(clinicos, numbers=T, upper=FALSE, main = "Pearson Correlation (C)", show.legend = FALSE)

poly_cor<- polychoric(histopat)
rho<- poly_cor$rho
cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation (H)", show.legend = FALSE)


poly_cor1<- polychoric(clinicos)
rho1<- poly_cor1$rho
cor.plot(poly_cor1$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation (C)", show.legend = FALSE)


library(haven)
datosps <- read_sav("dermatology.sav")
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





plot(datosps$desease, col=c("red","green","purple","yellow","blue","cyan"))
plot(datosps$follicularpapules)
plot(datosps$eosinophilsintheinfiltrate)
plot(datosps$fibrosisofthepapillarydermis)
plot(datosps)



tabla1<-table(datosps$erythema,datosps$desease)
tabla1
chisq.test(tabla1)
mosaicplot(tabla1, color=c("red","green","purple","yellow","blue","cyan"), main="deseaseXerythema")

tabla2<- table(datosps$familyhistory,datosps$desease)
mosaicplot(tabla2,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXhisfam")
chisq.test(tabla2)

tabla3<-table(datosps$melaninincontinence,datosps$desease)
chisq.test(tabla3)
mosaicplot(tabla3,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXmelincont")
plot(datosps$melaninincontinence)

tabla4<- table(datosps$parakeratosis,datosps$desease)
chisq.test(tabla4)
mosaicplot(tabla4,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXparak")



tabla5<- table(datosps$itching,datosps$desease)
chisq.test(tabla5)
mosaicplot(tabla5,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXitching")


tabla6<- table(datosps$exocytosis,datosps$desease)
chisq.test(tabla6)
mosaicplot(tabla6,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXexocy")

tabla7<- table(datosps$hyperkeratosis,datosps$desease)
chisq.test(tabla7)
mosaicplot(tabla7,color= c("red","green","purple","yellow","blue","cyan"), main="deseaseXhyperk")

ggplot(data=datosps,aes(x=Age,fill=as.factor(desease)))+
  geom_density(alpha=0.4)+ggtitle("20MID0042")

ggplot(data=datosps, aes(desease,Age)) +
  geom_boxplot()

str(datos)
summary(datos)


install.packages("openxlsx")
library(openxlsx)
write.xlsx(datos,"dermatology.xlsx")


table(datos$desease)/nrow(datos)

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

str(datosps)

#realizamos un diagrama de barras para ver mejor la variable dependiente

plot(datosps$desease)
table(datosps$desease)/nrow(datosps)


library(nnet)
library(stargazer)
library(car)
library(caret)
library(pROC)

modelo_inicial<- multinom(desease ~., data=data_trainf)

stargazer(modelo_inicial,type="text")


Anova(modelo_inicial,type="II")


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

sapply(modelos,function(x) x$edf)

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


summary(resamples(vcrTodosModelos),metric=c("AUC", "Kappa", "Accuracy"))
sapply(modelos,function(x) x$edf)

stargazer(modeloStepBIC, type="text", coef=list(exp(coef(modeloStepBIC))), p.auto=FALSE)



Anova(modeloStepBIC,type="II")

       
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


mean(salidaAUC)

       
cm_test<-confusionMatrix(table(predict(modeloStepBIC,newdata=data_testf),data_testf$desease))
cm_test$table
cm_test$overall[1:2]
cm_test$byClass[,1:2]

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


datosps$Age <- as.integer(datosps$Age)
data_trainf<-datosps[trainIndex,]
data_testf<-datosps[-trainIndex,]
class(datosps$Age)



salida<-filterVarImp(x = data_trainf[,-35], y = data_trainf$desease)
ranking<-sort(apply(salida, 1, mean), decreasing =T)

par(mar=c(8.1, 4.1, 4.1, 2.1))
bb<-barplot(ranking, las=2, ylim=c(0,1))
text(bb, ranking+0.03, labels = round(ranking, 2))

dataVCR<-data_trainf
nbFuncs$summary <- multiClassSummary
set.seed(12345)
results <- rfe(dataVCR[,-35],dataVCR[,35],
sizes=c(1:(ncol(dataVCR)-1)), metric = "AUC",
rfeControl = rfeControl(functions = nbFuncs,
method="cv", number=10),)

plot(results)


library(caret)
library(rpart)
library(rpart.plot)
library(pROC)
library(rattle)




set.seed(12345)
modgini<-rpart(desease~., data=data_trainf, method = "class",
minbucket=ceiling(0.02*nrow(data_trainf)),cp=0,
parms=list(split="gini"),maxsurrogate = 0)

rpart.plot(modgini,extra=105,nn=TRUE,tweak=1.2)




asRules(modgini)

       
set.seed(12345)
modEnt<-rpart(desease~., data=data_trainf, method = "class",
minbucket=ceiling(0.02*nrow(data_trainf)),cp=0,
parms=list(split="information"),maxsurrogate = 0)


rpart.plot(modEnt,extra=105,nn=TRUE,tweak=1.2)



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


modEnt$cptable
plotcp(modEnt)


