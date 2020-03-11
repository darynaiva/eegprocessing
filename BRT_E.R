library(pwr)
library(psych)
library(Hmisc)
library(ggplot2)
library(cluster)
library(factoextra)
library(SDMPlay)
library(corrplot)
library(caret)
library(gbm)
library(dismo)

a <- read.csv(
  file = "alpha.csv",
  sep = "\t",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors=FALSE,
  dec = ",",
  colClasses=c("numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               
               "numeric", "numeric", "numeric", "numeric"
  )
)

b <- read.csv(
  file = "beta.csv",
  sep = "\t",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors=FALSE,
  dec = ",",
  colClasses=c("numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               
               "numeric", "numeric", "numeric", "numeric"
  )
)

g <- read.csv(
  file = "gamma.csv",
  sep = "\t",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors=FALSE,
  dec = ",",
  colClasses=c("numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               
               "numeric", "numeric", "numeric", "numeric"
  )
)

d <- read.csv(
  file = "delta.csv",
  sep = "\t",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors=FALSE,
  dec = ",",
  colClasses=c("numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               
               "numeric", "numeric", "numeric", "numeric"
  )
)

t <- read.csv(
  file = "teta.csv",
  sep = "\t",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors=FALSE,
  dec = ",",
  colClasses=c("numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               "numeric", "numeric", "numeric", "numeric", "numeric", 
               
               "numeric", "numeric", "numeric", "numeric"
  )
)

E <- read.csv(
  file = "E.csv",
  sep = "\t",
  header = TRUE,
  encoding = "UTF-8",
  stringsAsFactors=FALSE,
  dec = ",",
  colClasses=c("numeric", "numeric")
)

colnames(a)

data <- cbind(a,b,d,g,t, E)
colnames(data) <- c("aFp1", "aFp2", "aF3",  "aF4",  "aF7",  "aF8",  "aT3",  "aT4",  "aC3",  "aC4",  
                    "aT5",  "aT6",  "aP3",  "aP4",  "aO1",  "aO2",  "aFz",  "aCz", "aPz",
                    
                    "bFp1", "bFp2", "bF3",  "bF4",  "bF7",  "bF8",  "bT3",  "bT4",  "bC3",  "bC4",  
                    "T5",  "T6",  "P3",  "P4",  "O1",  "O2",  "Fz",  "Cz", "Pz",
                    
                    "dFp1", "dFp2", "dF3",  "dF4",  "dF7",  "dF8",  "dT3",  "dT4",  "dC3",  "dC4",  
                    "dT5",  "dT6",  "dP3",  "dP4",  "dO1",  "dO2",  "dFz",  "dCz", "dPz",
                    
                    "gFp1", "gFp2", "gF3",  "gF4",  "gF7",  "gF8",  "gT3",  "gT4",  "gC3",  "gC4",  
                    "gT5",  "gT6",  "gP3",  "gP4",  "gO1",  "gO2",  "gFz",  "gCz", "gPz",
                    
                    "tFp1", "tFp2", "tF3",  "tF4",  "tF7",  "tF8",  "tT3",  "tT4",  "tC3",  "tC4",  
                    "tT5",  "tT6",  "tP3",  "tP4",  "tO1",  "tO2",  "tFz",  "tCz", "tPz",
                    
                    "E", "N")

corrplot(cor(a), method = 'number', tl.cex = 0.5, number.cex = 0.5)
corrplot(cor(b), method = 'number', tl.cex = 0.5, number.cex = 0.5)
corrplot(cor(d), method = 'number', tl.cex = 0.5, number.cex = 0.5)
corrplot(cor(g), method = 'number', tl.cex = 0.5, number.cex = 0.5)
corrplot(cor(cldata), method = 'number', tl.cex = 0.3, number.cex = 0.3)



a$PF <- a$Pz - a$Fz
sig.corr <- corr.test(a$PF, a$V20)
sig.corr$p
a <- a[, -21]

b$PF <- b$Pz - b$Fz
sig.corr <- corr.test(b$PF, b$V20)
sig.corr$p
b <- b[, -21]

d$PF <- d$Pz - d$Fz
sig.corr <- corr.test(d$PF, d$V20)
sig.corr$p
d <- d[, -21]

g$PF <- g$Pz - g$Fz
sig.corr <- corr.test(g$PF, g$V20)
sig.corr$p
g <- g[, -21]

t$PF <- t$Pz - t$Fz
sig.corr <- corr.test(t$PF, t$V20)
sig.corr$p

t$P4tb <- t$P4/b$P4
sig.corr <- corr.test(t$P4tb, t$V20)
sig.corr$p
t$PF2 <- (t$Pz - (t$Fp1+t$Fp2)/2)
sig.corr <- corr.test(t$PF2, t$V20)
sig.corr$p
t <- t[, -23]

model1 <- lm(t$V20 ~ t$P4tb + t$PF)
plot(model1)

min(t$V20)
max(t$V20)
Mode(t$V20)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

cldata <- na.omit(data)
Fp <- cbind(cldata$aFp1,cldata$aFp2,
            cldata$bFp1, cldata$bFp2,
            cldata$tFp1, cldata$tFp2,
            cldata$gFp1, cldata$gFp2,
            cldata$dFp1, cldata$dFp2,
            cldata$V99)
colnames(Fp) <- c("aFp1","aFp2",
                  'bFp1', 'bFp2',
                  'tFp1', 'tFp2',
                  'gFp1', 'gFp2',
                  'dFp1', 'dFp2',
                  'E')
corrplot(cor(Fp), method = 'number')

nrow(Fp[which(Fp[,11] == 0),])
data0 <- as.data.frame(Fp[which(Fp[,11] == 0),])
data0 <- data0[-c(114:122),]
ind=sample(2,nrow(data0),replace=T,prob=c(0.7,.3))
data1 <- Fp[which(Fp[,11] == 1),]
fdata <- rbind(data0[1:113,], data1)

set.seed(502)
ind=sample(2,nrow(fdata),replace=T,prob=c(0.7,.3))
train<- as.data.frame(fdata[ind==1,])
test<-as.data.frame(fdata[ind==2,])
nrow(test[which(test[, 11]==0),])
nrow(train[which(train[, 11]==0),])

#creation of the greed for boosting 
grid<-expand.grid(.n.trees=seq(100,500,by=200),.interaction.depth=seq(1,4,by=1),.shrinkage=c(.001,.01,.1),
                  .n.minobsinnode=10)
control<-trainControl(method = "CV")

#train the model
gbm.train<-train(E~.,data=train,method='gbm',trControl=control,tuneGrid=grid)
gbm.train

gbm.E.Fp <- gbm(E~.,data=train,n.trees = 100,interaction.depth = 1,
             shrinkage = .001,distribution = 'bernoulli', cv.folds = 5)
n_trees.Fp <- gbm.perf(gbm.E.Fp, method = "cv")
gbm.test.Fp <- predict(gbm.E.Fp,newdata = test,n.trees = n_trees.Fp)

#testing the model
?calc.deviance(obs = test$E, pred = gbm.test, calc.mean = T)
PredictionBinaries <- as.factor(ifelse((gbm.test.Fp/21) > 0.5,1,0))
FactorE <- as.factor(test$E)
confusionMatrix(PredictionBinaries, FactorE)

#another model
cv.gbm.E <- gbm(c~.,data=train,n.trees = 300,interaction.depth = 3,
                shrinkage = .001,distribution = 'bernoulli', cv.folds = 5)
n_tree_cv <- gbm.perf(cv.gbm.E, method = 'cv')
n_tree_cv <- gbm.perf(cv.gbm.E, method = 'OOB')
print(n_tree_cv)
gbmCrossVal(cv.folds = 300, data = train, nTrain = 3, class.stratify.cv = F,
            distribution = 'bernoulli')

#model with more data
data <- cbind(a, b, d, g, t, deparse.level = 1)
data <- data[, -c(20,40, 60, 80)]
colnames(a)
colnames(data) <- c("aFp1", "aFp2", "aF3",  "aF4",  "aF7",  "aF8",  "aT3",  "aT4",  "aC3",  "aC4",  "aT5",  "aT6",  
                    "aP3",  "aP4",  "aO1",  "aO2",  "aFz",  "aCz", "aPz",
                    "bFp1", "bFp2", "bF3",  "bF4",  "bF7",  "bF8",  "bT3",  "bT4",  "bC3",  "bC4",  "bT5",  
                    "bT6",  "bP3",  "bP4",  "bO1",  "bO2",  "bFz",  "bCz", "bPz",
                    "dFp1", "dFp2", "dF3",  "dF4",  "dF7",  "dF8",  "dT3",  "dT4",  "dC3",  "dC4",  "dT5",  
                    "dT6",  "dP3",  "dP4",  "dO1",  "dO2",  "dFz",  "dCz", "dPz",
                    "gFp1", "gFp2", "gF3",  "gF4",  "gF7",  "gF8",  "gT3",  "gT4",  "gC3",  "gC4",  "gT5",  
                    "gT6",  "gP3",  "gP4",  "gO1",  "gO2",  "gFz",  "gCz", "gPz",
                    "tFp1", "tFp2", "tF3",  "tF4",  "tF7",  "tF8",  "tT3",  "tT4",  "tC3",  "tC4",  "tT5",  
                    "tT6",  "tP3",  "tP4",  "tO1",  "tO2",  "tFz",  "tCz", "tPz", "E", "P4tb", "PF2", "PF",
                    "c")
data <- data[, -96]
set.seed(502)
ind=sample(2,nrow(data),replace=T,prob=c(0.5,.5))
train<-data[ind==1,]
random_index <- sample(1:nrow(train), nrow(train))
random_train <- train[random_index, ]
nrow(test)
nrow(test[which(test$c == 0),])
train <- train[-c(1,3),]
test<-data[ind==2,]

rm(train, test)


##
introverts <- cldata[which(cldata$V99==0),]
i <- nrow(introverts)
ifelse(i %% 2 == 1, introverts <- introverts[-sample(1:i,1),], NA)
extraverts <- cldata[which(cldata$V99==1),]
e <- nrow(extraverts)
ifelse(e %% 2 == 1, extraverts <- extraverts[-sample(1:e,1),], NA)
set.seed(502)
ind=sample(2,nrow(introverts), replace=T, prob=c(0.5,0.5))
train.i <- introverts[ind==1,]
test.i <- introverts[ind==2,]
ind=sample(2,nrow(extraverts),replace=T,prob=c(0.5,0.5))
train.e <- extraverts[ind==1,]
test.e <- extraverts[ind==2,]
train <- rbind(train.e, train.i)
test <- rbind(test.e, test.i)

#creation of the greed for boosting 
grid<-expand.grid(.n.trees=seq(100,500,by=200),.interaction.depth=seq(1,4,by=1),.shrinkage=c(.001,.01,.1),
                  .n.minobsinnode=10)
control<-?trainControl(method = "CV")

#train the model
gbm.E<-train(V99~.,data=train,method='gbm',trControl=control,tuneGrid=grid)
gbm.E

gbm.E.full <- gbm(V99~.,data=train,n.trees = gbm.E$bestTune$n.trees,interaction.depth = gbm.E$bestTune$interaction.depth,
             shrinkage = gbm.E$bestTune$shrinkage,distribution = 'poisson', cv.folds =5, 
             n.minobsinnode=10)
print(gbm.E.full)
summary(gbm.E.full)
n.t <- gbm.perf(gbm.E.full, method = "cv")
gbm.test <- predict(gbm.E.full,newdata = test,n.trees = n.t, type = "response")

#testing the model
gbm.test <- predict.gbm(gbm.E.full,newdata = test,n.trees = 300)
plot(gbm.test, test$V99)
print(gbm.test)
calc.deviance(obs = test$V99, pred = gbm.test, calc.mean = T)
PredictionBinaries <- as.factor(ifelse(gbm.test > 0.50,1,0))
FactorE <- as.factor(test$V99)
confusionMatrix(PredictionBinaries, FactorE)

devtools::install_github("craddm/eegUtils")
  