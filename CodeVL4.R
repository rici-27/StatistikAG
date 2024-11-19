setwd("C:/Users/Anwender/Desktop/Statistik AG/Vorlesung4")

## Part 1: Daten einlesen und anschauen
realestate = read.csv("Realestate.csv", sep=";",row.names = 1)
View(realestate)
dim(realestate)
str(realestate)
library(DT)
datatable(realestate, filter = "top", rownames = FALSE,
          options = list(pageLength = 8))
  # rownames = FALSE entfernt die Nummerierung der Zeilen

## Part 2: Preise und Alter als Variablen aufbereiten
price<-realestate$Y.house.price.of.unit.area
price # Preise werden einfach in einen Vektor gepackt
y<-as.numeric(sub(",", ".", price, fixed = TRUE)) 
  # und das format wird geändert
age<-realestate$X2.house.age
x1<-as.numeric(sub(",", ".", age, fixed = TRUE))

## Part 3: Lineare Regression/ Least Squared Estimator mit Preise & Alter
lm(y~x1)
plot(x1,y,main="OLS Residual-Plot",ylab="price",xlab="age")
lines(x1,fitted(lm(y~x1)))
segments(x1,fitted(lm(y~x1)),x1,y,lty=2)

## Part 4: ... mit Preisen & Distance
dist<-realestate$X3.distance.to.the.nearest.MRT.station
x2<-as.numeric(sub(",", ".", dist, fixed = TRUE))
plot(x2,y,main="OLS Residual-Plot",ylab="price",xlab="distanceMRT")
lines(x2,fitted(lm(y~x2)))
segments(x2,fitted(lm(y~x2)),x2,y,lty=2)

## Part 5: andere Möglichkeit?
lm.fit = lm(y ~ x2)
summary(lm.fit)

## Part 6: andere Daten
z<-runif(1000)
x<-runif(1000)
lm.fit = lm(z ~ x)
summary(lm.fit)
plot(x,z)
lines(x,fitted(lm(x~z)))

## Part 7: der lm befehl
?lm
lm.fit$coefficients

## Part 8: Multivariate
lm.fit = lm(y ~ x1 + x2)
summary(lm.fit)

## Part 9: MatMul in R
str(c(1,2,3)%*%c(1,2,3))
crossprod(c(1,2,3))
sum(c(1,2,3)^2)

## Part 10: Def des LSE
ls<-function(X, y){
  lseq <- function(w, X, y) {
    crossprod(y - X%*%w)
  }
  result=optim(rep(0, ncol(X)), lseq, X=X, y=y, method='BFGS')
  result$par
}

## Part 11: Design Matrix durch Kombi von x1 und x2 erstellen
design<-cbind(x1,x2)
?cbind
ls(design,y)

## Part 12: Reg ohne Intercept durchführen
lm(y ~ 0+ x1 + x2)$coefficients

## Part 13: mit intercept in design matrix (testen der eigenen fkt)
design<-cbind(rep(1,length(x1)),x1,x2)
ls(design,y)

## Part 14: Multivariate
names(realestate) <- c("x_1","x_2","x_3","x_4","x_5","x_6","y")
x1<-realestate$x_1
x1<-as.numeric(sub(",", ".", x1, fixed = TRUE))
x2<-realestate$x_2
x2<-as.numeric(sub(",", ".", x2, fixed = TRUE))
x3<-realestate$x_3
x3<-as.numeric(sub(",", ".", x3, fixed = TRUE))
x4<-realestate$x_4
x4<-as.numeric(sub(",", ".", x4, fixed = TRUE))
x5<-realestate$x_5
x5<-as.numeric(sub(",", ".", x5, fixed = TRUE))
x6<-realestate$x_6
x6<-as.numeric(sub(",", ".", x6, fixed = TRUE))
lm.fit = lm(y ~ x1+x2+x3+x4+x5+x6)

## Part 15: glmnet (Lasso wegen alpha=1 (war nicht im code aber ist standard wert))
library(glmnet)
design<-cbind(x1,x2,x3,x4,x5,x6)
?glmnet
as.numeric(glmnet(design,y,intercept = T,nlambda=1,lambda=1, alpha=1)$beta)
as.numeric(glmnet(design,y,intercept = T,nlambda=1,lambda=cv.glmnet(design,y,intercept = T)$lambda.min)$beta)

