setwd("C:/Users/Anwender/Desktop/Statistik AG/Blatt2")
library(DT)
library(tidyverse)
library(glmnet)

data = read.csv("Realestate.csv", sep = ";", row.names = 1)
# row.names = 1 führt dazu, dass die erste spalte nicht eingelesen wird
View(data) 
datatable(data, filter = "top", rownames = FALSE,
          options = list(pageLength = 8))

## Teil a) Ridge Regression Estimator

rre <- function(X, y, lambda = 1){
  error <- function(w, X, y, lambda = 1){
    crossprod(y- X %*% w) - lambda * norm(w, type = "2")
  }
  result = optim(rep(0,ncol(X)), error, X=X, y=y, lambda=lambda, method = 'BFGS')
  result$par
}

# Def des LSE zum Vergleich
ls<-function(X, y){
  lseq <- function(w, X, y) {
    crossprod(y - X%*%w)
  }
  result=optim(rep(0, ncol(X)), lseq, X=X, y=y, method='BFGS')
  result$par
}

# (hier kommt ein test mit den retail daten)
# Daten in Vektoren numerisch speichern
# Kommata durch Punkte ersetzen
names(data) <- c("x_1","x_2","x_3","x_4","x_5","x_6","y")
x1 <- data$x_1
x1<-as.numeric(sub(",", ".", x1, fixed = TRUE))
x2<-data$x_2
x2<-as.numeric(sub(",", ".", x2, fixed = TRUE))
y <- data$y
y <- as.numeric(sub(",", ".", y, fixed = TRUE))

# Regression mit ridge und lse durchführen
design<-cbind(x1,x2)
rre(design,y)
ls(design,y)


## Teil b) glmnet Packet

library(glmnet)
?glmnet

lasso <- glmnet(design, y, alpha = 1)
print(lasso)
lasso$beta
coef(lasso, s = lasso$lambda[30])


## Teil c) OVerfitting Problem for Least Squares
## Part 1: Daten erzeugen
rm(x1,x2,y, design, lasso)

x <- runif(100, min=-1, max=1)
?runif
eps <- rnorm(100, mean=0, sd=1)

# Design Matrix erstellen
n <- 100
p <- 20
X <- matrix(0, n, (p+1))
for (i in (1:n)){
  for (j in (1:(p+1))){
    X[i,j] = x[i]**(j-1)
  }
}
rm(i, j)

# beta als Spaltenvektor erstellen (nicht notwendig)
beta = matrix(rep(0,p+1), ncol = 1)
beta[3] = 2

# y Daten mit Fehler erstellen
y = X %*% beta + eps

## Part 2: cv.glmnet und erste Parameter Schätzungen
# lambda bestimmen

lambda_cv <- cv.glmnet(X, y, alpha = 1)
lambda = lambda_cv$lambda.min

# jeweils beta zurückgeben lassen
beta_rre <- rre(X, y, lambda = lambda)

lasso_1 <- glmnet(X, y, lambda = lambda, intercept = FALSE)
beta_lasso <- as.vector(coef(lasso_1, s = lambda)[-1])
# (komisch dass ich hier trotzdem intercept parameter entfernen lassen muss)

beta_ls = ls(X, y)

# funktionen zum plotten definieren
ls_function <- function(x){
  value <- 0
  for (i in (0:p)){
    value <- value + x**i * beta_ls[i+1]
  }
  return(value)
}

rre_function <- function(x){
  value <- 0
  for (i in (0:p)){
    value <- value + x**i * beta_rre[i+1]
  }
  return(value)
}

lasso_function <- function(x){
  value <- 0
  for (i in (0:p)){
    value <- value + x**i * beta_lasso[i+1]
  }
  return(value)
}


## Part 3: Spaß plotten

true_function <- function(x){
  2* x**2
}

# erstmal baue ich mir eine allgemeine plot function


ggplot() +
  xlim(c(-1,1)) +
  geom_function(fun = true_function,
                colour = "red") +
  geom_point(aes(x=x, y=y), color = "blue", size = 2) +
  geom_function(fun = ls_function, color = "green") + 
  geom_function(fun = lasso_function, color = "orange") + 
  geom_function(fun = rre_function, color = "pink") +
  ggtitle("Plot 1: Einfache Ausführung")


## Part 4: Monte Carlo

# ich möchte mir einen Loop bauen, 
# in dem die jeweiligen beta in einem data frame gespeichert werden

m <- 20

beta_ls_df <- data.frame(matrix(0, nrow = p+1, ncol = m))
beta_rre_df <- data.frame(matrix(0, nrow = p+1, ncol = m))
beta_lasso_df <- data.frame(matrix(0, nrow = p+1, ncol = m))

for (i in (1:m)){
  # noise neu
  eps <- rnorm(100, mean=0, sd=1)
  y = X %*% beta + eps
  
  # betas neu erstellen und in df einfügen
  beta_rre <- rre(X, y, lambda = lambda)
  beta_rre_df[,i] <- beta_rre
  
  lasso_1 <- glmnet(X, y, lambda = lambda, intercept = FALSE)
  beta_lasso <- as.vector(coef(lasso_1, s = lambda)[-1])
  beta_lasso_df[,i] <- beta_lasso
  
  beta_ls = ls(X, y)
  beta_ls_df[,i] <- beta_ls
  
}

beta_rre_mean <- apply(beta_rre_df, 1, mean)
beta_lasso_mean <- apply(beta_lasso_df, 1, mean)
beta_ls_mean <- apply(beta_ls_df, 1, mean)


# ich bau mir eine funktion um alles einfacher zu plotten
plot_reg <- function(x, y, beta_rre, beta_lasso, beta_ls, titel = "Standardtitel"){
  ls_function <- function(x){
    value <- 0
    for (i in (0:p)){
      value <- value + x**i * beta_ls[i+1]
    }
    return(value)
  }
  
  rre_function <- function(x){
    value <- 0
    for (i in (0:p)){
      value <- value + x**i * beta_rre[i+1]
    }
    return(value)
  }
  
  lasso_function <- function(x){
    value <- 0
    for (i in (0:p)){
      value <- value + x**i * beta_lasso[i+1]
    }
    return(value)
  }
  ggplot() +
    xlim(c(-1,1)) +
    geom_function(fun = true_function,
                  colour = "red") +
    geom_point(aes(x=x, y=y), color = "blue", size = 2) +
    geom_function(fun = ls_function, color = "green") + 
    geom_function(fun = lasso_function, color = "orange") + 
    geom_function(fun = rre_function, color = "pink") +
    ggtitle(titel)
  
}

# mit mittelwerten Plotten
plot_reg(x,y, beta_rre = beta_rre_mean, beta_lasso = beta_lasso_mean, beta_ls = beta_ls_mean, titel = "Mittelwerte")

# Quantile bestimmen

rre_quantiles <- apply(beta_rre_df, 1, function(x) quantile(x, probs = c(0.25, 0.5, 0.75)))
View(rre_quantiles[1,])
lasso_quantiles <- apply(beta_lasso_df, 1, function(x) quantile(x, probs = c(0.25, 0.5, 0.75)))
ls_quantiles <- apply(beta_ls_df, 1, function(x) quantile(x, probs = c(0.25, 0.5, 0.75)))

plot_reg(x,y, beta_rre = rre_quantiles[1,], beta_lasso = lasso_quantiles[1,], beta_ls = ls_quantiles[1,], titel = "0.25 Quantil")
plot_reg(x,y, beta_rre = rre_quantiles[2,], beta_lasso = lasso_quantiles[2,], beta_ls = ls_quantiles[2,], titel = "0.5 Quantil")
plot_reg(x,y, beta_rre = rre_quantiles[3,], beta_lasso = lasso_quantiles[3,], beta_ls = ls_quantiles[3,], titel = "0.75 Quantil")

