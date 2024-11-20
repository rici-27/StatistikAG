#setwd("C:/Users/Anwender/Desktop/Statistik AG/Blatt2")
#setwd("~/Desktop/Statistik_AG/Blatt2")

library(DT)
library(tidyverse)
library(glmnet)


## Teil a) Ridge Regression Estimator

rre <- function(X, y, lambda=1){
  error <- function(w){
    return(crossprod(y- X %*% w) + lambda * sqrt(sum(w^2)))
  }
  result <- optim(par =rep(0,ncol(X)), fn = error)
  result$par
}


# Def des LSE zum Vergleich
ls<-function(X, y){
  lseq <- function(w, X, y) {
    crossprod(y - X%*%w)
  }
  result=optim(rep(0, ncol(X)), lseq, X=X, y=y)
  result$par
}


library(glmnet)
?glmnet


## Teil c) OVerfitting Problem for Least Squares
## Part 1: Daten erzeugen

set.seed(27); x <- runif(100, min=-1, max=1)
?runif
set.seed(27); eps <- rnorm(100, mean=0, sd=1)

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

print(X%*% beta)
# y Daten mit Fehler erstellen
y = X %*% beta + eps

## Part 2: cv.glmnet und erste Parameter Schätzungen
# lambda bestimmen

set.seed(27); lambda_cv <- cv.glmnet(X, y, alpha = 1)
lambda = lambda_cv$lambda.min

# jeweils beta zurückgeben lassen
beta_rre <- rre(X, y, lambda = lambda)

lasso_1 <- glmnet(X, y, lambda = lambda)
beta_lasso <- as.vector(lasso_1$beta)
# der stimmt mit daniel überein

beta_ls = ls(X,y)

############## ridge und least squares stimmt nicht überein bin so verwirrt

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


ggplot() +
  xlim(c(-1.1,1.1)) +
  geom_function(aes(color = "Wahre Funktion"), fun = true_function) +
  geom_point(aes(x=x, y=y, color = "Daten"), size = 2) +
  geom_function(aes(color = "Least Squares"), fun = ls_function) + 
  geom_function(aes(color = "Lasso"), fun = lasso_function) + 
  geom_function(aes(color = "Ridge"), fun = rre_function) +
  ggtitle("Plot 1: Einfache Ausführung") +
  scale_color_manual(
    values = c("Daten" = "blue",
               "Wahre Funktion" = "red",
               "Least Squares" = "green", 
               "Lasso" = "orange", 
               "Ridge" = "pink"),
    name = "Legende"
  ) + 
  theme_minimal()


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
    xlim(c(-1.1,1.1)) +
    geom_function(aes(color = "Wahre Funktion"), fun = true_function) +
    geom_point(aes(x=x, y=y, color = "Daten"), size = 2) +
    geom_function(aes(color = "Least Squares"), fun = ls_function) + 
    geom_function(aes(color = "Lasso"), fun = lasso_function) + 
    geom_function(aes(color = "Ridge"), fun = rre_function) +
    ggtitle(titel) +
    scale_color_manual(
      values = c("Wahre Funktion" = "red",
                 "Daten" = "blue",
                 "Least Squares" = "green", 
                 "Lasso" = "orange", 
                 "Ridge" = "pink"),
      name = "Legende"
    ) + 
    theme_minimal()


}

# mit mittelwerten Plotten
plot_reg(x,y, beta_rre = beta_rre_mean, beta_lasso = beta_lasso_mean, beta_ls = beta_ls_mean, titel = "Mittelwerte")

# Quantile bestimmen
### andere Variante: Quantile der Funktionswerte und nicht der beta-Vektoren nehmen !

rre_quantiles <- apply(beta_rre_df, 1, function(x) quantile(x, probs = c(0.25, 0.5, 0.75)))
View(rre_quantiles[1,])
lasso_quantiles <- apply(beta_lasso_df, 1, function(x) quantile(x, probs = c(0.25, 0.5, 0.75)))
ls_quantiles <- apply(beta_ls_df, 1, function(x) quantile(x, probs = c(0.25, 0.5, 0.75)))

plot_reg(x,y, beta_rre = rre_quantiles[1,], beta_lasso = lasso_quantiles[1,], beta_ls = ls_quantiles[1,], titel = "0.25 Quantil")
plot_reg(x,y, beta_rre = rre_quantiles[2,], beta_lasso = lasso_quantiles[2,], beta_ls = ls_quantiles[2,], titel = "0.5 Quantil")
plot_reg(x,y, beta_rre = rre_quantiles[3,], beta_lasso = lasso_quantiles[3,], beta_ls = ls_quantiles[3,], titel = "0.75 Quantil")

