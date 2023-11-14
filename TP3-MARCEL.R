# Exercice 1 

# question 2)

set.seed(1)
x1 <- rnorm(20)
x2 <- rnorm(20,mean=x1,sd=.01)
y <- rnorm(20,mean=3+x1+x2)
data <- data.frame(y = y,x1 = x1, x2=x2)
pairs(data)

# régression linéaire multiple 

rlm <- lm(y ~., data = data)
summary(rlm)

# régression Ridge

lm.ridge(y ~., data = data, lambda = seq(0,1,0.05))

# question 3)
set.seed(1)
n = 50
z1 <- rnorm(n)
z2 <- rnorm(n,mean = 2*z1, sd = 0.01)
w <- rnorm(n,mean= z1 + 2*z2)
data1 <- data.frame(w=w,z1=z1,z2=z2)
pairs(data1)


# coefficients de la régression linéaire multiple
data1RLM = data1
beta1 <- c()
beta2 <- c()
for (i in 1:n){
  ech <- sample(1:n,replace=T)       # échantillons boostrap
  data1RLM$w <- w[ech]
  data1RLM$z1 <- z1[ech]
  data1RLM$z2 <- z2[ech]
  rlm <- lm(w~.,data = data1RLM)
  beta1 <- c(beta1,rlm$coefficients[1])
  beta2 <- c(beta2,rlm$coefficients[2])
}
boxplot(data.frame(beta1=beta1,beta2=beta2))

# coefficients de la régression Ridge
data1RR = data1
beta1R <- c()
beta2R <- c()
for (i in 1:n){
  ech <- sample(1:n,replace=T)       # échantillons boostrap
  data1$w <- w[ech]
  data1RR$z1 <- z1[ech]
  data1RR$z2 <- z2[ech]
  rr <- lm.ridge(w~.,data = data1RR,lambda=1)
  coeff <- rr$coef/rr$scales
  beta1R <- c(beta1R,coeff[1])
  beta2R <- c(beta2R,coeff[2])
}
boxplot(data.frame(beta1R=beta1R,beta2R=beta2R))

