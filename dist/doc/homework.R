## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----echo=TRUE----------------------------------------------------------------
n <- 1000
u <- runif(n)
x <- u^{1/3} # F(x) = x^3, 0<=x<=1

## ---- echo=TRUE---------------------------------------------------------------
hist(x, prob = TRUE)

## ---- echo=TRUE---------------------------------------------------------------
hist(x, prob = TRUE, main = expression(f(x)==3*x^2))
y <- seq(0, 1, .01)
lines(y, 3*y^2,col="red")

## ---- echo=TRUE---------------------------------------------------------------
a <-matrix(runif(5*100),100,5)
a1<-a[,1]
a2<-a[,2]
a3<-a[,3]
a4<-a[,4]
a5<-a[,5]
c<-matrix(c(5,1,-2,10,9),5,1)
b<-a%*%c
lmr <- lm(b ~ a1+a2+a3+a4+a5)
s <- summary(lmr)$coefficients
knitr::kable(s)

## ----echo=TRUE----------------------------------------------------------------
n <- 10000
sigma<-2
u <- runif(n)
x <- sqrt(-2*sigma^2*log(1-u)) # F(x) =1-exp(-x^2/(2*sigma^2)), x>=0,sigma>0
hist(x, prob = TRUE, main = expression(f(x)==x/(sigma^2)*exp(-x^2/(2*sigma^2))))
y <- seq(0, 8, .01)
lines(y, (y/(sigma^2))*exp(-y^2/(2*sigma^2)))

## ----echo=TRUE----------------------------------------------------------------
n <- 10000
sigma<-4
u <- runif(n)
x <- sqrt(-2*sigma^2*log(1-u)) # F(x) =1-exp(-x^2/(2*sigma^2)), x>=0,sigma>0
hist(x, prob = TRUE, main = expression(f(x)==x/(sigma^2)*exp(-x^2/(2*sigma^2))))
y <- seq(0, 20, .01)
lines(y, (y/(sigma^2))*exp(-y^2/(2*sigma^2)))

## ----echo=TRUE----------------------------------------------------------------
n <- 10000
sigma<-6
u <- runif(n)
x <- sqrt(-2*sigma^2*log(1-u)) # F(x) =1-exp(-x^2/(2*sigma^2)), x>=0,sigma>0
hist(x, prob = TRUE, main = expression(f(x)==x/(sigma^2)*exp(-x^2/(2*sigma^2))))
y <- seq(0, 25, .01)
lines(y, (y/(sigma^2))*exp(-y^2/(2*sigma^2)))

## ----echo=TRUE----------------------------------------------------------------
n<-1000
x1<-rnorm(n,0,1)
x2<-rnorm(n,3,1)
r<-sample(c(0,1),n,replace=TRUE,prob=c(0.25,0.75))
z<-r*x1+(1-r)*x2
hist(z)

## ----echo=TRUE----------------------------------------------------------------
n<-1000
x1<-rnorm(n,0,1)
x2<-rnorm(n,3,1)
r<-sample(c(0,1),n,replace=TRUE,prob=c(0.75,0.25))
z<-r*x1+(1-r)*x2
hist(z)

## ----echo=TRUE----------------------------------------------------------------
n<-1000
x1<-rnorm(n,0,1)
x2<-rnorm(n,3,1)
r<-sample(c(0,1),n,replace=TRUE,prob=c(0.5,0.5))
z<-r*x1+(1-r)*x2
hist(z)

## ----eval=FALSE---------------------------------------------------------------
#  n=10000
#  Nt<-rep(0,n)
#  lambda<-
#  for(i in 1:n){
#    t<-0
#    I<-0
#    while(t<=10){
#       u<-runif(1)
#       t<-t-log(u)/lambda
#       I<-I+1
#     }
#    I<-I-1
#    Nt[i]=I
#  }
#  
#  #Y Gamma(r,beta)
#  r <-
#  beta <-
#  Y <- rgamma(n, r, beta)
#  
#  #X(t)
#  Xt<-rep(0,n)
#  for(i in 1:n){
#    k=Nt[i]
#    Xt[i]=sum(Y[1:k])
#  }
#  hist(Xt)
#  

## ----echo=TRUE----------------------------------------------------------------
set.seed(123)

#lambda=2 N(t)
n=10000
Nt<-rep(0,n)
lambda<-2
for(i in 1:n){
  t<-0
  I<-0 
  while(t<=10){
     u<-runif(1)
     t<-t-log(u)/lambda
     I<-I+1
   }
  I<-I-1
  Nt[i]=I
}


r <- 4
beta <- 2
Y <- rgamma(n, r, beta)



Xt<-rep(0,n)
for(i in 1:n){
  k=Nt[i]
  Xt[i]=sum(Y[1:k])
}
hist(Xt)


mean(Xt)
var(Xt)


## ----echo=TRUE----------------------------------------------------------------
estfun1<-function(x){
  x <- x
  m <- 1000
  N <-100 
  est<-matrix(0,N,1)
  for(i in 1:N){
    u<-runif(m, min=0, max=x)
    est[i] <- mean((u^2*(1-u)^2*x)/beta(3,3))
  }
  Est<-mean(est)
  return(Est)
}

## ----echo=TRUE----------------------------------------------------------------
set.seed(123)
Est1<-matrix(0,2,9)
for(i in 1:9){
  t<-0.1*i
  Est1[1,i]<-estfun1(t)
  Est1[2,i]<-pbeta(t,3,3)
 
} 

## ----echo=TRUE----------------------------------------------------------------
fun2<-function(n,sigma){
  n <- n
  sigma<-sigma
  u1 <- runif(n/2)
  x1 <- sqrt(-2*sigma^2*log(1-u1)) 
  x11<- sqrt(-2*sigma^2*log(1-(1-u1)))
  X1<-(x1+x11)/2
  return(X1)
}

## ----echo=TRUE----------------------------------------------------------------
set.seed(111)
n <- 10000
sigma<-2
u1 <- runif(n/2)
x1 <- sqrt(-2*sigma^2*log(1-u1)) 
x11<- sqrt(-2*sigma^2*log(1-(1-u1)))
X1<-(x1+x11)/2
var(X1)

u2<- runif(n/2)
x2<- sqrt(-2*sigma^2*log(1-u2))
X2<-(x1+x2)/2
var(X2)

(var(X2)-var(X1))/var(X2)*100

## ----echo=TRUE----------------------------------------------------------------
set.seed(123)
m <- 10000
est <- sd <- numeric(2)
g <- function(x) {
  exp(-1/(2*x^2))/(sqrt(2*pi)*x^4)* (x > 0)*( x < 1 ) 
}
#f1=4/((1 + x^2)*pi), inverse transform method
u <- runif(m) 
x <- tan(pi * u / 4)
fg <- g(x) / (4 / ((1 + x^2) * pi))
est[1] <- mean(fg)
sd[1] <- sd(fg)
#f2=x^(-3)*exp(-1/(2*x^2)), inverse transform method
u <- runif(m) 
x <- sqrt(-1/(2*log(u)))
fg <- g(x) / (x^(-3)*exp(-1/(2*x^2)))
est[2] <- mean(fg)
sd[2] <- sd(fg)
sd

## ----echo=TRUE----------------------------------------------------------------
set.seed(123)
m <- 10000
g <- function(x) {
  exp(-1/(2*x^2))/(sqrt(2*pi)*x^4)* (x > 0)*( x < 1 ) 
}
#f=x^(-3)*exp(-1/(2*x^2)), inverse transform method
u <- runif(m) 
x <- sqrt(-1/(2*log(u)))
fg <- g(x) / (x^(-3)*exp(-1/(2*x^2)))
est<- mean(fg)
est

## ----echo=TRUE----------------------------------------------------------------
set.seed(111)
m<-10000
n<-20
alpha<-0.05
mu1<-2 
p11<-numeric(m)
for(i in 1:m){
 x<-rchisq(n,2)
 c1<-mean(x)+qt(alpha/2,n-1)*sd(x)*n^(-1/2)
 c2<-mean(x)-qt(alpha/2,n-1)*sd(x)*n^(-1/2)
 p11[i]<-as.numeric(c1<=mu1&&mu1<=c2)
}
cp11<-mean(p11)

sigma1<-4
p12<-numeric(m)
for(i in 1:m){
 x<-rchisq(n,2)
 c1<-0
 c2<-(n-1)*(sd(x))^2/qchisq(alpha,n-1)
 p12[i]<-as.numeric(c1<=sigma1&&sigma1<=c2)
}
cp12<-mean(p12)



m<-10000
n<-20
alpha<-0.05
mu2<-0
p21<-numeric(m)
for(i in 1:m){
 x<-rnorm(n,0,2)
 c1<-mean(x)+qt(alpha/2,n-1)*sd(x)*n^(-1/2)
 c2<-mean(x)-qt(alpha/2,n-1)*sd(x)*n^(-1/2)
 p21[i]<-as.numeric(c1<=mu2&&mu2<=c2)
}
cp21<-mean(p21)

sigma2<-4  
p22<-numeric(m)
for(i in 1:m){
 x<-rnorm(n,0,2)
 c1<-0
 c2<-(n-1)*(sd(x))^2/qchisq(alpha,n-1)
 p22[i]<-as.numeric(c1<=sigma2&&sigma2<=c2)
}
cp22<-mean(p22)

print(c(cp11,cp21,cp12,cp22))


## ----echo=TRUE----------------------------------------------------------------
set.seed(123)

m<-1000
n<-20
alpha<-0.05
mu1<-1
p1<-numeric(m)
for(i in 1:m){
 x<-rchisq(n,1)
 ttest <- t.test(x, alternative = "two.sided", mu = mu1)
 p1[i]<-ttest$p.value
}
cp1<-mean(p1 < alpha)


m<-1000
n<-20
alpha<-0.05
mu2<-1 
p2<-numeric(m)
for(i in 1:m){
 x<-runif(n,0,2)
 ttest <- t.test(x, alternative = "two.sided", mu = mu2)
 p2[i]<-ttest$p.value
}
cp2<-mean(p2 < alpha)


m<-1000
n<-20
alpha<-0.05
mu3<-1
p3<-numeric(m)
for(i in 1:m){
 x<-rexp(n,1)
 ttest <- t.test(x, alternative = "two.sided", mu = mu3)
 p3[i]<-ttest$p.value
}
cp3<-mean(p3 < alpha)

print(c(cp1,cp2,cp3))


## ----echo=TRUE----------------------------------------------------------------
MMST<-function(X){
  d<-dim(X)[1]
  n<-dim(X)[2]
  X_bar<-matrix(rowMeans(X),d,1)
  centralX=X-matrix(rep(X_bar,n),d,n)
  S0<-matrix(0,d,d)
  for(i in 1:n){
    S0<-S0+centralX[,i]%*%t(centralX[,i])
}
  sigma_hat<-S0/n
  b0<-0
  for(i in 1:n){
    for(j in 1:n){
        b0<-b0+(t(centralX[,i])%*%ginv(sigma_hat)%*%centralX[,j])^3
    }
  }
  b<-b0/(n^2)
  T<-n*b/6
  chi<-qchisq(0.95,d*(d+1)*(d+2)/6)
  as.integer(T>chi)
}



## ----eval=FALSE---------------------------------------------------------------
#  library(MASS)
#  set.seed(666)
#  d<-3
#  m<-1000
#  p<-rep(0,m)
#  mu<-c(0,0,0)
#  sigma<-matrix(c(1,0,0,0,1,0,0,0,1),3,3)
#  
#  n1<-10
#  for(i in 1:m){
#    X<-mvrnorm(n1,mu,sigma)
#    X<-t(X)
#    p[i]<-MMST(X)
#  }
#  t1e1<-sum(p)/m
#  
#  n2<-20
#  for(i in 1:m){
#    X<-mvrnorm(n2,mu,sigma)
#    X<-t(X)
#    p[i]<-MMST(X)
#  }
#  t1e2<-sum(p)/m
#  
#  n3<-30
#  for(i in 1:m){
#    X<-mvrnorm(n3,mu,sigma)
#    X<-t(X)
#    p[i]<-MMST(X)
#  }
#  t1e3<-sum(p)/m
#  
#  n4<-50
#  for(i in 1:m){
#    X<-mvrnorm(n4,mu,sigma)
#    X<-t(X)
#    p[i]<-MMST(X)
#  }
#  t1e4<-sum(p)/m
#  
#  n5<-100
#  for(i in 1:m){
#    X<-mvrnorm(n5,mu,sigma)
#    X<-t(X)
#    p[i]<-MMST(X)
#  }
#  t1e5<-sum(p)/m
#  
#  
#  m<-100
#  n6<-500
#  for(i in 1:m){
#    X<-mvrnorm(n6,mu,sigma)
#    X<-t(X)
#    p[i]<-MMST(X)
#  }
#  t1e6<-sum(p)/m
#  
#  print(c(t1e1,t1e2,t1e3,t1e4,t1e5,t1e6))
#  

## ----eval=FALSE---------------------------------------------------------------
#  set.seed(1239)
#  a <-0.1
#  n <- 30
#  m <- 100
#  k<-3
#  eps <- c(seq(0,0.15,0.01), seq(0.15,1,0.05))
#  N <- length(eps)
#  power <- numeric(N)
#  mu1<-rep(0,3)
#  s1<-matrix(c(1,0,0,0,1,0,0,0,1),3,3)
#  mu2<-rep(0,3)
#  s2<-matrix(c(100,0,0,0,100,0,0,0,100),3,3)
#  
#  for (j in 1:N) {
#    e <- eps[j]
#    p <- numeric(m)
#  
#    for (i in 1:m) {
#  
#      r<-sample(c(0,1),n,replace=TRUE,prob=c(e,1-e))
#      z<-matrix(0,k,n)
#      for(d in 1:n){
#        x1<-MASS::mvrnorm(1,mu1,s1)
#        x2<-MASS::mvrnorm(1,mu2,s2)
#        z[,d]<-r[d]*x1+(1-r[d])*x2
#      }
#      p[i]<-MMST(z)
#    }
#    power[j] <-sum(p)/m
#  }
#  
#  plot(eps, power, type = "b",ylim = c(0,1))
#  abline(h = 0.05, lty = 2)
#  s <- (power*(1-power) / m)^(1/2)
#  lines(eps, power+s, lty = 2)
#  lines(eps, power-s, lty = 2)
#  

## ----echo=TRUE----------------------------------------------------------------
library(bootstrap)
data<-scor
covscor<-cov(data)
l<-eigen(covscor)$values
theta.hat<-l[1]/sum(l)

B <- 2000 #larger for estimating bias
n <- nrow(data)
theta.b <- numeric(B)
for (b in 1:B) {
  j <- sample(1:n, size = n, replace = TRUE)
  dataj<-data[j,]
  covscorj<-cov(dataj)
  lj<-eigen(covscorj)$values
  theta.b[b]<-lj[1]/sum(lj)
}
bias1 <- mean(theta.b) - theta.hat
se1<-sd(theta.b)
print(c(bias1,se1))

## ----echo=TRUE----------------------------------------------------------------
theta.jack <- numeric(n)
for (i in 1:n){
  datai<-data[-i,]
  covscori<-cov(datai)
  li<-eigen(covscori)$values
  theta.jack[i]<-li[1]/sum(li)
}
bias2 <- (n - 1) * (mean(theta.jack) - theta.hat)
se2 <- sqrt((n-1) *mean((theta.jack - mean(theta.jack))^2))
print(c(bias2,se2))


## ----echo=TRUE----------------------------------------------------------------
library(boot)
B<-2000
bootfun <- function(data,id){
  x <- data[id,]
  l <- eigen(cov(x))$values
  theta.hat <- l[1] / sum(l)
  return(theta.hat)
}
bootresult <- boot(data = cbind(scor$mec, scor$vec, scor$alg, scor$ana, scor$sta),statistic = bootfun, R = B)
boot.ci(bootresult, conf = 0.95, type = c("perc"," bca"))

## ----echo=TRUE----------------------------------------------------------------
library(boot)
set.seed(12345)
B<-1000 
n<-100 
skfun <- function(x,i){
  xbar<-mean(x[i])
  s1<-mean((x[i]-xbar)^3)
  s2<-mean((x[i]-xbar)^2)
  s<-s1/(s2^1.5)
  return(s)
}

sk1<-0 
norm1<-basic1<-perc1<-matrix(0,B,2)
for(b in 1:B){
  X1<-rnorm(n,0,1)
  result <- boot(data=X1,statistic=skfun, R=1000)
  C<- boot.ci(result,type=c("norm","basic","perc"))
  norm1[b,]<-C$norm[2:3]
  basic1[b,]<-C$basic[4:5]
  perc1[b,]<-C$percent[4:5]
}
#CP
cat('norm =',mean(norm1[,1]<=sk1 & norm1[,2]>=sk1),
    'basic =',mean(basic1[,1]<=sk1 & basic1[,2]>=sk1),
    'perc =',mean(perc1[,1]<=sk1 & perc1[,2]>=sk1))
#missing on th left
cat('norm =',mean(norm1[,1]>=sk1),
    'basic =',mean(basic1[,1]>=sk1),
    'perc =',mean(perc1[,1]>=sk1))
#missing on th right
cat('norm =',mean(norm1[,2]<=sk1),
    'basic =',mean(basic1[,2]<=sk1),
    'perc =',mean(perc1[,2]<=sk1))

e <- rchisq(1000,5) 
sk2<-mean(((e-mean(e))/sd(e))^3) 
norm2<-basic2<-perc2<-matrix(0,B,2)
for(b in 1:B){
  X2<-rchisq(n,5)
  result <- boot(data=X2,statistic=skfun, R=1000)
  C<- boot.ci(result,type=c("norm","basic","perc"))
  norm2[b,]<-C$norm[2:3]
  basic2[b,]<-C$basic[4:5]
  perc2[b,]<-C$percent[4:5]
}
#CP
cat('norm =',mean(norm2[,1]<=sk2 & norm2[,2]>=sk2),
    'basic =',mean(basic2[,1]<=sk2 & basic2[,2]>=sk2),
    'perc =',mean(perc2[,1]<=sk2 & perc2[,2]>=sk2))
#missing on th left
cat('norm =',mean(norm2[,1]>=sk2),
    'basic =',mean(basic2[,1]>=sk2),
    'perc =',mean(perc2[,1]>=sk2))
#missing on th right
cat('norm =',mean(norm2[,2]<=sk2),
    'basic =',mean(basic2[,2]<=sk2),
    'perc =',mean(perc2[,2]<=sk2))

## ----echo=TRUE----------------------------------------------------------------
library(energy)
library(Ball)
library(MASS)
library(RANN)
library(boot)

Spearcor <- function(x, y) {
  x=as.matrix(x);y=as.matrix(y);n=nrow(x); m=nrow(y)
  cor(x,y,method="spearman")
}
Spcor <- function(z, ix, dims) {
  #dims contains dimensions of x and y
  n1 <- dims[1]
  n2 <- dims[2]
  n <- n1 + n2
  x <- z[ , 1:n1] # LEAVE x as is
  y <- z[ix, -(1:n1)] # PERMUTE rows of y
  return(Spearcor(x,y))
}
set.seed(7532)
z <- as.matrix(iris[1:50, 1:4]) 
boot.obj <- boot(data = z, statistic = Spcor, R = 1000,
                 sim = "permutation", dims = c(2, 2))
tb <- c(boot.obj$t0, boot.obj$t)
pcor <- mean(tb>=tb[1])
cortestp<-cor.test(z[1:50,1:2],z[1:50,3:4],method = "pearson")
pcor
cortestp

## ----echo=TRUE----------------------------------------------------------------
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1)
  block1 <- NN$nn.idx[1:n1,-1]
  block2 <- NN$nn.idx[(n1+1):n,-1]
  i1 <- sum(block1 < n1 + 0.5); i2 <- sum(block2 > n1+0.5)
  (i1 + i2) / (k * n)
}
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
                   sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}

#1
m <- 1000
k<-3
d<-3
set.seed(8675)
n1 <- 15
n2 <- 15
n <- n1+n2
N = c(n1,n2)
R<-1000
m11<-rep(1,d)
s11<-matrix(c(1,0,0,0,1,0,0,0,1),d,d)
m12<-rep(1,d)
s12<-matrix(c(4,0,0,0,4,0,0,0,3),d,d)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x<-mvrnorm(n1,m11,s11)
  y<-mvrnorm(n2,m12,s12)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=R,seed=i*2468)$p.value
}
alpha <- 0.05
pow1 <- colMeans(p.values<alpha) 
pow1

## ----echo=TRUE----------------------------------------------------------------
#2
m <- 100
k<-3
d<-3
set.seed(7648)
n1 <- 15
n2 <- 15
n <- n1+n2
N = c(n1,n2)
R<-1000
m21<-rep(0,d)
s21<-matrix(c(1,0,0,0,1,0,0,0,1),d,d)
m22<-c(1,1/2,1/3)#rep(2,d)
s22<-matrix(c(2,0,0,0,2,0,0,0,2),d,d)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x<-mvrnorm(n1,m21,s21)
  y<-mvrnorm(n2,m22,s22)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=R,seed=i*2346)$p.value
}
alpha <- 0.05
pow2 <- colMeans(p.values<alpha) 
pow2

## ----echo=TRUE----------------------------------------------------------------
#3
m <- 1000
k<-3
set.seed(1234)
n1 <- 30
n2 <- 30
n <- n1+n2
N = c(n1,n2)
R<-100
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x<-matrix(rt(n1,1),n1,1)
  y321<-rnorm(n2,0,1)
  y322<-rnorm(n2,6,1)
  r<-sample(c(0,1),n2,replace=TRUE,prob=c(0.3,0.7))
  y<-matrix(r*y321+(1-r)*y322)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=R,seed=i*8674)$p.value
}
alpha <- 0.05
pow3 <- colMeans(p.values<alpha) 
pow3

## ----echo=TRUE----------------------------------------------------------------
#4
m <- 100
k<-3
d<-3
set.seed(1111)
n1 <- 12
n2 <- 120
n <- n1+n2
N = c(n1,n2)
R<-1000
m41<-rep(0.5,d)
s41<-matrix(c(1,0,0,0,1,0,0,0,1),d,d)
m42<-rep(1,d)
s42<-matrix(c(1,0,0,0,1,0,0,0,2),d,d)
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x<-mvrnorm(n1,m41,s41)
  y<-mvrnorm(n2,m42,s42)
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=R,seed=i*8367)$p.value
}
alpha <- 0.05
pow4 <- colMeans(p.values<alpha)
pow4

## ----echo=TRUE----------------------------------------------------------------
set.seed(9966)
f <- function(x, theta) {
  yita<-0
  stopifnot(theta > 0)
  y0<-theta*pi*(1+((x-yita)/theta)^2)
  return(1/y0)
}

m <- 10000
theta <- 1
sigma<-4
x <- numeric(m)
x[1] <- -10
k <- 0
u <- runif(m)

for (i in 2:m) {
  xt <- x[i-1]
  y <- rnorm(1, xt, sigma) 
  z1 <- f(y,theta) * dnorm(xt, y, sigma)
  z2 <- f(xt,theta) * dnorm(y, xt, sigma)
  if (u[i] <= z1/z2){
    x[i] <- y
  } else {
    x[i] <- xt
    k <- k+1     #y is rejected
  }
}

index <- 1001:m
y <- x[index]
ysfw<-quantile(y,seq(0.1,1,0.1))
ytsfw<-qt(seq(0.1,1,0.1),1)
ysfw
ytsfw

## ----echo=TRUE----------------------------------------------------------------
N <- 5000 
cut <- 1000 
X <- matrix(0, N, 2) 
a<-2
b<-3
n<-10

x10<-5
x20<-runif(1,0,1)
X[1, ] <- c(x10, x20)
for (i in 2:N) {
  x2 <- X[i-1, 2]
  X[i, 1] <- rbinom(1, n, x2)
  x1 <- X[i, 1]
  X[i, 2] <- rbeta(1,x1+a,n-x1+b)
}
c <- cut + 1
x <- X[c:N, ]
cat('Means: ',round(colMeans(x),2))
cat('Standard errors: ',round(apply(x,2,sd),2))
cat('Correlation coefficients: ', round(cor(x[,1],x[,2]),2))



## ----echo=TRUE----------------------------------------------------------------
gr <- function(psi) {
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  psi.means <- rowMeans(psi)     
  B <- n * var(psi.means)        
  psi.w <- apply(psi, 1, "var")  
  W <- mean(psi.w)               
  v.hat <- W*(n-1)/n + (B/n)     
  r.hat <- v.hat / W             
  return(r.hat)
}

chain <- function(sigma, N, X1) {
  x <- rep(0, N)
  x[1] <- X1
  u <- runif(N)
  for (i in 2:N) {
    xt <- x[i-1]
    y <- rnorm(1, xt, sigma) 
    z1 <- f(y,theta) * dnorm(xt, y, sigma)
    z2 <- f(xt,theta) * dnorm(y, xt, sigma)
    if (u[i] <= z1/z2){
      x[i] <- y
    } else {
      x[i] <- xt
      k <- k+1    
    }
  }
  return(x)
}

sigma <- 0.5
k <- 4          
m <- 10000   
b <- 1000   
x0 <- c(-10, -5, 5, 10)

set.seed(12345)
X <- matrix(0, nrow=k, ncol=m)
for (i in 1:k){
  X[i, ] <- chain(sigma, m, x0[i])
}
  
psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi)){
  psi[i,] <- psi[i,] / (1:ncol(psi))
}
  
rhat <- rep(0, m)
for (j in (b+1):m)
  rhat[j] <- gr(psi[,1:j])
plot(rhat[(b+1):m], type="l", xlab="", ylab="R")
abline(h=1.1, lty=2)

## ----echo=TRUE----------------------------------------------------------------
chain2 <- function(a,b,n,N, X1) {
  x10<-X1
  x20<-runif(1,0,1)
  X<-matrix(0, N, 2) 
  X[1, ] <- c(x10, x20) #initialize
  for (i in 2:N) {
    x2 <- X[i-1, 2]
    X[i, 1] <- rbinom(1, n, x2)
    x1 <- X[i, 1]
    X[i, 2] <- rbeta(1,x1+a,n-x1+b)
  }
  return(X)
}

N <- 5000 
cut <- 1000 
X <- matrix(0, N, 2) 
a<-2
b<-3
n<-10
k<-4
X10 <- c(-6, -5, 5, 6)


set.seed(12345)
X <- matrix(0, nrow=2*k, ncol=N)
for (i in 1:k)
  X[((2*i-1):(2*i)), ] <- t(chain2(a,b,n, N, X10[i]))

X<-X[c(2,4,6,8),]


psi <- t(apply(X, 1, cumsum))
for (i in 1:nrow(psi))
  psi[i,] <- psi[i,] / (1:ncol(psi))


#plot the sequence of R-hat statistics
rhat <- rep(0, n)
for (j in (b+1):n)
  rhat[j] <- gr(psi[,1:j])
plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
abline(h=1.1, lty=2)

## ----echo=TRUE----------------------------------------------------------------
yk<-function(a,k,d){
  if(k==0){
    y<-exp(log(sum(a*a))-log(2)+lgamma((d+1)/2)+lgamma(k+3/2)-lgamma(k+d/2+1))
  }else{
    y0<-0
    for(i in 1:k){
      y0<-y0+log(i)
    }
    y<-(-1)^k*exp((k+1)*log(sum(a*a))+lgamma((d+1)/2)+lgamma(k+3/2)-y0-k*log(2)-log(2*k+1)-log(2*k+2)-lgamma(k+d/2+1))
  }
  return(y)
}


## ----echo=TRUE----------------------------------------------------------------
sumyk<-function(a){
  d<-length(a)
  k0<-10000
  try<-yk(a,k0,d)
  if(try==0){
    k<-k0
  }else{
    k<-k0*10
  }
  s<-0
  for(i in 0:k){
    s<-s+yk(a,i,d)
  }
  return(s)
}

a<-c(1,2)
d<-length(a)
yy<-sumyk(a)
print(yy)


## ----echo=TRUE----------------------------------------------------------------
f1<-function(u,k){
  (1+u^2/(k-1))^(-k/2)
}
f2<-function(u,k){
  (1+u^2/k)^(-(k+1)/2)
}
ff<-function(a,k0){
  k<-k0
  ck0<-((a^2*(k-1))/(k-a^2))^(1/2)
  ck1<-((a^2*k)/(k+1-a^2))^(1/2)
  cc1<-exp(log(2)+lgamma(k/2)-(1/2)*log(pi)-(1/2)*log(k-1)-lgamma((k-1)/2))
  cc2<-exp(log(2)+lgamma(k/2+1/2)-(1/2)*log(pi)-(1/2)*log(k)-lgamma(k/2))
  y01<-integrate(f1,lower=0,upper = ck0, k=k)$value
  y02<-integrate(f2,lower=0,upper = ck1, k=k)$value
  y1<-cc1*y01
  y2<-cc2*y02
  cha<-y1-y2
  return(cha)
}
Fun<-function(k1){
  k<-k1
  a1<-0+0.1
  a2<-2
  ffa1<-ff(a1,k)
  ffa2<-ff(a2,k)
  ffa<-1
  tt<-0
  while(ffa>1e-10){
    med<-(a1+a2)/2
    ffmed<-ff(med,k)
    if(ffa1*ffmed<0){
      a2<-med
    }else{
      a1<-med
    } 
    ffa1<-ff(a1,k)
    ffa2<-ff(a2,k)
    a<-med
    ffa<-abs(ff(a,k))
    tt<-tt+1
  }
  return(a)
}



## ----echo=TRUE----------------------------------------------------------------
c<-c(c(4:25),100,500,1000)
A<-matrix(0,2,length(c))
A[1,]<-c
for(i in 1:length(c)){
  A[2,i]<-Fun(c[i])
}
A

## ----echo=TRUE----------------------------------------------------------------
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)
lapply(trims, function(trim) mean(x, trim = trim))
lapply(trims, mean, x = x)

## ----echo=TRUE----------------------------------------------------------------
formulas <-list(
  mpg ~ disp,
  mpg ~I(1/ disp),
  mpg ~ disp + wt,
  mpg ~I(1/ disp) + wt
) 
for(i in 1:4){
  lm(formulas[[i]],mtcars)
}
Mod1<-lapply(formulas,lm,data=mtcars)
rsq <- function(mod)summary(mod)$r.squared
lapply(Mod1,rsq)



## ----echo=TRUE----------------------------------------------------------------

bootstraps <-lapply(1:10, function(i) {
  rows <-sample(1:nrow(mtcars),rep =TRUE)
  mtcars[rows, ]
})
Mod2<-lapply(bootstraps,lm,mtcars)
rsq <- function(mod)summary(mod)$r.squared
lapply(Mod2,rsq)


## ----echo=TRUE----------------------------------------------------------------
x1<-data.frame(rnorm(20),runif(20))
vapply(x1,sd,numeric(1))

## ----echo=TRUE----------------------------------------------------------------
x2 <- list(a = c(1:10), beta = exp(-3:3),ogic = c(TRUE,FALSE,FALSE,TRUE))
vapply(x2,sd,numeric(1))


## ----eval=FALSE---------------------------------------------------------------
#  library(parallel)
#  boot_df <- function(x) x[sample(nrow(x),rep =T), ]
#  rsquared <- function(mod)summary(mod)$r.square
#  boot_lm <- function(i) {
#    rsquared(lm(mpg ~ wt + disp,data =boot_df(mtcars)))
#  }
#  system.time(mclapply(1:500, boot_lm,mc.cores =2))
#  

## ----eval=FALSE---------------------------------------------------------------
#  mcvapply<-function (X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
#            mc.silent = FALSE, mc.cores = 1L, mc.cleanup = TRUE, mc.allow.recursive = TRUE,
#            affinity.list = NULL)
#  {
#    cores <- as.integer(mc.cores)
#    if (cores < 1L)
#      stop("'mc.cores' must be >= 1")
#    if (cores > 1L)
#      stop("'mc.cores' > 1 is not supported on Windows")
#    vapply(X, FUN, ...)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  #include <Rcpp.h>
#  #using namespace Rcpp;
#  
#  #// [[Rcpp::export]]
#  #NumericMatrix fun98( int a,int b,int N) {
#  #  NumericMatrix x(N, 2);
#  #  double x1 = 0, x2 = 0;
#  #  double t1 = 0, t2 = 0;
#  
#  #  for(int i = 1; i < N; i++) {
#  #    x2=x(i-1,1);
#  #    t1=rbinom(1,25,x2)[0];
#  #    x(i,0)=t1;
#  #    x1=x(i,0);
#  #    t2=rbeta(1,x1+a,25-x1+b)[0];
#  #    x(i,1)=t2;
#  #  }
#  #  return(x);
#  #}

## ----echo=TRUE----------------------------------------------------------------
#library(Rcpp)
#dir_cpp <- 'D:/Rcpp/'
# Can create source file in Rstudio
#sourceCpp(paste0(dir_cpp,"1cpp.cpp"))
#a <- 1
#b <- 1
#N <- 10000  


#X1<-fun98(a,b,N)
#plot(X1[,1],X1[,2],xlab = "x",ylab = "y")


## ----echo=TRUE----------------------------------------------------------------
#a <- 1
#b <- 1
#N <- 10000  


#X <- matrix(0, N, 2)
#X[1,] <- c(0,0.5)
#for(i in 2:N){
#  X2 <-  X[i-1, 2]
#  X[i,1] <- rbinom(1,25,X2)
#  X1 <- X[i,1]
#  X[i,2] <- rbeta(1,X1+a,25-X1+b)
#}
#plot(X[,1],X[,2],xlab = "x",ylab = "y")


## ----echo=TRUE----------------------------------------------------------------
#betasjs<-function(n,a,b){
#  X <- rchisq(n, 2 * a, ncp = 0)
#  sjs<-X/(X + rchisq(n, 2 * b))
#  return(sjs)
#}
#n<-500
#a<-2
#b<-3
#x1<-betasjs(n,a,b)
#x2<-rbeta(n,a,b)
#qqplot(x1,x2)


## ----echo=TRUE----------------------------------------------------------------
#library(microbenchmark)
#n<-500
#a<-2
#b<-3
#ts <- microbenchmark(x1=betasjs(n,a,b),x2=rbeta(n,a,b))
#summary(ts)[,c(1,3,5,6)]


