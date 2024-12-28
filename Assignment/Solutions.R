#Q1
N=8
p=3
x1<-c(30,90,-10,10,30,60,0,40)
x2<-c(-8,7,-2,0,-2,0,-2,1)
x3<-c(-1,6,4,2,5,3,4,2)

X<-matrix(c(x1,x2,x3),nrow=3,byrow=T)

X_hat<-rowMeans(X)

Y<-X-X_hat
A<-matrix(0,p,p)
for(i in 1:N-1)
{
    A<-A+(Y[,i]%*%t(Y[,i]))
    print(A)
}
S<-A/(N-1)
#A<-Y%*%t(Y)
A_1<-solve(A)
A_1

T_2<-(N-1)*N*(t(X_hat)%*%A_1%*%X_hat)

T_2

T0_2<-((N-1)/(N-p))*qf(.95, df1=3, df2=5)

T_2>=T0_2
#reject

c2<-(N-1)*p/(N-p)
CR<-matrix(0,3,1)
for(i in 1:p)
{
  CR[i,1]<-c2*sqrt(S[i,i]/(N-1))
}
CR

xs<-runif(1000,X_hat[3]-CR[3],X_hat[3]+CR[3])
ys<-runif(1000,X_hat[2]-CR[2]*sqrt(1-(xs^2)/(CR[3]^2)),X_hat[2]+CR[2]*sqrt(1-(xs^2)/(CR[3]^2)))

z<-runif(100,-50,50)

hist(ys)
hist(xs)
plot(z)
points(x=xs,y=ys,col='red')

x_l<-X_hat[1,]-T0_2*sqrt(S[1,1]/(N-1))
,
X_hat-T0_2*sqrt(S[/(N-1))

#Example

data(iris)

X<-iris
X<-as.matrix(X[,-5])


N1<-50
N2<-50
p<-4

X1<-X[sample(nrow(X), 50), ]
X2<-X[sample(nrow(X), 50), ]


X1_hat<-colMeans(X1)

Y1<-X1-X1_hat
A1<-matrix(0,p,p)
for(i in 2:N1-1)
{
  print(i)
  A1<-A1+((Y1[i,])%*%t(Y1[i,]))
}
X2_hat<-colMeans(X2)

Y2<-X2-X2_hat
A2<-matrix(0,p,p)
for(i in 2:N2-1)
{
  A2<-A2+(Y2[i,]%*%t(Y2[i,]))
}

S<-(A1+A2)/(N1+N2-2)

S_inv<-solve(S)

T2<-((N1*N2)/(N1+N2))*(t(X1_hat-X2_hat)%*%S_inv%*%(X1_hat-X2_hat))
T2
T0_2<-((N1+N2-2)*p/(N1+N2-p-1))*qf(0.99,p,N1+N2-p-1)
