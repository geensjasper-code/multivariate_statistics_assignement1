#task2 multivariate statistics assignement 1

#Data Exploration 
View(ndata)
anyNA(ndata)
dim(ndata)
str(ndata)

#Question 1: Explanatory Factor Analysis 

#Are variables standardized 
ndata_num <- subset(ndata,select=-cntry) #remove country from data set 
colMeans(ndata_num) #means are not 1, no standardized variables 

#Fixing the orientation 
ndata_num$hmsfmlsh <- 6 - ndata_num$hmsfmlsh
ndata_num$rlgatnd <- 8 - ndata_num$rlgatnd
ndata_num$pray <- 8 - ndata_num$pray 

#Standardizing Variables 
ndata_std <- scale(ndata_num[,1:16])

#Oblique Rotation 
install.packages("psych")
library(psych)
EFA <- fa(ndata_std,nfactors=5,rotate="oblimin",fm="mle")
print(EFA,digits=3,cutoff=0)

#Estimation of observed correlation matrix 
R <- cor(ndata_std) #observed correlation matrix
R_est <- EFA$model #estimated correlation matrix 
R_res <- R - R_est;R_res #residual correlation matrix 


