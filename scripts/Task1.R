#task1 multivariate statistics assignement 1


#-------------------------------------------
#1. load necessary packages
#-------------------------------------------

library(paran) #package for Horn's procedure


#-------------------------------------------
#2. load input data
#-------------------------------------------

load("data/agdata.Rdata")

#-------------------------------------------
#3. recoding variables
#-------------------------------------------

agdata$freehms <- with(agdata,max(freehms)-freehms) #recode such that max score is a tolerant view on gay/lesbian
agdata$hmsacld <- with(agdata, max(hmsacld)-hmsacld)


agdata$rlgatnd <- with(agdata, max(rlgatnd)-rlgatnd) #recode such that max score is more religious 
agdata$pray <- with(agdata, max(pray)-pray)

#-------------------------------------------
#4. standardizing variables
#-------------------------------------------

zdata <- scale(agdata[,1:16], center = TRUE, scale = TRUE)

#-------------------------------------------
#5. applying pca to standardized variables
#-------------------------------------------

agpca <- prcomp(zdata)
summary(agpca)
round(agpca$sdev^2,2)

#--------------------------------------------
#6. select number of components using procedure of Horn
#--------------------------------------------

paran(zdata,iterations=5000,graph=TRUE,cfa=FALSE,centile=95)





