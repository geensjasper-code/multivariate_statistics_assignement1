library(lavaan)

ndata_cfa <- ndata

ndata_cfa$hmsfmlsh <- 6 - ndata_cfa$hmsfmlsh
ndata_cfa$rlgatnd <- 8 - ndata_cfa$rlgatnd
ndata_cfa$pray <- 8 - ndata_cfa$pray

#CFA model with 5 correlated latent variables

cfa1 <- '
TRUST =~ 1*ppltrst+pplfair+pplhlp
NLGB =~ 1*freehms+hmsfmlsh+hmsacld
IMMIGR =~ 1*imbgeco+imueclt+imwbcnt
RELIG =~ 1*rlgdgr+rlgatnd+pray
GENDER =~ 1*eqwrkbg+eqpolbg+eqmgmbg+eqpaybg

TRUST ~~ TRUST
NLGB ~~ NLGB
IMMIGR ~~ IMMIGR
RELIG ~~ RELIG
GENDER ~~ GENDER

TRUST ~~ NLGB + IMMIGR + RELIG + GENDER
NLGB ~~ IMMIGR + RELIG + GENDER
IMMIGR ~~ RELIG + GENDER
RELIG ~~ GENDER
'

#Covariance matrix
fitcfa1 <- cfa(cfa1, data=ndata_cfa)

#Summary
summary(fitcfa1,fit.measures=TRUE)

#Print fit measures
fitm1 <- fitmeasures(fitcfa1,
                     c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))

fitm1

#Standardized solution
standardizedSolution(fitcfa1)

#Composite reliability
#Formula was taken from the course slides and created in R with AI.
d <- standardizedSolution(fitcfa1)

compositereliability <- function(x){
  A <- (sum(x))^2
  B <- sum(1-x^2)
  return(A/(A+B))
}


#Composite reliability - Social trust
compositereliability(d[1:3,4])

#Composite reliability - Negative attitude towards gays/lesbians
compositereliability(d[4:6,4])

#Composite reliability - Positive attitude towards immigrants
compositereliability(d[7:9,4])

#Composite reliability - Religiousness
compositereliability(d[10:12,4])

#Composite reliability - Positive attitude towards gender equality
compositereliability(d[13:16,4])
