###---Task 2c---###
#I'm following on task 2b with everything

sem0 = cfa1 <- '
#MEasurement MOdel
TRUST =~ 1*ppltrst+pplfair+pplhlp
NGLB =~ 1*freehms+hmsfmlsh+hmsacld
IMMIGR =~ 1*imbgeco+imueclt+imwbcnt
RELIG =~ 1*rlgdgr+rlgatnd+pray
GENDER =~ 1*eqwrkbg+eqpolbg+eqmgmbg+eqpaybg

#Structrual Model
NGLB ~ TRUST + IMMIGR + RELIG + GENDER'

#Estimating 4 versions
#ZIe of ndata is gestandardized over de assignments heen of nie
fit1 = sem(sem0, data=ndata_cfa, group="cntry")
fit2 = sem(sem0, data=ndata_cfa, group="cntry", group.equal ="regressions")
fit3 = sem(sem0, data=ndata_cfa, group="cntry", group.equal = "loadings")
fit4 = sem(sem0, data=ndata_cfa, group="cntry", group.equal = c("loadings", "regressions"))

#Fit and comparison tests
fit_table <- rbind(
  Free  = fitmeasures(fit1, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")),
  Equal = fitmeasures(fit2, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")),
  Free  = fitmeasures(fit3, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")),
  Equal = fitmeasures(fit4, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
)

round(fit_table, 3)

# Likelihood Ratio Tests (LR / Chi-Square Difference Tests)
anova(fit1, fit2) # Tests equality of structural regressions under configural invariance
anova(fit1, fit3) # Tests metric measurement invariance
anova(fit3, fit4) # Tests equality of structural regressions under metric invariance
