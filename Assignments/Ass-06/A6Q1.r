# https://stats.idre.ucla.edu/r/dae/poisson-regression/
# https://www.colorado.edu/physics/phys2150/phys2150_sp12/PHYS2150/Lecture_Notes_files/phys2150_lect6_sp12.pdf
library(tidyverse)
library(sandwich)
data = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                0,2,1,0,3,1,2,0,1,0,0,0,0,1,0,
                0,0,0,0,0,0,0,0,0,0,1,1,1,1,1),
              # byrow = TRUE,
              ncol = 3) %>% as.data.frame()
colnames(data) = c("Year","Deaths", "SpeedReduced")
data$SpeedReduced = factor(data$SpeedReduced, labels = c("No","Yes"))
with(data, tapply(Deaths, SpeedReduced, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
summary(m1 <- glm(Deaths ~ Year + SpeedReduced, family=poisson(), data=data))

summary(m1 <- glm(Deaths ~ Year + SpeedReduced, family=quasipoisson(), data=data))

cov.m1 <- vcovHC(m1, type="HC0")
std.err <- sqrt(diag(cov.m1))
r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
               LL = coef(m1) - 1.96 * std.err,
               UL = coef(m1) + 1.96 * std.err)

r.est

with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))

## update m1 model dropping prog
m2 <- update(m1, . ~ . - SpeedReduced)
## test model differences with chi square test
anova(m2, m1, test="Chisq")

summary(m1 <- glm(Deaths ~ SpeedReduced, family="poisson", data=data))
with(m1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))
(s1 <- data.frame(Deaths = mean(data$Deaths),
                  SpeedReduced = factor(1:2, levels = 1:2, labels = c("No","Yes"))))
predict(m1, s1, type="response", se.fit=TRUE)

#ZIP
library(pscl)
summary(m1 <- zeroinfl(Deaths ~ Year| SpeedReduced, data = data))
mnull <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)
summary(p1 <- glm(Deaths ~ Year + SpeedReduced, family = poisson, data = data))
vuong(p1, m1)

#######################################
# https://www.colorado.edu/ibs/crs/workshops/R_for_BayesianP10-24-2008_Lu/Bayesian_R.pdf
# http://learnbayes.blogspot.com.au/2007/12/poisson-change-point-model.html
Y <- c(0,2,1,0,3,1,2,0,1,0,0,0,0,1,0)
########################################################


#############################
library(MCMCpack)
c0 = mean(Y)
d0 = var(Y)

model1 <- MCMCpoissonChange(Y~1, 
                            m=1, 
                            c0=c0, 
                            d0=d0, 
                            burnin = 5000, 
                            mcmc = 30000,
                            verbose = 1000,
                            marginal.likelihood="Chib95")

## Draw plots using the "right" model
plotState(model1)
plotChangepoint(model1)
plot(model1)
summary(model1)
###############################