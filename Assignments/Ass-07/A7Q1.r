# http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r

library(tidyverse)
rat.dat <- matrix(c(30,60,
                    27,42,
                    52,58,
                    38,52,
                    20,28,
                    26,73,
                    8,16,
                    41,46,
                    49,63,
                    49,44),
                  byrow = TRUE,
                  ncol = 2) %>% 
  as.data.frame()
colnames(rat.dat) <- c("Control","0.1μg")

rat.dat2 <- rat.dat %>% 
  gather("Group","N", 1:2) %>% 
  mutate(Group = factor(.$Group, levels = c("Control", "0.1μg")))

with(rat.dat2, shapiro.test(N[Group == "Control"])) # p-value = 0.6387
with(rat.dat2, shapiro.test(N[Group == "0.1μg"])) # p-value = 0.8586

# From the output, the two p-values are greater than the significance level 0.05 
# implying that the distribution of the data are not significantly different from 
# the normal distribution. In other words, we can assume the normality.

# We’ll use F-test to test for homogeneity in variances.

res.ftest <- var.test(N ~ Group, data = rat.dat2)
res.ftest

# The p-value of F-test is p = 0.6189. It’s greater than the significance level alpha = 0.05.
# In conclusion, there is no significant difference between the variances of the two sets of data.
# Therefore, we can use the classic t-test witch assume equality of the two variances.

# Compute independent t-test
(mupt = t.test(rat.dat$Control,rat.dat$`0.1µg`, var.equal = TRUE))

(m1 = aov(N ~ Group, data = rat.dat2))
(m1t = t.test(rat.dat$Control,rat.dat$`0.1µg`, var.equal = FALSE))
summary.lm(m1)
(m2 = update(m1, .~. - Group))
anova(m2, m1, test="Chisq")


