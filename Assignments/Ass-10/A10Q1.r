library(tidyverse)

socspeech.dat <- matrix(c(8,24,32,27,
                          42,121,138,108),
                        byrow = TRUE,
                        nrow = 2,
                        dimnames = list(c("Present","Absent"),c("Upper", "UMid", "LMid", "Lower"))) %>% 
  as.data.frame()

(m1 <-  chisq.test(socspeech.dat, simulate.p.value = TRUE, B = 1000))
m1$expected
m1$observed
m1$residuals
m1$stdres


yfit <- dpois(as.integer(names(socspeech.dat)), estimate)