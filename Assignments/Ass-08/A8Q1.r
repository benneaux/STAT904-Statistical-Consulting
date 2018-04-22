library(tidyverse)
train.dat <- matrix(c(30,60,71,33,36,
                      27,42,50,78,27,
                      52,48,38,71,60,
                      38,52,59,58,51,
                      20,28,65,35,29,
                      26,93,58,35,34,
                      8,32,74,46,24,
                      41,46,67,32,17,
                      49,63,NA,NA,50,
                      49,44,NA,NA,53),
                    byrow = TRUE,
                    nrow = 10) %>% 
  as.data.frame()
colnames(train.dat) <- c("NoTraining","1hr","2hr","5hr","10hr")

train.dat2 <- train.dat %>% gather("Training","N",1:5) %>% mutate(Training = factor(.$Training, levels = c("NoTraining","1hr","2hr","5hr","10hr")))

ggplot(train.dat2, aes(y = N, x = Training)) +
  geom_boxplot()

ggplot(train.dat2, aes(y = N, x =Training, fill = Training)) +
  geom_violin(trim = FALSE)

# One Way Anova (Completely Randomized Design)
fit <- aov(N ~ Training, data=train.dat2)
summary(fit)
summary.aov(fit)

fit.single <- aov(N ~ 1, data = train.dat2) ## 1 means global mean (intercept)
## Compare with cell means model:
anova(fit.single, fit)
summary.lm(fit)

