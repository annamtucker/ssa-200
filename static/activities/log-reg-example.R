# logistic regression example

library(tidyverse)
library(cowplot)

x = rnorm(100, 5, 2)
beta = -0.7
p = plogis(5 + beta*x)
y = rbinom(100, 1, p)

df = data.frame(y = y,
                x = x,
                p = p)

ggplot(df, aes(x = x, y = p)) +
  geom_line(lwd = 4, col = "darkgreen") +
  xlab("Index of pirate rat abundance") +
  ylab("Island mouse survival probability") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24))

        


x = seq(0, 50, 0.5)

beta = 0.2
se = 0.5
int = qlogis(0.1)
p = plogis(int + beta*x)
lcl = beta-1.96*se
ucl = beta+1.96*se

df = data.frame(x = x,
                p = p,
                lower = plogis(int + lcl*x),
                upper = plogis(int + ucl*x))

ggplot(df, aes(x = x, y = p)) +
  geom_line(lwd = 4, col = "darkgreen") +
  xlab("Ambient noise level") +
  ylab("Transition to lower\nabundance state") +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 24)) +
  ylim(0,1)


