
nyrs = 30
nreps = 1000

N = matrix(0, nrow = nyrs, ncol = nreps)

lambda = matrix(rnorm(nyrs*nreps, 1.001, 0.11), nrow = nyrs, ncol = nreps)

N[1,] = runif(nreps, 400, 800)

for(r in 1:nreps){
  for(i in 2:nyrs){
    N[i,r] = N[i-1,r]*lambda[i-1,r]
  }
}

matplot(N, type = "l")

tibble(time = c(1:nyrs),
       med = apply(N, 1, median),
       lcl = apply(N, 1, FUN = function(x) quantile(x, prob = 0.025)),
       ucl = apply(N, 1, FUN = function(x) quantile(x, prob = 0.975))) %>% 
  ggplot(aes(x = time)) +
  geom_line(aes(y = med), lwd = 2) +
  geom_line(aes(y = lcl), lty = 2, lwd = 1.5) +
  geom_line(aes(y = ucl), lty = 2, lwd = 1.5) +
  xlab("Years in the future") +
  ylab("Projected population size") +
  scale_y_continuous(breaks = seq(0, 2500, 250), limits = c(0, 2000))

