options(digits = 10)
n_sim <- 1000
n_black_kill <- 132
n_white_kill <- 146
n_kill <- n_black_kill + n_white_kill
black_prob <- 0.13

black <- rep (NA, n_sim)
for (i in 1:n_sim){
  x <- rbinom(n_kill, 1, black_prob)
  black[i] <- sum (x)
}
mean(black/n_kill)
pct_black <- black/n_kill

paste ("Max % black killed in", n_sim,  "simulations:", round(max (pct_black)*100, 2))

more_than_prop_in_pop <- sum ((pct_black > black_prob ) == TRUE)*100/n_sim
paste ("Prob of Proportional killed blacks =", more_than_prop_in_pop, "%")

paste ("Prob of > 10% killed blacks =", 
       signif(sum ((pct_black > 0.1) == TRUE)*100/n_sim, digits=4),"%  -  ",
       signif((1-pbinom(0.1*n_kill, n_kill, black_prob))*100, 4))
paste ("Prob of > 12% killed blacks =", 
       signif(sum ((pct_black > 0.12) == TRUE)*100/n_sim, digits=4),"%  -  ",
       signif((1-pbinom(0.12*n_kill, n_kill, black_prob))*100, 4))
paste ("Prob of > 14% killed blacks =", 
       signif(sum ((pct_black > 0.14) == TRUE)*100/n_sim, digits=4),"%  -  ",
       signif((1-pbinom(0.14*n_kill, n_kill, black_prob))*100, 4))
paste ("Prob of > 16% killed blacks =", 
       signif(sum ((pct_black > 0.16) == TRUE)*100/n_sim, digits=4),"%  -  ",
       signif((1-pbinom(0.16*n_kill, n_kill, black_prob))*100, 4))
paste ("Prob of > 20% killed blacks =", 
       signif(sum ((pct_black > 0.2) == TRUE)*100/n_sim, digits=4),"%  -  ",
       signif((1-pbinom(0.2*n_kill, n_kill, black_prob))*100, 4))
paste ("Prob of > 30% killed blacks =", 
       signif(sum ((pct_black > 0.3) == TRUE)*100/n_sim, digits=4),"%  -  ",
       signif((1-pbinom(0.3*n_kill, n_kill, black_prob))*100, 4))
paste ("Prob of > 40% killed blacks =", 
       signif(sum ((pct_black > 0.4) == TRUE)*100/n_sim, digits=4),"%  -  ",
       signif((1-pbinom(0.4*n_kill, n_kill, black_prob))*100, 4))
paste ("Prob of > Actual % killed blacks =", 
       signif(sum ((pct_black > 132/146) == TRUE)*100/n_sim, digits=4),"%  -  ",
       signif((1-pbinom(132/146*n_kill, n_kill, black_prob))*100, 4))

prob <- rep (NA, n_kill)
for (i in 1:n_kill){
  prob[i] <- (1-pbinom(i, n_kill, black_prob))*100
}

people <- 1:n_kill
fake <- data.frame (people, prob)
ggplot () +
  geom_line (data=fake, aes (x=people, y=prob)) +
  geom_vline (xintercept=132, linetype="dashed") +
  labs (title="Probabilty of Unarmed Blacks Killed in Colorblind Police Shootings",
         subtitle="Out of 278 Police Killings of Unarmed People of All Races") +
  xlab ("Number of Unarmed Blacks Killed (NUBK)") +
  ylab ("Prob(NUBK") +
  annotate("text", x=130, y=50, angle=90, 
           label = "Actual Unarmed Blacks Killed=132") +
  xlim (0, 150)

plot (prob)
