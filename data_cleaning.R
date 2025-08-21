library(nflfastR)
library(tidyverse)

dat <- load_pbp(2018) |> 
  progressr::with_progress()


pass_plays <- dat |> 
  filter(pass==1 & penalty==0)

View(as.data.frame(pass_plays$desc))

plot(density(pass_plays$epa))
abline(v=mean(pass_plays$epa), col='red', lwd=2)
boxplot(pass_plays$epa)
mean(pass_plays$epa)
summary(pass_plays$epa)

pass_plays <- pass_plays |> 
  mutate(DL = as.integer((qb_hit==1 & blitz==0)| (pres))