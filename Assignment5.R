library(ggplot2)
library(haven)
library(stargazer)
library(sandwich)
library(rdd)


df <- read_dta("data.dta")
names(df)


table(df$winnable_next) #DV
table(df$elected) #IV

 #measures the distance to the threshold for winning a seat in the candidate's district.
#used for a regression discontinuity design
table(is.na(df$margin100))
# sharp or fuzzy design
logmod <- glm(winnable_next ~ elected,
                  data = df,
                  family = binomial(link = "logit")
    )
summary(logmod)

stargazer(logmod, type = "html", out = "logmod.html")

exp(5.766)

l2p <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

l2p(5.766)

logmod2 <- glm(winnable_next ~ elected + margin100,
              data = df,
              family = binomial(link = "logit")
)
summary(logmod2)

ggplot(df)+
  geom_point(aes( x = margin100, y = elected)) +
  geom_vline(xintercept = 0, lty = 2)
  
#fuzzy
# dots on either side, probabilistic



ggplot(df) + geom_smooth(aes(x = margin100,
                 y = winnable_next))



logmod3 <- glm(winnable_next ~ elected * margin100,
               data = df,
               family = binomial(link = "logit")
)
summary(logmod3)

margin0 <- seq(-2,0, 0.1)

elected0 <- 0

ypred0 <- predict(logmod2, data.frame(elected = elected0, margin100 = margin0))


ggplot()+
  geom_line(aes(x = ))

bw <- 1:10

rd1 <- RDestimate(winnable_next ~ margin100 + elected, 
                  data = df, 
                  bw = bw,
                  cutpoint = 0)
plot(rd1)

summary(rd1)

DCdensity(df$margin100, bw = 10, cutpoint = 0)

margincut <- df$margin100[df$margin100>-5 & df$margin100 < 5]

coefs <- rd1$est
ll <- rd1$se * -1.96
hl <- rd1$se * +1.96


ggplot()+
  geom_point(aes(x = bw, y = coefs))+
  ylim(-0.1, 0.75)+
  geom_segment(aes(y = coefs + ll),
               yend = (coefs + hl),
               x = bw,
               xend = bw) + 
  ggtitle("LATE at different bandwidths")+
  ylab("y")






