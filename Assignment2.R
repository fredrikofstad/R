library(dplyr)
library(stargazer)
library(ggplot2)
library(sandwich)
library(haven)
library(MASS)
library(countreg)
library(msme)

dt<-read_dta("AJPS.dta")

#####task 1####

#create logisticbills t-1
df <- dt %>% 
  dplyr::select(laws, logisticbills, oppcong, natct) %>%
  mutate(logisticbills = lag(logisticbills))
#check to see that the values in  our new dataset has been moved down one row
head(dt$logisticbills)
head(df$logisticbills)

####Task 2####

#changing reference category in order to replicate the same intercept

#construct mode function, because R doesn't have one for some reason.
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(df$natct)
df <- df %>% 
  mutate(natct = as.factor(natct), 
         natct = relevel(natct, ref = 49))

mod <- glm(laws ~ ## DV
              logisticbills +
              oppcong+
              natct,
              data = df,
              family = "poisson")


#clustering on natct
clustered <- vcovCL(mod, cluster = df$natct)
mod$clusterVCOV <- clustered
# square root of the diagonal to get the standard errors: 
SE <- sqrt(diag(clusetered))




stargazer(mod, type = "html", se = list(SE), omit="natct",
          dep.var.labels = "LawsStruckDown",
          covariate.labels = c("CourtCurbing",
                               "OppositePartyCongress"),
                                out="table1.html")


####Task 3####
set.seed(1224)
simBetas <- mvrnorm(n = 1000, 
                    mu = coefficients(mod), 
                    Sigma = mod$clusterVCOV)

#creating scenerios to compare
names(coefficients(mod))

range(df$logisticbills, na.rm=TRUE)
#letting logisticbills vary
xMatrix <- cbind(1, # value for the intercept 
                 c(0,1,2,3,4,5), #logisticbills
                 0,#oppcong
                 0,0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,0,0,0,
                 0,0,0,0,0,0,0,1 )
#check if all variables are included
ncol(simBetas) == ncol(xMatrix) #returns true
#calculating predicted probabilities

xBetaMatrix <- xMatrix %*% t(simBetas ) #multiply the betas with my x values
predProbability <- 1/(1+exp(-xBetaMatrix)) #used for logistical models

#point estimates and confidence intervals
quantileValues <- apply(X = predProbability,
                        MARGIN = 1,
                        FUN =  quantile, probs = c(.05,.5,.95))
quantileValues <- as.data.frame(t(quantileValues)) #make it a dataframe

plotPoints <- cbind(c("0", "1", "2", "3","4","5"),quantileValues)
colnames(plotPoints) <- c("logisticbills", "lower", "estimate", "upper")

#plotting the results

ggplot(plotPoints, 
       aes(x = logisticbills, 
           y = estimate, 
           ymin = lower, 
           ymax = upper)) +
  geom_errorbar(width =.2)+
  geom_point()+
  ylim(0,1)+
  ylab("Frequency of Constitutional Invalidations of Acts of Congress")+
  xlab("The number of Court-curbing bills
introduced each year (log transformed)")+
  theme_classic() 

##attempt 2
# setting a range for our continuous outcome of interest
ourRange <-seq(from=min(df$logisticbills, na.rm=TRUE), 
               to = max(df$logisticbills, na.rm=TRUE), 
               length.out=50)

## Creating a scenario with 0 lagged conflict, other variables at mean and 1999 as the year
ourX <- cbind(1, # value for the intercept 
               ourRange, #logisticbills
               0,#oppcong
               0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,0,0,0,
               0,0,0,0,0,0,0,1 )
  

## calculating the expected count for each simulation*scenario
xbetas <- ourX %*% t(simBetas)
expY  <- exp(xbetas)
# Taking quantiles to get point estimate and confidence intervals ##
quantileValues <- apply(X = expY, 
                        MARGIN = 1, 
                        FUN = quantile, 
                        probs = c(.025,.5,.975))
plotPoints <- as.data.frame(cbind(ourRange, t(quantileValues)))

# making the plot: 
ggplot(plotPoints, 
       aes(x = ourRange, 
           y = `50%`, 
           ymin = `2.5%`, 
           ymax = `97.5%`))+
  geom_ribbon(alpha = 0.3) + # the confidence intervals
  geom_line() + # the point predictions
  xlab("The number of Court-curbing bills
introduced each year (log transformed)") +
  ylab("Frequency of Constitutional Invalidations of Acts of Congress") +
  theme_classic()



####Task 4####
rootogram(mod)
P__disp(mod)
####Task 5####

neg <- glm.nb(formula = mod$formula, data = mod$data)
summary(neg)
rootogram(neg)

quasi <- glm(formula = mod$formula, family = quasipoisson, data = mod$data)
rootogram(quasi)

AIC(mod, neg, quasi)

mod <- glm(laws ~ ## DV
             logisticbills +  # 
             oppcong+
             natct,
           data = df,
           family = "poisson")


hurdle <- pscl::hurdle(laws ~  # dv
                         # truncated count component: 
                         logisticbills + 
                         oppcong +
                         natct  |
                         ## hurdle component:  
                         logisticbills + 
                         oppcong +
                         natct  , 
                       data=df,
                       dist = "negbin", zero.dist = "binomial", link = "logit")
summary(hurdle)
rootogram(hurdle)

zeroinflated <-zeroinfl(laws ~  # dependent variable
                          #  count component: 
                          logisticbills + 
                          oppcong +
                          natct  |
                          ## model of excess zeroes:   
                          logisticbills + 
                          oppcong +
                          natct  , 
                        data = df,
                        dist = "negbin",  link = "logit")
summary(zeroinflated)


rootogram(zeroinflated) 
rootogram(mod)
rootogram(zeroinflated) 
rootogram(mod)

rootogram(hurdle)
rootogram(neg)
