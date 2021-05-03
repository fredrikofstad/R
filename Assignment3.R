library(dplyr)
library(stargazer)
library(survival)
library(tidyr)
library(texreg)
library(ggplot2)
library(survminer)
library(eha)
library(coxed)


dt <- read.csv("davis.csv")


#subsetting the dataset
df <-  dt %>% 
  filter(ApplyYrI_NotYetApplied_a == 1 | ApplyYrI_Ind_a == 1) %>%
  filter(end0 >= 1) %>%
  filter(year > 1947) %>%
  filter(cty_ind == 1) %>%
  dplyr::select(year,
                begin0,
                end0,
                ApplyYrI_Ind_a,
                polity2,
                s3un100,
                allies_atop_ct_mbrs_zeroed,
                lnopentotperc,
                gatttotperc,
                lnnew_gdp_67,
                lnpcgdp,
                ColdWarPd,
                Art26_Strata,
                ccode) 
#3350  




mod2 <- coxph(Surv(begin0,end0,ApplyYrI_Ind_a) ~ 
                         +polity2 +s3un100	+allies_atop_ct_mbrs_zeroed
                         + lnopentotperc +gatttotperc
                         +lnnew_gdp_67 +lnpcgdp +ColdWarPd 
                         +cluster(ccode)+strata(Art26_Strata),
                         data=df)

summary(mod2)

mod3 <- coxph(Surv(begin0,end0,ApplyYrI_Ind_a) ~ 
                +polity2 +s3un100	+allies_atop_ct_mbrs_zeroed
              + lnopentotperc +gatttotperc
              +lnnew_gdp_67 +lnpcgdp +ColdWarPd,
              data=df)

screenreg(l = list(mod2, mod3), 
          include.rsquared = FALSE, include.maxrs=FALSE,
          include.events = TRUE, include.nobs = TRUE,
          include.missings = FALSE, include.zph = FALSE)



names(summary(mod2))
#SE <- summary(mod2)$coefficients[,"robust se"]
conf <- exp(confint(mod2))

stargazer(mod2, type = "html", apply.coef = exp, ci.custom = list(conf), out = "table3.htm",
          add.lines=list(c("AIC", round(AIC(mod2),2), round(AIC(mod3),1))))


#stuff
KaplanMeier <- survfit(Surv(begin0,end0,ApplyYrI_Ind_a) ~ 1, data = df)

ggsurvplot(KaplanMeier, risk.table = TRUE)

## You can stratify by categorical variables 

KaplanMeier2 <- survfit(Surv(begin0,end0,ApplyYrI_Ind_a) ~ Art26_Strata, data = df)

ggsurvplot(KaplanMeier2, risk.table = TRUE)


####task 3###
#Evaluate the proportional hazard assumption. If necessary correct 
#any violations of the assumption. Comment briefly on your approach
#and on what you find.

#the effects of the covariates are the same no matter how long we have observed the spell
#we evaluate this assumption

plot(cox.zph(mod2, "rank"), col = "red")
plot(cox.zph(mod2, "identity"), col = "gold")
cox.zph(mod2, "rank") 
?cox.zph


timeCoefficintPlot <-function(model, term1, term2, from, to){
  logtime <- log(seq(from, to))
  combcoef <-model$coefficients[term1]+logtime*model$coefficients[term2]
  vcov <- model$var 
  rownames(vcov) <- names(coefficients(model))
  colnames(vcov) <- names(coefficients(model))
  combcoef.se <- sqrt(vcov[term1,term1]+
                        logtime^2*vcov[term2,term2]+2*logtime*vcov[term1,term2])
  plotdata <-data.frame(coef=combcoef, 
                        time=seq(from, to), 
                        upper=combcoef+1.65*combcoef.se,
                        lower=combcoef-1.65*combcoef.se,
                        upper1=combcoef+1.96*combcoef.se,
                        lower1=combcoef-1.96*combcoef.se, 
                        upper2=combcoef+2.57*combcoef.se,
                        lower2=combcoef-2.57*combcoef.se)
  plot <- ggplot(plotdata) + geom_line(aes(y=coef, x=time))+
    geom_ribbon(aes(ymin=lower, ymax=upper, x=time), alpha = 0.3)+
    geom_ribbon(aes(ymin=lower1, ymax=upper1, x=time), alpha = 0.2)+
    geom_ribbon(aes(ymin=lower2, ymax=upper2, x=time), alpha = 0.1)+
    geom_hline(yintercept = 1, colour="grey")+
    ylab("Combined Coefficient")+
    xlab("Time in days")+
    theme_classic()+
    theme(legend.title=element_blank())
  print(plot)
  return(plot)
}


mod2tt <- coxph(Surv(begin0,end0,ApplyYrI_Ind_a) ~ 
                +polity2 +s3un100	+allies_atop_ct_mbrs_zeroed
              + lnopentotperc +gatttotperc
              +lnnew_gdp_67 +lnpcgdp +ColdWarPd +tt(ColdWarPd) 
              +cluster(ccode)+strata(Art26_Strata),
              data=df)

timeCoefficintPlot(mod2tt, 
                   term1 = "ColdWarPd",
                   term2 = "tt(ColdWarPd)",
                   from = min(df$end0), 
                   to = max(df$end0))

summary(mod2tt)

####Task 4###
#Re-estimate the model as at least three different parametric models. Which of the models
#you have estimated, including both the parametric models and the Cox model, do you
#think is most useful? Explain your choice.
cox <- coxph(Surv(begin0,end0,ApplyYrI_Ind_a) ~ 
                +polity2 +s3un100	+allies_atop_ct_mbrs_zeroed
              + lnopentotperc +gatttotperc
              +lnnew_gdp_67 +lnpcgdp +ColdWarPd,
              data=df)

weibull <- phreg(Surv(begin0,end0,ApplyYrI_Ind_a) ~ 
                   +polity2 +s3un100	+allies_atop_ct_mbrs_zeroed
                 + lnopentotperc +gatttotperc
                 +lnnew_gdp_67 +lnpcgdp +ColdWarPd,
                  data=df,
                  dist = "weibull")

exponential <- phreg(Surv(begin0,end0,ApplyYrI_Ind_a) ~ 
                        +polity2 +s3un100	+allies_atop_ct_mbrs_zeroed
                      + lnopentotperc +gatttotperc
                      +lnnew_gdp_67 +lnpcgdp +ColdWarPd,
                      data=df,
                      dist = "weibull",
                      shape = 1)

loglogistic <- phreg(Surv(begin0,end0,ApplyYrI_Ind_a) ~ 
                       +polity2 +s3un100	+allies_atop_ct_mbrs_zeroed
                     + lnopentotperc +gatttotperc
                     +lnnew_gdp_67 +lnpcgdp +ColdWarPd,
                      data=df,
                      dist = "loglogistic")
#function to return AIC rounded to two significant figures
AICphreg <- function(x){
  k <- length(coefficients(x))
  loglik <- x$loglik[2]
  aic <- round(2*k - 2*loglik,2)
  return(aic)
}
#compare the models
?stargazer()
stargazer(cox,weibull,exponential,loglogistic,mod2, type = "html",out = "table2.html",
          dep.var.caption = "", dep.var.labels.include = FALSE,model.names = FALSE, 
          column.labels = c("Cox","Weibull","Exponential","Loglogistic", "original"),
          add.lines=list(c("AIC",AICphreg(cox), AICphreg(weibull),AICphreg(exponential), AICphreg(loglogistic),AICphreg(mod2))))

screenreg(weibull1)



AICphreg(weibull)
AICphreg(loglogistic)
AICphreg(exponential)

####Task 5####
#Using either the Cox model or one of the parametric models, 
#create a figure that illustrates the relationship between regime type
#("Polity score") and the number of years between a country became eligible
#for GATT/WTO membership and the country applying for such membership.

mod2$coefficients
predictedSurv <-survfit(mod2,
                        newdata = data.frame(polity2 = c(min(df$polity2, na.rm = TRUE), 
                                                         max(df$polity2, na.rm = TRUE)),
                                             s3un100 = mean(df$s3un100, na.rm = TRUE), 
                                             allies_atop_ct_mbrs_zeroed = mean(df$allies_atop_ct_mbrs_zeroed, na.rm = TRUE),
                                             lnopentotperc = mean(df$lnopentotperc, na.rm = TRUE), 
                                             gatttotperc = mean(df$gatttotperc, na.rm = TRUE), 
                                             lnnew_gdp_67 = mean(df$lnnew_gdp_67, na.rm = TRUE), 
                                             lnpcgdp = mean(df$lnpcgdp, na.rm = TRUE), 
                                             ColdWarPd = 1))

ggsurvplot(predictedSurv, 
           data = predictedSurv$surv)




scenario1 <- df %>% 
  dplyr::select(names(coefficients(mod2))) %>% 
  mutate(polity2 = min(df$polity2,na.rm = TRUE)) %>% 
  na.omit()

scenario2 <- mutate(scenario1, polity2 = max(df$polity2,na.rm = TRUE))

?coxed()
expectedDurationsBS <- coxed(cox,
                             newdata = scenario1,
                             newdata2 = scenario2,
                             id = df$ccode,
                             bootstrap = TRUE,
                             method = "gam",
                             B = 100)
summary(expectedDurations) 




expectedDurations <- coxed(mod2,
                             newdata = scenario1,
                             newdata2 = scenario2,
                             #  id = scenario1$id,
                             id = scenario1$ccode,
                             method = "gam")


expectedDurationsPlotData <- as.data.frame(summary(expectedDurations))
expectedDurationsPlotData$Accountability <- factor(c("lowest", 
                                                     "highest", 
                                                     "difference"), 
                                                   levels = c("lowest", 
                                                              "highest", 
                                                              "difference"))
ggplot(expectedDurationsPlotData, 
       aes(y = mean, 
           x = Accountability,
           ymin =lb,
           ymax = ub))+
  geom_errorbar(width = 0.1)+
  geom_point() +
  theme_classic()+
  ylab("Average predicted number of days until compliance")


filter(expectedDurationsPlotData, Accountability != "difference") %>% 
  ggplot( aes(y = mean, 
              x = Accountability,
              ymin =lb,
              ymax = ub))+
  geom_errorbar(width = 0.1)+
  geom_point() +
  theme_classic()+
  ylab("Average predicted number of days until compliance")

