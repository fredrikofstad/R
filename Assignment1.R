library(ggplot2)
library(haven)
library(stargazer)
library(sandwich)
library(lmtest)
library(dplyr)
library(broom)
library(MASS)
library(pROC) 
library(ResourceSelection)


#Task 1#

dt <-read_dta("dodgingrules.dta")

#original model
logmod <- glm(filisave ~ gop +
                running18 +
                goprun18 +
                wings +
                leader +
                stdrank +
                servedmin,
              data=dt, 
              family = binomial(link = "logit"),
              y = TRUE, 
              x = TRUE
)
#model without wings variable
logmod2 <- glm(filisave ~ gop +
                 running18 +
                 goprun18 +
                 leader +
                 stdrank +
                 servedmin,
               data=dt, 
               family = binomial(link = "logit"))

#Correcting standard errors
# Clustering the standard errors by state:

clusteredlogmod <- vcovCL(logmod, cluster = dt$stateid)
clusteredlogmod2 <- vcovCL(logmod2, cluster = dt$stateid)

# square root of the diagonal to get the standard errors: 
robust_se <- sqrt(diag(clusteredlogmod))
robust_se2 <- sqrt(diag(clusteredlogmod2))
robust_se

logmod$clusterVCOV <- clusteredlogmod
logmod2$clusterVCOV <- clusteredlogmod2

#see the variables
str(logmod)

#recreating table A1 from the article
stargazer(logmod, type = "text", se = list(robust_se), 
          covariate.labels = c("Republican",
                               "Running in 2018", 
                               "GOP running in 2018" ,
                               "Ideologue", 
                               "Party leader",
                               "Chamber rank",
                               "Served in minority"))

### recreating figure 2 from the article
logmodtidy <- tidy(logmod) %>% 
  dplyr::select(term, estimate, std.error) %>% 
  mutate(clusteredSE = robust_se) %>% 
  filter(grepl("as.factor", term) == FALSE, 
         term != "(Intercept)") %>% 
  mutate(labels = c("Republican",
                    "Running in 2018", 
                    "GOP running in 2018" ,
                    "Ideologue", 
                    "Party leader",
                    "Chamber rank",
                    "Served in minority"))


ggplot(logmodtidy, aes(y = reorder(labels, -estimate), x = estimate, 
                       xmin = estimate - 1.96 * abs(clusteredSE), 
                       xmax = estimate + 1.96 * abs(clusteredSE))) +
  geom_errorbarh(height = .2) +
  geom_point() +
  geom_vline(aes(xintercept = 0), color = "lightgrey") +
  ylab("") +
  xlab("Coefficient") +
  theme_classic()


##Task 2##
logmod$coefficients["running18"]
str(logmod)

exp(logmod$coefficients["running18"])
running18prob <- 1/(1+exp(-logmod$coefficients["running18"]))

#calculating the scenarios

dem <- 0.996 + 0.154*0 + 1.410*0 -2.416*0 -4.844*0 -1.614*0 + 0.241*1.73 + 1.285*1
dem18 <- 0.996 + 0.154*0 + 1.410*1 -2.416*0 -4.844*0 -1.614*0 + 0.241*1.73 + 1.285*1
rep <- 0.996 + 0.154*1 + 1.410*0 -2.416*0 -4.844*0 -1.614*0 + 0.241*1.73 + 1.285*1
rep18 <- 0.996 + 0.154*1 + 1.410*1 -2.416*1 -4.844*0 -1.614*0 + 0.241*1.73 + 1.285*1

l2p <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

pdem <- l2p(dem)
pdem18 <- l2p(dem18)
prep <- l2p(rep)
prep18 <- l2p(rep18)

pdem18 - pdem
prep18 - prep
pdem18 - prep18

###Task 3###

#simulating predicted probabilities

set.seed(961209)
simBetas <- mvrnorm(n = 1000, 
                    mu = coefficients(logmod), 
                    Sigma = logmod$clusterVCOV)

#creating scenerios to compare
names(coefficients(logmod))

range(dt$wings, na.rm=TRUE)
#letting the wings variable vary on min mean and max
xMatrix <- cbind(1, # value for the intercept 
                 #Mode(dt$gop, na.rm = TRUE),
                 c(1,1,0,0),
                 Mode(dt$running18, na.rm = TRUE),
                 Mode(dt$goprun18, na.rm = TRUE),
                 c(0.012,0.861,0.012,0.861),
                 Mode(dt$leader, na.rm = TRUE),
                 mean(dt$stdrank, na.rm = TRUE),
                 Mode(dt$servedmin, na.rm = TRUE)
)
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

plotPoints <- cbind(c("R Close", "R Far", "D close", "D far"),quantileValues)
colnames(plotPoints) <- c("chambermean", "lower", "estimate", "upper")

#plotting the results

ggplot(plotPoints, 
       aes(x = chambermean, 
           y = estimate, 
           ymin = lower, 
           ymax = upper)) +
  geom_errorbar(width =.2)+
  geom_point()+
  ylim(0,1)+
  ylab("Predicted probability of support for saving the filibuster")+
  xlab("Absolute ideological distance from the chamber mean")+
  theme_classic() 

##### Task 4 #####

#Evaluating the models

#regression table comparing the models
stargazer(logmod, logmod2, align=TRUE, type = "text", se = list(robust_se, robust_se2), 
          covariate.labels = c("Republican",
                               "Running in 2018", 
                               "GOP running in 2018" ,
                               "Ideologue", 
                               "Party leader",
                               "Chamber rank",
                               "Served in minority"))

#checking the performance of the models

performance <- data.frame(logits = fitted(logmod), 
                          logits2 = fitted(logmod2), 
                          outcomes = as.integer(logmod$y))

separationplot(pred = performance$logits, 
               actual = performance$outcomes, 
               heading = "Performance of orignial model") 

separationplot(pred = performance$logits2, 
               actual = performance$outcomes, 
               heading = "Performance of model without wings variable")

#Hosmer and Lemeshow  test and ROC graph comparison

predictor <- fitted(logmod)
predictor2 <- fitted(logmod2)
response <- logmod$y

hl1 <- hoslem.test(logmod$y, fitted(logmod), g=10)
hl2 <- hoslem.test(logmod$y, fitted(logmod2), g=10)
hl1
hl2

roc <- roc(response,predictor, plot=TRUE)

roc(response, predictor, plot=TRUE, legacy.axes=TRUE, percent=TRUE, col="#1337FF", lwd=4, print.auc=TRUE)

plot.roc(response, predictor2, percent=TRUE, col="#FF1337", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Model with wings", "Model without wings"), col=c("#1337FF", "#FF1337"), lwd=4)