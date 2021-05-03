library(haven)
library(Zelig)
library(stargazer)
library(MatchIt)
library(cobalt)

WooCon <- read_dta("WooCon16.dta")

# task 1

#Subset the dataset to only include dictatorship by excluding observations that have
#the value of 1 on the "democracy" variable.

dt <- WooCon[WooCon$democracy == 0,]
table(WooCon$democracy)
table(dt$democracy)

#Create a dichotomous dependent variable that takes the value of 1 if the variable
#"Coups" is greater to or equals 1 and 0 otherwise.
table(dt$Coups)
dt$coupsD <- ifelse(dt$Coups >= 1, 1, 0)
table(dt$coupsD)
#Create a dichotomous independent variable that takes the value of 1 if the variable
#"lparty" equals 2 and 0 otherwise.
table(dt$lparty)
dt$lpartyD <- ifelse(dt$lparty == 2, 1, 0)
table(dt$lpartyD)

#task 2 Replicate model 2 in Table 1

dt <- na.omit(dt)

mod1 <- glm(coupsD ~ lpartyD +
                     rgdpch +
                     grgdpch +
                     openk +
                     nmil +
                     comm +
                     ColdWar, 
        data = dt, family = binomial(link = "logit"))

summary(mod1)

stargazer(mod1, type = "html", out = "table1.html")

propensity <- mod1$fitted.values


#task 4

##workflow , select covariates. check balance, see if the averages are the same
#match and check the balance again
#if we happy, run regression, if not try another matching technique
#no Y in matching

#matching package also interesting

variables <- c( "coupsD",
                "lpartyD" ,
                 "rgdpch" ,
                 "grgdpch" ,
                 "openk" ,
                 "nmil" ,
                "comm" ,
                "ColdWar"
               )

dt2 <- dt[, variables]
sum(is.na(dt2)) #has no NA

#maybe do it 1 at a time?
match1 <- matchit(lpartyD  ~
          rgdpch +
          grgdpch +
          openk +
          nmil +
          comm +
          ColdWar,
          data = dt2,
          method = "nearest")
match1
summary(match1)

?matchit()


#mean difference should be 0
#check before and after matching

plot(match1)
#we want the points to be in the dotted lines
?bal.tab()
bal.tab(match1$X, treat = match1$treat, 
                  weights = match1$weights)

bal.plot(match1, which = "both",
                  var.name = "openk")
#we want densities to overlap
plot(match4)

love.plot(match1)
love.plot(match2)
love.plot(match3)
love.plot(match4)

bal.plot(match4, which = "both",
         var.name = "openk")
#we want the dots closer to 0

?love.plot()


matched_data <- match.data(match1)

z <- zelig(coupsD ~ lpartyD +
        rgdpch +
        grgdpch +
        openk +
        nmil +
        comm +
        ColdWar,
      data = matched_data, 
      model = "ls")

x0 <- setx(z, matched_data, lpartyD = 0)#set variables
x1 <- setx(z, matched_data, lpartyD = 1)

s.out <- sim(z, x = x0, x1 = x1)
summary(s.out)
summary(z)



match2 <- matchit(lpartyD  ~
                    rgdpch +
                    grgdpch +
                    openk +
                    nmil +
                    comm +
                    ColdWar,
                  data = dt2,
                  method = "nearest",
                  ratio = 2,
                  replace = TRUE) 


match3 <- matchit(lpartyD  ~
                    rgdpch +
                    grgdpch +
                    openk +
                    nmil +
                    comm +
                    ColdWar,
                  data = dt2,
                  method = "nearest",
                  caliper =  0.1) #0.1 to 0.5ishh

#can change methods

match4 <- matchit(lpartyD  ~
                    rgdpch +
                    grgdpch +
                    openk +
                    nmil +
                    comm +
                    ColdWar,
                  data = dt2,
                  method = "cem")
summary(match4)

match4$subclass

library(lme4)

dt2$subclass <- match4$subclass
dt2$weights <- match4$weights

mod.h <- lmer(coupsD ~ lpartyD +
                rgdpch +
                grgdpch +
                openk +
                nmil +
                comm +
                ColdWar,
              data = matched_data, )