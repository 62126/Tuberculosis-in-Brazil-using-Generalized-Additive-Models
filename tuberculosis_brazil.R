library(mgcv)
library(fields)
library(maps)
library(sp)
library(broom)
library(tidyverse)

summary(TBdata)
cor(TBdata[,1:10])
pairs(TBdata[,1:10])

library(ggcorrplot)
# Data cleaning 
# We use the correlation matrix to see the correlation coefficient, whether there is a correlation, and whether it is significant.
corr <- TBdata[, c(1:8)] %>%
  mutate(select(TBdata, TB))
corr1 = cor(corr[,c(1:9)])
p.mat <- cor_pmat(corr, use = "complete", method = 'pearson')
ggcorrplot(corr1,hc.order = TRUE, type = "lower", lab = TRUE, p.mat = p.mat, insig = "blank")

library(ggstatsplot)
library(ggpubr) 
# risk is defined as the rate of TB cases per unit population
TBdata$TB_Rating <- TBdata$TB / TBdata$Population
# Histograms for each of the two variables so that their distribution can be observed
p1<-ggscatterstats(data = TBdata, x = "Indigenous", y = "TB_Rating")

TBdata_train <- TBdata %>% filter(Year < 2014)

mod1 = gam(TB ~ offset(log(Population))+ s(Illiteracy, k = 10, bs = "cr") + s(Indigenous, k = 10, bs = "cr") + s(Urbanisation, k = 10, bs = "cr") +
             s(Density, k = 10, bs = "cr") + Poverty + s(Poor_Sanitation, k = 10, bs = "cr") + Unemployment + Timeliness +
             as.factor(Year) + Region + s(lon, lat), 
           data = TBdata_train, 
           family = nb(link = 'log'))

# Fit the GAM
mod2 = gam(TB ~ offset(log(Population))+ s(Illiteracy, k = 10, bs = "cr") + s(Indigenous, k = 10, bs = "cr") + s(Urbanisation, k = 10, bs = "cr") +
             s(Density, k = 10, bs = "cr") + Poverty + s(Poor_Sanitation, k = 10, bs = "cr") + Unemployment + Timeliness +
             as.factor(Year) + Region + s(lon, lat), 
           data = TBdata_train, 
           family = poisson)

# Fit the GAM
mod3 = gam(TB ~ offset(log(Population))+ s(Illiteracy, k = 10, bs = "cr") + s(Indigenous, k = 10, bs = "cr") + s(Urbanisation, k = 10, bs = "cr") +
             s(Density, k = 10, bs = "cr") + Poverty + s(Poor_Sanitation, k = 10, bs = "cr") + Unemployment + Timeliness +
             as.factor(Year) + Region + s(lon, lat), 
           data = TBdata_train, 
           family = gaussian(link = "identity"))

# Summarise the model
summary(mod1)
summary(mod2)
summary(mod3)

# 2x2 plot for the residuals
par(mfrow=c(2,2))

#  Runing gam.check on our original model
gam.check(mod1,pch=20)
gam.check(mod2,pch=20)
gam.check(mod3,pch=20)

AIC(mod1)
AIC(mod2)
AIC(mod3)

summary(mod4)
summary(mod1)

# 2x2 plot for the residuals
par(mfrow=c(2,2))
#  Runing gam.check on our original model
gam.check(mod4,pch=20)
gam.check(mod1,pch=20)

AIC(mod4)
AIC(mod1)

predicted_TB_Risk <- predict(mod4, newdata = TBdata_train, type = "response")
plot.map(x = predicted_TB_Risk, main = "Predicted TB Risk", n.levels = 7, cex = 1)

# PLotting map of cases
plot.map(TBdata$TB[TBdata$Year==2014],n.levels=7,main="TB counts for 2014")

#final model
modf = gam(TB ~ offset(log(Population))+ s(Illiteracy, k = 10, bs = "cr") + s(Indigenous, k = 10, bs = "cr") + s(Urbanisation, k = 10, bs = "cr") +
             s(Density, k = 10, bs = "cr")+ s(Poverty, k = 10, bs = "cr") + s(Poor_Sanitation, k = 10, bs = "cr") + s(Unemployment, k = 10, bs = "cr") + s(Timeliness, k = 10, bs = "cr") + Region + s(lon, lat), 
           data = TBdata, 
           family = nb(link = 'log'))

# Identify regions with high TB risk
pred <- predict(modf, type="response")
TBdata$risk <- pred / TBdata$Population   # calculate TB risk
high_risk <- subset(TBdata, risk > quantile(risk, 0.95))   # regions with top 5% TB risk
high_risk

# PLotting map of cases
predicted_TB_Risk <- predict(modf, newdata = TBdata, type = "response")
plot.map(x = predicted_TB_Risk, main = "Predicted TB Risk", n.levels = 7, cex = 1)


