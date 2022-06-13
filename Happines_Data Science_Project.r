

#Luis Martinez

### ANALYSIS ###


# importing data:

happy2015 <- read.csv('2015.csv')
happy2016 <- read.csv('2016.csv')
happy2017 <- read.csv('2017.csv')

# checking data:

head(happy2015)

# recoding columns names for data 2015:

colnames(happy2015) <- c('Country','Region','Hrank','Hscore','Stnderror','GDP','family','lifexp','Freedom','Trust','Generosity','Resid')

# checking:

head(happy2015)

# recoding columns names for data 2016:

colnames(happy2016) <- c('Country','Region','Hrank','Hscore','lowconf','uppconf','GDP','family','lifexp','Freedom','Trust','Generosity','Resid')

# checking:

head(happy2016)

# recoding columns names for data 2017:

colnames(happy2017) <- c('Country','Hrank','Hscore','Whiskerhigh','Whiskerlow','GDP','family','lifexp','Freedom','Generosity','Trust','Resid')

# checking:

head(happy2017)

# installing knitr package:

if(!require(knitr))install.packages("knitr")

# calling knitr library:

library('knitr')

# checking type of variables for dataset 2015:

variables2015 <- sapply(happy2015, class)
kable(data.frame(var=names(variables2015), class=as.vector(variables2015)))

# checking type of variables for dataset 2016:

variables2016 <- sapply(happy2016, class)
kable(data.frame(var=names(variables2016), class=as.vector(variables2016)))

# checking type of variables for dataset 2017:

variables2017 <- sapply(happy2017, class)
kable(data.frame(var=names(variables2017), class=as.vector(variables2017)))

# creating a new dataframe to make a fusion of the three dataframes:

case2015 <- data.frame(Country = happy2015$Country, Hscore = happy2015$Hscore)
case2016 <- data.frame(Country = happy2016$Country, Hscore = happy2016$Hscore)
case2017 <- data.frame(Country = happy2017$Country, Hscore = happy2017$Hscore)

merges <- merge( x = case2015, y = case2016, by = 'Country', all = TRUE)
Countries <- merge( x = merges, y = case2017, by = 'Country', all = TRUE)

colnames(Countries) <- c('Country','Hscore2015','Hscore2016','Hscore2017')

head(Countries)

# chacking new dataframe:

str(Countries)

# creating the new dataframe merging all to do a analysis after:

case2015_2 <- data.frame(Country = happy2015$Country, Hscore = happy2015$Hscore, Group = '2015')
case2016_2 <- data.frame(Country = happy2016$Country, Hscore = happy2016$Hscore, Group = '2016')
case2017_2 <- data.frame(Country = happy2017$Country, Hscore = happy2017$Hscore, Group = '2017')

total <- rbind(case2015_2, case2016_2)
Countries2 <- rbind(total, case2017_2)



# checking the new dataframe:

head(Countries2)

# creating groups by year and creating the boxplot for the visualization 
# of the happines distribution during the three years:

groups2015 <- subset(Countries2$Hscore, Countries2$Group == '2015')
groups2016 <- subset(Countries2$Hscore, Countries2$Group == '2016')
groups2017 <- subset(Countries2$Hscore, Countries2$Group == '2017')
boxplot(Countries2$Hscore~Countries2$Group, main='Happiness distribution during the three years', names = c('2015','2016','2017'))

#chacking the anova for differences between years:

Hscoreaov <- aov(Countries2$Hscore~Countries2$Group, data=Countries2)
Hscoreaov

#Summary of the anova:

summary (aov(Countries2$Hscore~Countries2$Group))

#installing RcmdrMisc package:

if(!require(RcmdrMisc))install.packages("RcmdrMisc")

# calling RcmdrMisc library:

library('RcmdrMisc')

# using numSummary to summarize the data:

numSummary(Countries2$Hscore, groups=Countries2$Group, statistics=c('mean','sd'))

# checking regions quantities:

table(happy2015$Region)

# Recoding regions names to work easier:

happy2015$Region <- factor(happy2015$Region, levels=c('Australia and New Zealand','Eastern Asia','Middle East and Northern Africa',
                                                     'Southeastern Asia','Sub-Saharan Africa','Central and Eastern Europe',
                                                     'Latin America and Caribbean','North America','Southern Asia','Western Europe'),
                          labels=c('Oce','EA','MeNa','SA','SubA','CEE','LAC','NAM','SASI','WE'))

# visualizing differences hapiness between regions:

boxplot(happy2015$Hscore~happy2015$Region, plot=TRUE,main='Distribution happiness by Regions in 2015')

# calculation of ANOVA between all regions:

Hscore2015 <- aov(happy2015$Hscore~happy2015$Region)
Hscore2015

# summary of the anova:

summary(Hscore2015)

# using numSummary to summarize the data:

numSummary(happy2015$Hscore, groups=happy2015$Region, statistics = c('mean','sd'))

#looking at the effects:

model.tables(Hscore2015)

# checking normality with qqnorm:

qqnorm(happy2015$Hscore)

# chacking homogeneity of variances using the bartlett test:

bartlett.test(happy2015$Hscore,happy2015$Region)

head(happy2015,1)

# creating the multiple linear regression model:

ModeloRM <- lm(Hscore~GDP+family+lifexp+Freedom+Trust+Generosity, data = happy2015)
summary(ModeloRM)

# lets do analysis of the residuals

par(mfrow=c(2,2))
modelresiduals <- lm(Hscore~GDP+family+lifexp+Freedom+Trust+Generosity, data = happy2015)
plot(modelresiduals)

# adding nominal variable to the model

ModeloRM2 <- lm(Hscore~GDP+family+lifexp+Freedom+Trust+Generosity+Region, data = happy2015)
summary(ModeloRM2)


