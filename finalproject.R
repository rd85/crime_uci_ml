# IMPORT
# *****************************************
# install and import libraries

if(!require("tidyverse")){install.packages("tidyverse")}
if(!require("readr")){install.packages("readr")}
if(!require("ggpubr")){install.packages("ggpubr")}
if(!require("PerformanceAnalytics")){install.packages("PerformanceAnalytics")}

library(tidyverse)
library(readr)
library(ggpubr)
library(PerformanceAnalytics)

# import text file
setInternet2(use = TRUE)
crimedataraw <-
  read.csv(
    "http://archive.ics.uci.edu/ml/machine-learning-databases/00211/CommViolPredUnnormalizedData.txt",
    header = FALSE,
    sep = ",",
    quote = "\"",
    dec = ".",
    fill = TRUE,
    comment.char = "",
    stringsAsFactors = default.stringsAsFactors()
  )

crimedatacleaned <-
  read.csv(
    "http://archive.ics.uci.edu/ml/machine-learning-databases/00211/CommViolPredUnnormalizedData.txt",
    header = FALSE,
    sep = ",",
    quote = "\"",
    dec = ".",
    fill = TRUE,
    comment.char = "",
    na.strings = "?",
    strip.white = TRUE,
    stringsAsFactors = default.stringsAsFactors()
  )

# import sate region mapping data set
# download the mapping file from [https://github.com/rd85/crime_uci_ml/blob/master/state_region_mapping.xlsx]
library(readxl)
state_region_mapping <- 
  read_excel("~/Harrisburg Uni/4. EDA/Final Project/state_region_mapping.xlsx")


# DATA WRANGLING
# *****************************************

# add column name to the data frame
colnames(crimedatacleaned) <-
  c(
    "communityname",
    "state",
    "countycode",
    "communitycode",
    "fold",
    "population",
    "householdsize",
    "racepctblack",
    "racepctwhite",
    "racepctasian",
    "racepcthisp",
    "agepct12t21",
    "agepct12t29",
    "agepct16t24",
    "agepct65up",
    "numburban",
    "pcturban",
    "medincome",
    "pctwwage",
    "pctwfarmself",
    "pctwinvinc",
    "pctwsocsec",
    "pctwpubasst",
    "pctwretire",
    "medfaminc",
    "percapinc",
    "whitepercap",
    "blackpercap",
    "indianpercap",
    "asianpercap",
    "otherpercap",
    "hisppercap",
    "numunderpov",
    "pctpopunderpov",
    "pctless9thgrade",
    "pctnothsgrad",
    "pctbsormore",
    "pctunemployed",
    "pctemploy",
    "pctemplmanu",
    "pctemplprofserv",
    "pctoccupmanu",
    "pctoccupmgmtprof",
    "malepctdivorce",
    "malepctnevmarr",
    "femalepctdiv",
    "totalpctdiv",
    "persperfam",
    "pctfam2par",
    "pctkids2par",
    "pctyoungkids2par",
    "pctteen2par",
    "pctworkmomyoungkids",
    "pctworkmom",
    "numkidsbornnevermar",
    "pctkidsbornnevermar",
    "numimmig",
    "pctimmigrecent",
    "pctimmigrec5",
    "pctimmigrec8",
    "pctimmigrec10",
    "pctrecentimmig",
    "pctrecimmig5",
    "pctrecimmig8",
    "pctrecimmig10",
    "pctspeakenglonly",
    "pctnotspeakenglwell",
    "pctlarghousefam",
    "pctlarghouseoccup",
    "persperoccuphous",
    "persperownocchous",
    "persperrentocchous",
    "pctpersownoccup",
    "pctpersdensehous",
    "pcthousless3br",
    "mednumbr",
    "housvacant",
    "pcthousoccup",
    "pcthousownocc",
    "pctvacantboarded",
    "pctvacmore6mos",
    "medyrhousbuilt",
    "pcthousnophone",
    "pctwofullplumb",
    "ownocclowquart",
    "ownoccmedval",
    "ownocchiquart",
    "ownoccqrange",
    "rentlowq",
    "rentmedian",
    "renthighq",
    "rentqrange",
    "medrent",
    "medrentpcthousinc",
    "medowncostpctinc",
    "medowncostpctincnomtg",
    "numinshelters",
    "numstreet",
    "pctforeignborn",
    "pctbornsamestate",
    "pctsamehouse85",
    "pctsamecity85",
    "pctsamestate85",
    "lemasswornft",
    "lemasswftperpop",
    "lemasswftfieldops",
    "lemasswftfieldperpop",
    "lemastotalreq",
    "lemastotreqperpop",
    "policreqperoffic",
    "policperpop",
    "racialmatchcommpol",
    "pctpolicwhite",
    "pctpolicblack",
    "pctpolichisp",
    "pctpolicasian",
    "pctpolicminor",
    "officassgndrugunits",
    "numkindsdrugsseiz",
    "policaveotworked",
    "landarea",
    "popdens",
    "pctusepubtrans",
    "policcars",
    "policoperbudg",
    "lemaspctpoliconpatr",
    "lemasgangunitdeploy",
    "lemaspctofficdrugun",
    "policbudgperpop",
    "murders",
    "murdperpop",
    "rapes",
    "rapesperpop",
    "robberies",
    "robbbperpop",
    "assaults",
    "assaultperpop",
    "burglaries",
    "burglperpop",
    "larcenies",
    "larcperpop",
    "autotheft",
    "autotheftperpop",
    "arsons",
    "arsonsperpop",
    "violentcrimesperpop",
    "nonviolperpop"
  )

# #rename the dataframe
# crime <- crimedatacleaned
#
# # summarize the data frame
# summary(crimedatacleaned)
#
# # check junk values
# # crime_df %>%
# #   filter(countycode == '?')
#
# # replace ? with NA
# crime_df[crime_df == '?'] <- NA


# create a dataframe with variables relevant for our analysis
crime <-
  crimedatacleaned %>%
  select(
    state,
    communityname,
    population,
    numimmig,
    pctimmigrecent,
    pctimmigrec5,
    pctimmigrec8,
    pctimmigrec10,
    pctrecentimmig,
    pctrecimmig5,
    pctrecimmig8,
    pctrecimmig10,
    violentcrimesperpop,
    nonviolperpop
  )

# view summary of the newly created data frame
summary(crime)

# remove na values in violent and non violent crime variable
# since there are only ~300 rows, we would not compromise data's integrity
crime <- 
  crime %>%
  filter(!is.na(violentcrimesperpop)) %>%
  filter(!is.na(nonviolperpop))

# add nonviolent anc violent crime to come up with total crime per 100k pop
crime <- 
  crime %>%
  mutate(totcrimeperpop = violentcrimesperpop+nonviolperpop)

# join crime dataset with state_region_mapping to identify regions in united states
crime <- 
  left_join(crime, state_region_mapping,  by = c("state" = "state"))

# analyze missing values
# DC is not being recognized into any region because it is not a state
crime %>%
  group_by(region) %>%
  summarise(
    population = sum(population),
    numimmig = sum(numimmig),
    totcrimeperpop = sum(totcrimeperpop)
  )

# add DC to North East, since we know from earlier 
# analysis that issue persists only with DC
crime <-
  crime %>%
  mutate(region = ifelse(is.na(region), "Northeast", region))

# total crime national level
crime %>%
  summarise(
    mean_totcrimeperpop = mean(totcrimeperpop),
    median_totcrimeperpop = median(totcrimeperpop)
  )

# total crime per region
crime %>%
  group_by(region) %>%
  summarise(
    population = sum(population),
    numimmig = sum(numimmig),
    totcrimeperpop = sum(totcrimeperpop),
    mean_totcrimeperpop = mean(totcrimeperpop)
  )

# VISUALIZATION
# *****************************************

# box plot to identify outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(crime$numimmig, main="Num of Immigrants") 
boxplot(crime$totcrimeperpop, main="Total Crime")  

# denisty plot - check if the response variable is close to normality
par(mfrow=c(1, 2)) 

plot(density(crime$numimmig), main="Density Plot: numimmig", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(crime$numimmig), 2)))  # density plot for 'speed'
polygon(density(crime$numimmig), col="red")

plot(density(crime$totcrimeperpop), main="Density Plot: totcrimeperpop", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(crime$totcrimeperpop), 2)))  # density plot for 'speed'
polygon(density(crime$totcrimeperpop), col="red")

# scatter plot
# given the density of the points, this does not reveal any meaningful information
pairs(~population+numimmig+totcrimeperpop,
      data=crime,
      main="Scatterplot of population, immigrant, total crime")

# scatter Plot of Total Crime and Population by Region
# change y scale to log for better visualization
crime_ne <-
  crime %>%
  filter(region == "Northeast") %>%
  ggplot(aes(x = totcrimeperpop, y = population)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Population") +
  scale_y_continuous(trans = 'log2') +
  theme_minimal()
crime_mw <-
  crime %>%
  filter(region == "Midwest") %>%
  ggplot(aes(x = totcrimeperpop, y = population)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Population") +
  scale_y_continuous(trans = 'log2') +
  theme_minimal()
crime_s <-
  crime %>%
  filter(region == "South") %>%
  ggplot(aes(x = totcrimeperpop, y = population)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Population") +
  scale_y_continuous(trans = 'log2') +
  theme_minimal()
crime_w <-
  crime %>%
  filter(region == "West") %>%
  ggplot(aes(x = totcrimeperpop, y = population)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Population") +
  scale_y_continuous(trans = 'log2') +
  theme_minimal()

ggarrange(crime_ne, crime_mw, crime_s, crime_w, 
          labels = c("NorthEast", "MidWest", "South", "West"),
          ncol = 2, nrow = 2) %>%
  annotate_figure(top = text_grob("Scatter plot of Total Crime and Population", face = "bold", size = 14))

# scatter Plot of Total Crime and Immigrant population by Region
# change y scale to log for better visualization
crime_immi_ne <-
  crime %>%
  filter(region == "Northeast") %>%
  ggplot(aes(x=totcrimeperpop, y=numimmig)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Immigrant population") +
  scale_y_continuous(trans = 'log2') +
  theme_minimal()
crime_immi_mw <-
  crime %>%
  filter(region == "Midwest") %>%
  ggplot(aes(x=totcrimeperpop, y=numimmig)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Immigrant population") +
  scale_y_continuous(trans = 'log2') +
  theme_minimal()
crime_immi_s <-
  crime %>%
  filter(region == "South") %>%
  ggplot(aes(x=totcrimeperpop, y=numimmig)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Immigrant population") +
  scale_y_continuous(trans = 'log2') +
  theme_minimal()
crime_immi_w <-
  crime %>%
  filter(region == "West") %>%
  ggplot(aes(x=totcrimeperpop, y=numimmig)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Immigrant population") +
  scale_y_continuous(trans = 'log2') +
  theme_minimal()


ggarrange(crime_immi_ne, crime_immi_mw, crime_immi_s, crime_immi_w, 
          labels = c("NorthEast", "MidWest", "South", "West"),
          ncol = 2, nrow = 2) %>%
  annotate_figure(top = text_grob("Scatter plot of Total Crime and Immigrant Population", face = "bold", size = 14))


# from the above graphs one might think that there seems to be some
# positive correlation of crime with immigrant population. however, it is 
# so easy to lie with numbers and visuals. being data sceientists, we need to
# critically think and ask the right questions. if analyzed on percentage basis,
# does our analysis tell the same story? create new columns, pct immigrants and
# pct total crime when compared to total population
crime <-
  crime %>%
  mutate(
    pctnumimmig = (numimmig / population) * 100,
    pcttotcrimeperpop = (totcrimeperpop / population) * 100)

# scatter Plot of Total Crime and Immigrant population by Region on percentage basis
# log y scale is not needed as we already have data in percentages
# we prove the hypothesis that it is easy to lie with data. when analyzed on 
# percentage basis, there seems to be a negative trend

pct_crime_immi_ne <-
  crime %>%
  filter(region == "Northeast") %>%
  ggplot(aes(x=pcttotcrimeperpop, y=pctnumimmig)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Immigrant population") +
  theme_minimal()
pct_crime_immi_mw <-
  crime %>%
  filter(region == "Midwest") %>%
  ggplot(aes(x=pcttotcrimeperpop, y=pctnumimmig)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Immigrant population") +
  theme_minimal()
pct_crime_immi_s <-
  crime %>%
  filter(region == "South") %>%
  ggplot(aes(x=pcttotcrimeperpop, y=pctnumimmig)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Immigrant population") +
  theme_minimal()
pct_crime_immi_w <-
  crime %>%
  filter(region == "West") %>%
  ggplot(aes(x=pcttotcrimeperpop, y=pctnumimmig)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = lm, color="black") + # Add linear regression line
  labs(x = "Total Crime",  y = "Immigrant population") +
  theme_minimal()

ggarrange(pct_crime_immi_ne, pct_crime_immi_mw, pct_crime_immi_s, pct_crime_immi_w, 
          labels = c("NorthEast", "MidWest", "South", "West"),
          ncol = 2, nrow = 2) %>%
  annotate_figure(top = text_grob("Total Crime and Immigrant Population - Pct Basis", face = "bold", size = 14))


# MODELING
# *****************************************

# identify correlations
#import cooplot library
library(corrplot)

# create a new data frame to handle only continous variables
cor_crime <-
  crime %>%
  select(
    region,
    population,
    numimmig,
    totcrimeperpop)

# plot correlation
corrplot(cor(cor_crime), method = "number")

# plot correlation for each region
cor_crime_ne <-
  cor_crime %>%
  filter(region == "Northeast") %>%
  select(population,
         numimmig,
         totcrimeperpop) %>%
  cor()
cor_crime_mw <-
  cor_crime %>%
  filter(region == "Midwest") %>%
  select(population,
         numimmig,
         totcrimeperpop) %>%
  cor()
cor_crime_s <-
  cor_crime %>%
  filter(region == "South") %>%
  select(population,
         numimmig,
         totcrimeperpop) %>%
  cor()
cor_crime_w <-
  cor_crime %>%
  filter(region == "West") %>%
  select(population,
         numimmig,
         totcrimeperpop) %>%
  cor()

# corrplots for each region
cor_crime_ne %>%
  corrplot(method = "number")
cor_crime_mw %>%
  corrplot(method = "number")
cor_crime_s %>%
  corrplot(method = "number")
cor_crime_w %>%
  corrplot(method = "number")

# build a linear regression model
linearmod <- lm(totcrimeperpop ~ numimmig, data=crime) 

summary(linearmod)

# create linear regression models for each region
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# create a new data frame to handle only continous variables for each region
cor_crime_ne <-
  crime %>%
  filter(region == "Northeast") %>%
  select(population, numimmig, totcrimeperpop)
cor_crime_mw <-
  crime %>%
  filter(region == "Midwest") %>%
  select(population, numimmig, totcrimeperpop)
cor_crime_s <-
  crime %>%
  filter(region == "South") %>%
  select(population, numimmig, totcrimeperpop)
cor_crime_w <-
  crime %>%
  filter(region == "West") %>%
  select(population, numimmig, totcrimeperpop)

# create correlation charts for each region
chart.Correlation(cor_crime_ne, histogram=TRUE, pch=19) #Northeast
chart.Correlation(cor_crime_mw, histogram=TRUE, pch=19) #Midwest
chart.Correlation(cor_crime_s, histogram=TRUE, pch=19) #South
chart.Correlation(cor_crime_w, histogram=TRUE, pch=19) #West

# build a linear regression model for each region
lm_ne <- lm(totcrimeperpop ~ numimmig, data=cor_crime_ne) 
lm_mw <- lm(totcrimeperpop ~ numimmig, data=cor_crime_mw) 
lm_s <- lm(totcrimeperpop ~ numimmig, data=cor_crime_s) 
lm_w <- lm(totcrimeperpop ~ numimmig, data=cor_crime_w) 

# summary stats for lm for each region
summary(lm_ne)
summary(lm_mw)
summary(lm_s)
summary(lm_w)
