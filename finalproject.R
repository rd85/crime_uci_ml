# IMPORT
# *****************************************
# import libraries
library(tidyverse)
library(readr)

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

# VISUALIZATION
# *****************************************

# scatter plot
pairs(~population+numimmig+totcrimeperpop,
      data=crime,
      main="Scatterplot of population, immigrant, total crime")

# viz
crime %>%
  ggplot(aes(x=totcrimeperpop, y=population)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) + # Add linear regression line 
  ggtitle("Scatter Plot of Total Crime and Population") +
  labs(x = "Total Crime",  y = "Population") +
  theme_minimal()

# viz
crime %>%
  ggplot(aes(x=totcrimeperpop, y=numimmig)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm) + # Add linear regression line 
  ggtitle("Scatter Plot of Total Crime and Immigrant population") +
  labs(x = "Total Crime",  y = "Immigrant population") +
  theme_minimal() 


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


# MODELING
# *****************************************

# identify correlations
#import cooplot library
library(corrplot)

# create a new data frame to handle only continous variables
cor_crime <-
  crime %>%
  select(
    population,
    numimmig,
    totcrimeperpop)

# plot correlation
corrplot(cor(cor_crime), method = "number")

# build a linear regression model
linearmod <- lm(numimmig ~ totcrimeperpop, data=crime) 

summary(linearmod)

