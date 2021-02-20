rm(list=ls())

setwd("C:/Users/rasim/Desktop/MPP2/Populism/final paper/")

# df <- read.csv("president_county.csv")
anes <- read.csv("anes_pilot_2020ets_csv.csv")

df <- subset(anes, select=c(follow, reg1, votecount,
                            turnout16a, turnout16b, 
                            vote16, hopeful,afraid, outraged,
                            angry, happy, worried, proud, irritated,
                            nervous, talk1,fttrump1, ftobama1,
                            ftbiden1, ftwarren1, ftsanders1, 
                            ftharris1, ftpence1, ftocasioc1, 
                            ftblack, ftwhite, fthisp, ftasian, ftillegal, 
                            ftfeminists, ftmetoo, fttransppl, ftsocialists,
                            ftcapitalists, ftbigbusiness, ftlaborunions,
                            ftrepublicanparty, ftdemocraticparty, primaryvote,
                            vote20jb, cvote2020, apppres7, immpres7,
                            econpres7, covidpres7, healthcarepres7,
                            econnow, finworry, confecon, taxecon, 
                            billtax1, billtax2, richpoor2, lcself, lcd, lcr,
                            freemkt1a, govsize1a, regulate1, leastgrp,
                            tol_rally, tol_pres, tol_teach, immignum, wall7,
                            pathway, return, open, affact, hlthcare1, hlthcare2, 
                            covid1, covid2, abort1, abort_imp, abort2, freecol1,
                            freecol2, diversity7, excessive, rural2, rural3, rural4,
                            pid1r, pidlean, pidstr1, pidstr2, pidstr3, rr1, rr2, rr3, rr4, latin1, race1a_1, race1a_2,
                            race1a_3, race1a_4, race1a_5, whitejob,
                            femid1a, stress1, depress1, inc_anes, birthyr, sex, educ))
#Follow - follow politics
#reg1 - registered to vote
#votecount = how accurately votes will be counted
#turnout16a - turned out to vote or not in 2016
#turnout16b - check if they voted
#vote 16 : 1 - Trump; 2 - Clinton; 3 - Someone Else
#talk1 - how many days in the past week did you talk
#about politics with family or friends
#apppres7 - how do you approve of Donald Trump's job as president
#affact - affirmative action

colnames(df)

inc <- subset(anes, select=c(inc_anes, inc_cps, inc_cpsmod))
inc$income <- inc$inc_anes + inc$inc_cps + inc$inc_cpsmod - (66*2)

table(inc$income)

stud <- subset(df, select=c(vote16, fttrump1, ftbiden1, vote20jb, birthyr, sex, educ, latin1, race1a_1, race1a_2, race1a_3, race1a_4, race1a_5,
                            pid1r, pidlean, pidstr1, pidstr2, pidstr3, rural2, rural3, rural4, taxecon, econnow, finworry, confecon, lcself, lcd, lcr,
                            pathway, return, open, affact, covid1, covid2, abort2, diversity7, rr1, rr2, rr3, rr4))

#get the income data
stud$income <- inc$income

#Fix the age
stud$age <- 2020- stud$birthyr

#Fix the gender: 1 - Female; 0 - Male
stud$sex <- stud$sex -1

#Get outcome variable for Trump 2020 and Trump 2016
stud$trump20 <- as.numeric(stud$vote20jb == 1)
stud$trump16 <- as.numeric(stud$vote16 == 1)

#Get the support features
stud$trump_sup <- as.numeric(stud$fttrump1/100)
stud$biden_sup <- as.numeric(stud$ftbiden1/100)

nrow(stud)

#Removing missing values for Trump and Biden support
stud<- stud[!(stud$trump_sup > 1 | stud$biden_sup > 1),]

nrow(stud)

#Create an outcome variable where if Trump has more support than Biden the variable is a 1
stud$trump20a <- as.numeric(stud$trump_sup > stud$biden_sup)

table(stud$trump20a)
table(stud$vote20jb)

#Race
stud$latin <- as.numeric(stud$latin1 == 1)
stud$white <- as.numeric(stud$race1a_1 == 1)
stud$black <- as.numeric(stud$race1a_2 == 1)
stud$other_race <- as.numeric(stud$race1a_3 ==1 | stud$race1a_4 ==1 | stud$race1a_5 ==1)

table(stud$pid1r)

#Partisanship
table(stud$pidlean)
stud$pid1r[(stud$pid1r ==3)] <- stud$pidlean[(stud$pid1r ==3)]
stud$dem <- as.numeric(stud$pid1r == 2)
stud$rep <- as.numeric(stud$pid1r == 1)
stud$other <- as.numeric(stud$pid1r == 3 | stud$pid1r == 4 | stud$pid1r == 9)

#Flip the pidstir1 for consistency with pidstr2 and pidstr3
stud$pidstr1a[stud$pidstr1==1] <- 5
stud$pidstr1a[stud$pidstr1==2] <- 4
stud$pidstr1a[stud$pidstr1==3] <- 3
stud$pidstr1a[stud$pidstr1==4] <- 2
stud$pidstr1a[stud$pidstr1==5] <- 1

#Fill the NAs with 6 and then change to mean of 3
stud$pidstr1a[is.na(stud$pidstr1a)] <- 6

#Change the missing values to the mean 3
stud$pidstr1a[stud$pidstr1a==6] <- 3
stud$pidstr2[stud$pidstr2==6 | stud$pidstr2==9 ] <- 3
stud$pidstr3[stud$pidstr3==6 | stud$pidstr3==9 ] <- 3

#check the numbers
table(stud$pidstr1a)
table(stud$pidstr2)
table(stud$pidstr3)

#Final partisanship variable
stud$partisan <- round((stud$pidstr1a + stud$pidstr2 + stud$pidstr3)/3,2)

#Rural Resentment
stud$rural_resent <- round((stud$rural2 + stud$rural3 + stud$rural4)/3,2)

#Economic Resentment
stud$econ <- round((stud$econnow + stud$finworry + stud$confecon)/3,2)

#Tax
#Just use taxecon

#Ideology
#just use lcself

#Immigration
stud$returna[stud$return==1] <- 5
stud$returna[stud$return==2] <- 4
stud$returna[stud$return==3] <- 3
stud$returna[stud$return==4] <- 2
stud$returna[stud$return==5] <- 1

#final anti-immigrant sentiment score
stud$antimig <- round((stud$returna + stud$pathway + stud$open)/3,2)

#covid_worry
stud$covid_nw <- round((stud$covid1 +stud$covid2)/2,2)

#Single issue subjects: 
#Opposing affermative action: 
# use affact: the higher affact, the more opposed

#Supporting abortion
# use abort2: the higher the abort2, the more support of abortion

#Opposing multiculturalism
# use diversity7: the higher the diversity7, the more opposed

#Lastly, racial resentment
stud$rr1a[stud$rr1==1] <- 5
stud$rr1a[stud$rr1==2] <- 4
stud$rr1a[stud$rr1==3] <- 3
stud$rr1a[stud$rr1==4] <- 2
stud$rr1a[stud$rr1==5] <- 1

stud$rr4a[stud$rr4==1] <- 5
stud$rr4a[stud$rr4==2] <- 4
stud$rr4a[stud$rr4==3] <- 3
stud$rr4a[stud$rr4==4] <- 2
stud$rr4a[stud$rr4==5] <- 1

#Final racial resentment variable
stud$race_resent <- round((stud$rr1a + stud$rr2 + stud$rr3 + stud$rr4a)/4,2)

#Final dataframe
fm <- subset(stud, select = c(trump20a, age, sex, income, educ, latin, white, black, other_race, partisan,
                              dem, rep, other, rural_resent, econ, taxecon, lcself, antimig, covid_nw, affact,
                              abort2, diversity7, race_resent))

#Check the balance of the outcome classes
table(fm$trump20a)

#check for missng data
apply(is.na(fm),2,sum)

#remove missing data
fm <- fm[complete.cases(fm), ]

#3052 observaations


#First logistic regression:
#Model 1: Demographics

model1 <- glm(trump20a ~ age + sex + income + educ + latin + white + black, data = fm, family = binomial)
summary(model1)

pR2 = 1 - model1$deviance / model1$null.deviance
print(pR2)

#Model 1 - Demographics: shows that you are much less likely to support trump if you are female, are less educated, are republican, or if you're non-black.

#Model 2: Demographics + Ideology
model2 <- glm(trump20a ~ age + sex + income + educ + latin + white + black + 
                partisan + lcself, data = fm, family = binomial)

summary(model2)


pR2 = 1 - model2$deviance / model2$null.deviance
print(pR2)

# Model 2 shows that age and being a republican voter is indicative of supporting Trump. Black voters have negative association with Trump support, and 
# and being conservative has positive association

#Model 3: Demographics + Ideology + Frustrations:

model3 <- glm(trump20a ~ age + sex + income + educ + latin + white + black +
                partisan + lcself + rural_resent + econ + taxecon + covid_nw, data = fm, family = binomial)

pR2 = 1 - model3$deviance / model3$null.deviance
print(pR2)

summary(model3)

#Model 3 - Important predictrs are now education, being republican, white, black, conservative ideology, 
# economic frustrations, and taxes. Covid is surprisingly not a factor. 

#Model 4: Demographics + Ideology + Frustrations + Single-Issue

model4 <- glm(trump20a ~ age + sex + income + educ + latin + white + black +
                partisan + lcself + rural_resent + econ + taxecon + covid_nw + 
                affact + diversity7 + abort2, data = fm, family = binomial)

pR2 = 1 - model4$deviance / model4$null.deviance
print(pR2)

summary(model4)

#Interestingly single issue predictors all show statistical significance with Trump support. Affirmative Action, 
# Diversity, and Abortion Rights are all well correlated.

#Model 5: Demographics + Ideology + Frustrations + Single-Issue + Racial Resentment/Anti-immigration

model5 <- glm(trump20a ~ age + sex + income + educ + latin + white + black +
                partisan + lcself + rural_resent + econ + taxecon + covid_nw + 
                affact + diversity7 + abort2 + race_resent + antimig, data = fm, family = binomial)

pR2 = 1 - model5$deviance / model5$null.deviance
print(pR2)

summary(model5)

#Interestingly, now that race resentment and anti immigration sentiment variables have been added, the predictors
# that still have an association is abortion, race resentment, and anti immigration sentiments. Other than that, 
# ideology, being republican, ecocnomic frustrations, and taxation issues are all important predictors of trump support.



