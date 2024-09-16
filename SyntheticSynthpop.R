########## Part A ##############
set.seed(123)
## generate normal distribution for A
x <- rnorm(3000,mean = 120, sd = 5)
y <- 'A'
df <- data.frame(x,y)

## generate normal distribution for B
x1 = rnorm(2500,mean = 500, sd = 50)
y1 <- 'B'
df1 <- data.frame(x1,y1)

## generate normal distribution for C
x2 = rnorm(4500,mean = 5000, sd = 300)
y2 <- 'C'
df2 <- data.frame(x2,y2)

## combine A, B, C together into a data frame
names(df1) <- c("x", "y")
names(df2) <- c("x", "y")
new<-rbind(df,df1,df2)

head(new)
View(new)
str(new)

myseed=1020
##Apply Synthpop package
library(synthpop)
##Do Synthetic data
synth.obj <- syn(new, method = "cart",seed = myseed)
s_new <- data.frame(synth.obj$syn)
s_new 

## Distribution of X for original data
hist(new$x, main = "Distribution of variable X for original data", col = "Blue")
## Distribution of X for synthesized data
hist(s_new$x, main = "Distribution of X for synthesized data", col="blue")

##QQ-plot of original X
qqnorm(new$x, pch = 1, frame = FALSE, main = "QQ-plot of original X", col = "lightblue")
qqline(new$x, col = "blue", lwd = 2)
##QQ-plot of synthesized X
qqnorm(s_new$x, pch = 1, frame = FALSE, main = "QQ-plot of synthesized X", col = "lightblue")
qqline(s_new$x, col = "blue", lwd = 2)

library(dplyr)
library(psych)
## Histograms of A
A.new <- s_new%>% filter(s_new$y == 'A')
hist(A.new$x, main = "Synthetic Normal DIstribution of A")
hist(x, main = "Original Normal DIstribution of A")
dim(A.new)
describe(A.new)
## QQ-plots of A
qqnorm(df$x, pch = 1, frame = FALSE, main = "QQ-plot of original A", col = "lightblue")
qqline(df$x, col = "blue", lwd = 2)
qqnorm(A.new$x, pch = 1, frame = FALSE, main = "QQ-plot of synthesized A", col = "lightblue")
qqline(A.new$x, col = "blue", lwd = 2)

## Histograms of B
B.new <- s_new%>% filter(s_new$y == 'B')
hist(x1, main = "Original Normal Distribution of B")
hist(B.new$x,main = "Synthetic Normal DIstribution of B")
dim(B.new)
describe(B.new)
## QQ-plots of B
qqnorm(df1$x, pch = 1, frame = FALSE, main = "QQ-plot of original B", col = "lightblue")
qqline(df1$x, col = "blue", lwd = 2)
qqnorm(B.new$x, pch = 1, frame = FALSE, main = "QQ-plot of synthesized B", col = "lightblue")
qqline(B.new$x, col = "blue", lwd = 2)

## Histograms of C
C.new <- s_new%>% filter(s_new$y == 'C')
hist(C.new$x, main = "Synthetic Normal DIstribution of C")
hist(x2, main = "Original Normal DIstribution of C")
dim(C.new)
describe(C.new)
##QQ-plots of C
qqnorm(df2$x, pch = 1, frame = FALSE, main = "QQ-plot of original C", col = "lightblue")
qqline(df2$x, col = "blue", lwd = 2)
qqnorm(C.new$x, pch = 1, frame = FALSE, main = "QQ-plot of synthesized C", col = "lightblue")
qqline(C.new$x, col = "blue", lwd = 2)


#ks test for A
ks.test(df$x,A.new$x)
plot.ecdf(df$x, verticals=TRUE, do.points=FALSE, col="red")
lines(ecdf(A.new$x), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for B
ks.test(df1$x,B.new$x)
plot.ecdf(df1$x, verticals=TRUE, do.points=FALSE, col="red")
lines(ecdf(B.new$x), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for C
ks.test(df2$x,C.new$x)
plot.ecdf(df2$x, verticals=TRUE, do.points=FALSE, col="red")
lines(ecdf(C.new$x), verticals=TRUE, do.points=FALSE, col="blue")

### Regression models
##The original data
model = lm(x~y, data = new)
summary(model)

#The synthetic data
model_syn = lm(x~y, data = s_new)
summary(model_syn)

#checking the assumptions
par(mfrow=c(2,2))
plot(model, main= "Residual plot for original model")
par(mfrow=c(1,1))

#checking the assumptions
par(mfrow=c(2,2))
plot(model_syn, main= "Residual plot for synthesized model")
par(mfrow=c(1,1))

########## Part B ##############
##Library Import##
library(synthpop)
library(tidyverse)
library(sampling)
library(rapportools)
library(dplyr)
library(caret)
library(vcd)
library(GGally)
library(forcats)
library(car)

#pt0 = proc.time()

##Import Dataset##
HI<-read.csv('health_ insurance.csv')
view(HI)
str(HI)

##Missing value and blank spaces checking##
colSums(is.na(HI))
colSums(is.empty(HI))

##Set up dataset into data frame##
HI<-as.data.frame(HI)
myseed=1337


##Apply synthpop
library(synthpop)

ptm <- proc.time()   # reading time of generating data
synth.obj <- syn(HI,drop.not.used = TRUE, minnumlevels=3, seed = myseed)
proc.time() - ptm

synth.obj$predictor.matrix
synth.obj$method
syn1<-synth.obj$syn
view(syn1)

compare(synth.obj,HI,ncol = 2)
utility.gen(synth.obj,HI,method = 'logit',maxorder=1)

##Output synthetic data
syn1<- as.data.frame(synth.obj$syn)
View(syn1)
str(syn1)

write.csv(syn1, file = "HI_syn1.csv", row.names=FALSE)


# Checking distribution of Age based on Vehicle_Age categories
hist(HI$Age, xlab = 'Age', main = 'Distribution of Age', col = 'lightblue')

barchart(x= HI$Vehicle_Age, xlab = 'Vehicle Age', main = 'Distribution of Vehicle Age', col = 'lightblue')

# change typr of variable to categorical
HI$Vehicle_Age = as.factor(HI$Vehicle_Age)
levels(HI$Vehicle_Age)


## statistical summary of Age based on Vehicle_age categories
HI %>% 
  group_by(Vehicle_Age) %>% 
  summarise(across(Age, mean,na.rm=TRUE)) %>%
  summaries(count())

HI %>% 
  group_by(Vehicle_Age) %>% 
  summaries()

library(MASS)
head(birthwt)
# Use vehicle_age as the faceting variable
ggplot(HI, aes(x = Age)) +
  geom_histogram(fill = "Blue", colour = "Blue") +
  facet_grid(Vehicle_Age ~ . )

# plot for synthesized data
ggplot(syn1, aes(x = Age)) +
  geom_histogram(fill = "Blue", colour = "Blue") +
  facet_grid(Vehicle_Age ~ .)


##for normality comparison between synthetic data and original data
##QQ-plot for the original Age and the synthetic Age
qqnorm(HI$Age, pch = 1, frame = FALSE, main = "QQ-plot of original Age", col = "blue")
qqline(HI$Age, col = "red", lwd = 2)
qqnorm(syn1$Age, pch = 1, frame = FALSE, main = "QQ-plot of synthetic Age", col = "blue")
qqline(syn1$Age, col = "red", lwd = 2)


levels(year_1$Vehicle_Age)
# "< 1 Year"  "> 2 Years" "1-2 Year" 
##QQ-plot for the original Age and the synthetic Age < 1 year
year_1 = HI %>% filter(Vehicle_Age =="< 1 Year") 
year_1syn = syn1 %>% filter(Vehicle_Age =="< 1 Year")
str(year_1syn)
qqnorm(year_1$Age, pch = 1, frame = FALSE, main = "QQ-plot of original < 1 year", col = "blue")
qqline(year_1$Age, col = "red", lwd = 2)
qqnorm(year_1syn$Age, pch = 1, frame = FALSE, main = "QQ-plot of synthetic < 1 year", col = "blue")
qqline(year_1syn$Age, col = "red", lwd = 2)

##QQ-plot for the original Age and the synthetic Age < 2 year
year_2 = HI %>% filter(Vehicle_Age =="1-2 Year") 
year_2syn = syn1 %>% filter(Vehicle_Age =="1-2 Year")
str(year_2syn)
qqnorm(year_2$Age, pch = 1, frame = FALSE, main = "QQ-plot of original 1-2 Year", col = "blue")
qqline(year_2$Age, col = "red", lwd = 2)
qqnorm(year_2syn$Age, pch = 1, frame = FALSE, main = "QQ-plot of synthetic 1-2 Year", col = "blue")
qqline(year_2syn$Age, col = "red", lwd = 2)

##QQ-plot for the original Age and the synthetic Age > 2 year
year_3 = HI %>% filter(Vehicle_Age =="> 2 Years") 
year_3syn = syn1 %>% filter(Vehicle_Age =="> 2 Years")
str(year_3syn)
qqnorm(year_3$Age, pch = 1, frame = FALSE, main = "QQ-plot of original > 2 Years", col = "blue")
qqline(year_3$Age, col = "red", lwd = 2)
qqnorm(year_3syn$Age, pch = 1, frame = FALSE, main = "QQ-plot of synthetic > 2 Years", col = "blue")
qqline(year_3syn$Age, col = "red", lwd = 2)

############## KS test ##############
#ks test for Age
ks.test(HI$Age, syn1$Age)
plot.ecdf(HI$Age, verticals=TRUE, do.points=FALSE, col="red",
          xlab="Age", main=" Variable Age for original and syn data")
lines(ecdf(syn1$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for < 1 year
ks.test(year_1$Age,year_1syn$Age)
plot.ecdf(year_1$Age, verticals=TRUE, do.points=FALSE, col="red", 
          xlab="Age", main=" Variable Age, < 1 Year for original and syn data")
lines(ecdf(year_1syn$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for 1-2 years
ks.test(year_2$Age,year_2syn$Age)
plot.ecdf(year_2$Age, verticals=TRUE, do.points=FALSE, col="red", 
          xlab="Age", main=" Variable Age, 1-2 Years for original and syn data")
lines(ecdf(year_2syn$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for >2 years
ks.test(year_3$Age,year_3syn$Age)
plot.ecdf(year_3$Age, verticals=TRUE, do.points=FALSE, col="red", 
          xlab="Age", main=" Variable Age, > 2 Years for original and syn data")
lines(ecdf(year_3syn$Age), verticals=TRUE, do.points=FALSE, col="blue")

####### multinomial Logistic regression for Vehicle_age and age
library(nnet)
library(VGAM)

fit1<- vglm(Vehicle_Age~ Age+Vintage+factor(Gender),  family=multinomial, data=HI)
summary(fit1)

#checking the assumptions
par(mfrow=c(2,2))
plot(fit1, main= "Residual plot for original model")
par(mfrow=c(1,1))

# fit the model for synthetic data
fit1_syn<- vglm(Vehicle_Age~ Age+Vintage+factor(Gender),  family=multinomial, data=syn1)
summary(fit1_syn)
anova(fit1_syn)
AIC(fit1_syn)

#checking the assumptions
par(mfrow=c(2,2))
plot(fit1_syn, main= "Residual plot for synthesized model")
par(mfrow=c(1,1))

########## Part C ##############
library(readr)
library(readxl)
library(eeptools)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(ggpubr)
library(tidyverse) 
library(outliers)
library(psych)
library(MASS)
library(psych)


data <- read_csv("Synthesized_Data.csv")
View(data)
str(data)
dim(data)
unique(data$Sport)
unique(data$Test.Type)
unique(data$Team.ID)
unique(data$Examiner.ID)
unique(data$Examiner.Role)
#Convert Injury date to numeric
data$Injury.Date<-strptime(paste0(as.character(data$Injury.Date)," 00:00"), format = "%d/%m/%Y %H:%M")
str(data$Injury.Date)
data$Injury.Date<-as.numeric(data$Injury.Date)

data$Age <- as.Date(data$Birthdate, format = "%Y-%m-%d")
data = filter(data, Age >= '1920-01-01' & Age <= '2021-01-01')
data$Age <- floor(age_calc(data$Age, units = "years"))
unique(sort(data$Age))
#Sorting and dropping value out of range
data = filter(data, Age >= 11 & Age <= 60)
str(data)


df_new <- data[,-c(6,12)]
head(df_new)
rules.list <- list(
  Injury.Date = "Test.Type ==  'Baseline'",
  Concussion.Diagnosed.Decision = "Test.Type ==  'Baseline' || Test.Type ==  'RTP' || Test.Type ==  'Athlete Self-Test'",
  Removed.from.Play = "Test.Type ==  'Baseline'")

rules.value.list <- list(
  Injury.Date = NA,
  Concussion.Diagnosed.Decision = NA,
  Removed.from.Play = NA)

myseed=1020
##Applying synthpop
library(synthpop)
synth.obj <- syn(df_new, method = "cart",rules = rules.list, rvalues = rules.value.list, seed = myseed, maxfaclevels = 430)

df_syn <- data.frame(synth.obj$syn)
View(df_syn)
head(df_syn)
dim(df_syn)
unique(df_syn$Sport)
df_syn <- df_syn[!is.na(df_syn$Sport),]
unique(df_new$Sport)
df_new <- df_new[!is.na(df_new$Sport),]

##histogram of Age Original and Age Synthesized
hist(df_new$Age,main = "Age Original",col = 'blue')
hist(df_syn$Age,main = "Age Synthesized",col = 'blue')

##QQ plot of Age Original and Age Synthesized
plot(as.factor(df_new$Age),main = "Age Original",col = 'blue')
plot(as.factor(df_syn$Age),main = "Age Synthesized",col = 'blue')

##mean,median,std,min,max
describe(df_new$Age)
describe(df_syn$Age)

##frequency table for Sports
table(df_new$Sport)
table(df_syn$Sport)

##bar chart of the Sports Original and the Sports Synthesized
plot(as.factor(df_new$Sport),col="blue", main = "Sports Original")
plot(as.factor(df_syn$Sport),col="blue", main = "Sports Synthesized")

###Comparison between the original data and the synthetic data
####sport 1####
par(mfrow = c(1,2))
sport1.df_new <- df_new%>% filter(df_new$Sport == 1)

plot(as.factor(sport1.df_new$Age),col="blue", main = "The Original Sport1")
hist(sport1.df_new$Age,main = "Sport1 The Original Data",col = 'blue')
dim(sport1.df_new)
describe(sport1.df_new$Age)
sport1.df_syn <- df_syn%>% filter(df_syn$Sport == 1)

plot(as.factor(sport1.df_syn$Age),col="blue", main = "The Synthetic Sport1")
hist(sport1.df_syn$Age,main = "Sport1 The Synthesized Data",col = 'blue')
dim(sport1.df_syn)
describe(sport1.df_syn$Age)

####sport 10####
sport10.df_new <- df_new%>% filter(df_new$Sport == 10)

plot(as.factor(sport10.df_new$Age),col="blue", main = "The Original Sport10")
hist(sport10.df_new$Age,main = "Sport10 Distribution of The Original Data",col = 'blue')
dim(sport10.df_new)
describe(sport10.df_new)

sport10.df_syn <- df_syn%>% filter(df_syn$Sport == 10)

plot(as.factor(sport10.df_syn$Age),col="blue", main = "The Synthetic Sport10")
hist(sport10.df_syn$Age,main = "Sport10 Distribution of The Synthesized Data",col = 'blue')
dim(sport10.df_syn)
describe(sport10.df_syn)

##young people
Age11.df_new <- df_new%>% filter(df_new$Age == 11)
table(Age11.df_new$Sport)
Age11.df_syn <- df_syn%>% filter(df_syn$Age == 11)
table(Age11.df_syn$Sport)

##old people
Age54.df_new <- df_new%>% filter(df_new$Age == 54)
table(Age54.df_new$Sport)
Age54.df_syn <- df_syn%>% filter(df_syn$Age == 54)
table(Age54.df_syn$Sport)

##The categories with very small observations
####sport 13####
sport13.df_new <- df_new%>% filter(df_new$Sport == 13)
plot(as.factor(sport13.df_new$Age),col="blue", main = "The Original Sport13")
hist(sport13.df_new$Age,main = "Sport13 Distribution of The Original Data",col = 'blue')
dim(sport13.df_new)
describe(sport13.df_new$Age)

sport13.df_syn <- df_syn%>% filter(df_syn$Sport == 13)
plot(as.factor(sport13.df_syn$Age),col="blue", main = "The Synthetic Sport13")
hist(sport13.df_syn$Age,main = "Sport13 Distribution of The Synthesized Data",col = 'blue')
dim(sport13.df_syn)
describe(sport13.df_syn$Age)

####sport 14####
sport14.df_new <- df_new%>% filter(df_new$Sport == 14)
plot(as.factor(sport14.df_new$Age),col="blue", main = "The Original Sport14")
hist(sport14.df_new$Age,main = "Sport14 Distribution of The Original Data",col = 'blue')
dim(sport14.df_new)
describe(sport14.df_new$Age)

sport14.df_syn <- df_syn%>% filter(df_syn$Sport == 14)
plot(as.factor(sport14.df_syn$Age),col="blue", main = "The Synthetic Sport14")
hist(sport14.df_syn$Age,main = "Sport14 Distribution of The Synthesized Data",col = 'blue')
dim(sport14.df_syn)
describe(sport14.df_syn$Age)

####sport 17####
sport17.df_new <- df_new%>% filter(df_new$Sport == 17)
plot(as.factor(sport17.df_new$Age),col="blue", main = "The Original Sport17")
hist(sport17.df_new$Age,main = "Sport17 Distribution of The Original Data",col = 'blue')
dim(sport17.df_new)
describe(sport17.df_new$Age)

sport17.df_syn <- df_syn%>% filter(df_syn$Sport == 17)
plot(as.factor(sport17.df_syn$Age),col="blue", main = "The Synthetic Sport17")
hist(sport17.df_syn$Age,main = "Sport17 Distribution of The Synthesized Data",col = 'blue')
dim(sport17.df_syn)
describe(sport17.df_syn$Age)


#######ks test######
par(mfrow = c(2,2))
#ks test for sport1
ks.test(sport1.df_syn$Age,sport1.df_new$Age)
plot.ecdf(sport1.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport1")
lines(ecdf(sport1.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport2
ks.test(sport2.df_syn$Age,sport2.df_new$Age)
plot.ecdf(sport2.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport2")
lines(ecdf(sport2.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport3
ks.test(sport3.df_syn$Age,sport3.df_new$Age)
plot.ecdf(sport3.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport3")
lines(ecdf(sport3.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport4
ks.test(sport4.df_syn$Age,sport4.df_new$Age)
plot.ecdf(sport4.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport4")
lines(ecdf(sport4.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport5
ks.test(sport5.df_syn$Age,sport5.df_new$Age)
plot.ecdf(sport5.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport5")
lines(ecdf(sport5.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport6
ks.test(sport6.df_syn$Age,sport6.df_new$Age)
plot.ecdf(sport6.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport6")
lines(ecdf(sport6.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport7
ks.test(sport7.df_syn$Age,sport7.df_new$Age)
plot.ecdf(sport7.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport7")
lines(ecdf(sport7.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport8
ks.test(sport8.df_syn$Age,sport8.df_new$Age)
plot.ecdf(sport8.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport8")
lines(ecdf(sport8.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport9
ks.test(sport9.df_syn$Age,sport9.df_new$Age)
plot.ecdf(sport9.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport9")
lines(ecdf(sport9.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport10
ks.test(sport10.df_syn$Age,sport10.df_new$Age)
plot.ecdf(sport10.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport10")
lines(ecdf(sport10.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport11
ks.test(sport11.df_syn$Age,sport11.df_new$Age)
plot.ecdf(sport11.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport11")
lines(ecdf(sport11.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport12
ks.test(sport12.df_syn$Age,sport12.df_new$Age)
plot.ecdf(sport12.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport12")
lines(ecdf(sport12.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport13
ks.test(sport13.df_syn$Age,sport13.df_new$Age)
plot.ecdf(sport13.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport13")
lines(ecdf(sport13.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport14
ks.test(sport14.df_syn$Age,sport14.df_new$Age)
plot.ecdf(sport14.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport14")
lines(ecdf(sport14.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport15
ks.test(sport15.df_syn$Age,sport15.df_new$Age)
plot.ecdf(sport15.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport15")
lines(ecdf(sport15.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport16
ks.test(sport16.df_syn$Age,sport16.df_new$Age)
plot.ecdf(sport16.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport16")
lines(ecdf(sport16.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport17
ks.test(sport17.df_syn$Age,sport17.df_new$Age)
plot.ecdf(sport17.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport17")
lines(ecdf(sport17.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport19
ks.test(sport19.df_syn$Age,sport19.df_new$Age)
plot.ecdf(sport19.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport19")
lines(ecdf(sport19.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport20
ks.test(sport20.df_syn$Age,sport20.df_new$Age)
plot.ecdf(sport20.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport20")
lines(ecdf(sport20.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport21
ks.test(sport21.df_syn$Age,sport21.df_new$Age)
plot.ecdf(sport21.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport21")
lines(ecdf(sport21.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport22
ks.test(sport22.df_syn$Age,sport22.df_new$Age)
plot.ecdf(sport22.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport22")
lines(ecdf(sport22.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")

#ks test for sport23
ks.test(sport23.df_syn$Age,sport23.df_new$Age)
plot.ecdf(sport23.df_syn$Age, verticals=TRUE, do.points=FALSE, col="red",main = "KS plot of Sport23")
lines(ecdf(sport23.df_new$Age), verticals=TRUE, do.points=FALSE, col="blue")
