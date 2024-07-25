library(dplyr)
library(ggplot2)

#As requested, here is the data set up to data1, data2, and data3
data1 <- read.csv(file="january2018.csv")
data2 <- read.csv(file="august2018.csv")
data3 <- read.csv(file="carriers.csv")

january2018 = data1
august2018 = data2
carriers = data3

#-----------------------------------------------------------------------------------------------
#---------------------- START OF CODE ----------------------------------------------------------
#-----------------------------------------------------------------------------------------------

#First: Creating the Histogram
#Combine the two months of data
AllDelayTimes <- c(january2018$ARR_DELAY, august2018$ARR_DELAY)

#Some quick analysis before we begin the graph
Airportavg <- mean(AllDelayTimes)
Airportsd <- sd(AllDelayTimes)
BestDelay = min(AllDelayTimes)
WorstDelay = max(AllDelayTimes)
Airportmedi = median(AllDelayTimes)
RoundedAvg = round(Airportavg, digits = 3)
mean(january2018$ARR_DELAY)
mean(august2018$ARR_DELAY)

#Used code below to get rid of the extremes (over 3 SDs on either side)
EditedJan = filter(january2018, ARR_DELAY <= Airportsd*2 & ARR_DELAY >= -Airportsd*2)
EditedAug = filter(august2018, ARR_DELAY >= -Airportsd*2 & ARR_DELAY <= Airportsd*2)

#Make a dataframe of the combined delay times from Aug and Jan
DelayTimes <- c(EditedAug$ARR_DELAY, EditedJan$ARR_DELAY)
PerfectedDelayTimes <- data_frame(DelayTimes, rep(length(DelayTimes),1:1))

#NOW MAKE A DETAILED GRAPH
ggplot(PerfectedDelayTimes, aes(x=DelayTimes, y=..density..))+
  geom_histogram(binwidth = 4)+
  ggtitle("Histogram of Flight Delays (Jan & Aug 2018)")+
  xlab("Arrival Delay")+
  ylab("Density")+
  annotate("text", x = 75, y = .02, label = "Average =")+
  annotate("text", x = 95, y = .02, label = print(RoundedAvg), color="red")+
  annotate("text", x = 75, y = .018, label = "Median =")+
  annotate("text", x = 95, y = .018, label = print(Airportmedi), color="blue")+
  annotate("text", x = 75, y = .016, label = "Average Aug =")+
  annotate("text", x = 100, y = .016, label = print(round(mean(august2018$ARR_DELAY), digits = 3)), color="orange")+
  annotate("text", x = 75, y = .014, label = "Average Jan =")+
  annotate("text", x = 100, y = .014, label = print(round(mean(january2018$ARR_DELAY), digits = 3)), color="green")+
  geom_vline(xintercept = Airportavg, color = "red")+
  geom_vline(xintercept = Airportmedi, color = "blue")+
  geom_vline(xintercept = mean(january2018$ARR_DELAY), color = "green")+
  geom_vline(xintercept = mean(august2018$ARR_DELAY), color = "orange")


#-----------------------------------------------------------------------------------------------

ggplot(august2018,aes(x=DAY_OF_WEEK,y=ARR_DELAY))+geom_point()+ ggtitle("Arrival Delays vs Day of Week in August") + xlab ("Day of Week") + ylab ("Delays") + geom_smooth(method=lm,color="green",se=TRUE)+
  scale_x_discrete(name="Day of Week", limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

A = filter(august2018,DAY_OF_WEEK=="1")
B = filter(august2018,DAY_OF_WEEK=="2")
C = filter(august2018,DAY_OF_WEEK=="3")
D = filter(august2018,DAY_OF_WEEK=="4")
E = filter(august2018,DAY_OF_WEEK=="5")
F = filter(august2018,DAY_OF_WEEK=="6")
G = filter(august2018,DAY_OF_WEEK=="7")
#Creating subsets for the arrival times delays on each day of the week 
mean(A$ARR_DELAY)
mean(B$ARR_DELAY)
mean(C$ARR_DELAY)
mean(D$ARR_DELAY)
mean(E$ARR_DELAY)
mean(F$ARR_DELAY)
mean(G$ARR_DELAY)
#Calculating the mean arrival delays per day

#Creating a vector for the arrival delays (by Nik Osorio)
week = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
augweekdaymeans = c(mean(A$ARR_DELAY), mean(B$ARR_DELAY), mean(C$ARR_DELAY), mean(D$ARR_DELAY), mean(E$ARR_DELAY),mean(F$ARR_DELAY),mean(G$ARR_DELAY))
AugustWeekMeans = data_frame(week, augweekdaymeans)
AugustWeekMeans

#-----------------------------------------------------------------------------------------------

ggplot(january2018,aes(x=DEP_DELAY,y=ARR_DELAY))+
  geom_point()+
  geom_smooth(method=lm,color="green",se=TRUE)+
  ggtitle("Arrival/Departure Delay Correlation")+
  xlab("Departure Delay")+
  ylab("Arrival Delay")


#CORRELATING THE TWO DELAYS IN AUG AND JAN
jancor=cor(january2018$DEP_DELAY,january2018$ARR_DELAY)
augcor=cor(august2018$DEP_DELAY,august2018$ARR_DELAY)
jancor
augcor
mean(c(augcor,jancor))
#this is to compare the correlations between arrival and departure delays

#IDENTIFY THE DIFFERENT CARRIERS
united=filter(january2018, OP_UNIQUE_CARRIER=="UA")
american= filter(january2018,OP_UNIQUE_CARRIER=="AA")
frontier=filter(january2018,OP_UNIQUE_CARRIER=="F9")
delta=filter(january2018, OP_UNIQUE_CARRIER=="DL")
republic=filter(january2018,OP_UNIQUE_CARRIER=="YX")
endeavor=filter(january2018,OP_UNIQUE_CARRIER=="9E")
alaska=filter(january2018,OP_UNIQUE_CARRIER=="AS")
jetblue=filter(january2018,OP_UNIQUE_CARRIER=="B6")
expressjet=filter(january2018,OP_UNIQUE_CARRIER=="EV")
allegiant=filter(january2018,OP_UNIQUE_CARRIER=="G4")
hawaiian=filter(january2018,OP_UNIQUE_CARRIER=="HA")
envoy=filter(january2018,OP_UNIQUE_CARRIER=="MQ")
spirit=filter(january2018,OP_UNIQUE_CARRIER=="NK")
psa=filter(january2018,OP_UNIQUE_CARRIER=="OH")
skywest=filter(january2018,OP_UNIQUE_CARRIER=="OO")
mesa=filter(january2018,OP_UNIQUE_CARRIER=="YV")
virgin=filter(january2018,OP_UNIQUE_CARRIER=="VX")
southwestern=filter(january2018,OP_UNIQUE_CARRIER=="WN")

#PREPARE A DATAFRAME FOR AIRLINES, AIRLINE CODE, AND THEIR AVERAGE
carr_list = c("alaska", "allegiant", "american", "delta", "endeavor", "envoy", "expressjet", "frontier", "hawaiian", "jetblue", "mesa", "psa", "republic", "skywest", "southwestern", "spirit", "united", "virgin")
carr_code = c("AS", "G4", "AA", "DL", "9E", "MQ", "EV", "F9", "HA", "B6", "YV", "OH", "YX", "OO", "WN", "NK", "UA", "VX")
carr_avg = c(mean(alaska$ARR_DELAY), mean(allegiant$ARR_DELAY), mean(american$ARR_DELAY), mean(delta$ARR_DELAY), mean(endeavor$ARR_DELAY), mean(envoy$ARR_DELAY), mean(expressjet$ARR_DELAY), mean(frontier$ARR_DELAY), mean(hawaiian$ARR_DELAY), mean(jetblue$ARR_DELAY), mean(mesa$ARR_DELAY), mean(psa$ARR_DELAY), mean(republic$ARR_DELAY), mean(skywest$ARR_DELAY), mean(southwestern$ARR_DELAY), mean(spirit$ARR_DELAY), mean(united$ARR_DELAY), mean(virgin$ARR_DELAY))
carr_avg
carr_list
carr_code
#CREATE THE DATAFRAME
carrier_avg <- data_frame(carr_list, carr_code, carr_avg)
carrier_avg

#CREATING A BOX PLOT FOR THE JANUARY WAIT TIMES
ggplot(data=january2018, aes(x=OP_UNIQUE_CARRIER,y=ARR_DELAY))+
  geom_boxplot(outlier.shape=NA,outlier.size=0, notch=FALSE)+
  ylim(-65, 80)+
  ggtitle("Flight Delay Patterns by Carrier")+
  xlab("Carrier Code")+
  ylab("Delays (minutes)")

# ------------------------------ Hypothesis Test -----------------------------------
# The following calculates the z-score of a two sample z-test
HypoMean <- mean(august2018$ARR_DELAY)-mean(january2018$ARR_DELAY)
SEjames <- sqrt((sd(january2018$ARR_DELAY)/sqrt(nrow(january2018)))^2+(sd(august2018$ARR_DELAY)/sqrt(nrow(august2018)))^2)
#while the line above looks intimidating, almost all the code is for the calculation of the SE
Zscore = 8.466212/0.1579458
pval = 1-pnorm(Zscore)
HypoMean
SEjames
Zscore
pval
#P-Value is not zero exactly, but very small to the point R recognises it as zero
#Because p<1 we can reject null hypothesis


#-----------------------------------------------------------------------------------------------

#takes a long time to load due to heavy data
ggplot(january2018,aes(x=DAY_OF_WEEK,y=ARR_DELAY))+
  geom_point()+
  geom_smooth(method=lm,color="red",se=TRUE)+
  ggtitle("Arrival Delays vs Day of Week in January") +
  xlab("Day of Week")+
  ylab("Delay")+
  scale_x_discrete(name="Day of Week", limits=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

A= filter(january2018,DAY_OF_WEEK=="1")
B= filter(january2018,DAY_OF_WEEK=="2")
C= filter(january2018,DAY_OF_WEEK=="3")
D= filter(january2018,DAY_OF_WEEK=="4")
E= filter(january2018,DAY_OF_WEEK=="5")
F= filter(january2018,DAY_OF_WEEK=="6")
G= filter(january2018,DAY_OF_WEEK=="7")
mean(A$ARR_DELAY)
mean(B$ARR_DELAY)
mean(C$ARR_DELAY)
mean(D$ARR_DELAY)
mean(E$ARR_DELAY)
mean(F$ARR_DELAY)
mean(G$ARR_DELAY)

#Creating a dataframe for the january arrival delays by day of week (by Nik Osorio)
week = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
janweekdaymeans = c(mean(A$ARR_DELAY), mean(B$ARR_DELAY), mean(C$ARR_DELAY), mean(D$ARR_DELAY), mean(E$ARR_DELAY),mean(F$ARR_DELAY),mean(G$ARR_DELAY))
JanuaryWeekMeans = data_frame(week, janweekdaymeans)
JanuaryWeekMeans

#Finding the average of both Jan and Aug for Day of Week
sizeaug <- length(august2018$ARR_DELAY)
sizejan <- length(january2018$ARR_DELAY)
totalweekdaymeans <- (janweekdaymeans*sizejan+augweekdaymeans*sizeaug)/(sizeaug+sizejan)
totalweekdaymeans

#Creating a dataframe for average delay by week for both Aug and Jan (by Nik Osorio)
DelayWeek = data_frame(week, janweekdaymeans, augweekdaymeans, totalweekdaymeans)
DelayWeek

#-----------------------------------------------------------------------------------------------

#FINDING THE AVERAGE WAIT TIME AUGUST AIRLINES
#IDENTIFY THE DIFFERENT CARRIERS, THIS TIME FOR AUGUST
united=filter(august2018, OP_UNIQUE_CARRIER=="UA")
american= filter(august2018,OP_UNIQUE_CARRIER=="AA")
frontier=filter(august2018,OP_UNIQUE_CARRIER=="F9")
delta=filter(august2018, OP_UNIQUE_CARRIER=="DL")
republic=filter(august2018,OP_UNIQUE_CARRIER=="YX")
endeavor=filter(august2018,OP_UNIQUE_CARRIER=="9E")
alaska=filter(august2018,OP_UNIQUE_CARRIER=="AS")
jetblue=filter(august2018,OP_UNIQUE_CARRIER=="B6")
expressjet=filter(august2018,OP_UNIQUE_CARRIER=="EV")
allegiant=filter(august2018,OP_UNIQUE_CARRIER=="G4")
hawaiian=filter(august2018,OP_UNIQUE_CARRIER=="HA")
envoy=filter(august2018,OP_UNIQUE_CARRIER=="MQ")
spirit=filter(august2018,OP_UNIQUE_CARRIER=="NK")
psa=filter(august2018,OP_UNIQUE_CARRIER=="OH")
skywest=filter(august2018,OP_UNIQUE_CARRIER=="OO")
mesa=filter(august2018,OP_UNIQUE_CARRIER=="YV")
virgin=filter(august2018,OP_UNIQUE_CARRIER=="VX")
southwestern=filter(august2018,OP_UNIQUE_CARRIER=="WN")

#PREPARE A DATAFRAME FOR AIRLINES, AIRLINE CODE, AND THEIR AVERAGE
carr_list = c("alaska", "allegiant", "american", "delta", "endeavor", "envoy", "expressjet", "frontier", "hawaiian", "jetblue", "mesa", "psa", "republic", "skywest", "southwestern", "spirit", "united", "virgin")
carr_code = c("AS", "G4", "AA", "DL", "9E", "MQ", "EV", "F9", "HA", "B6", "YV", "OH", "YX", "OO", "WN", "NK", "UA", "VX")
carr_avg = c(mean(alaska$ARR_DELAY), mean(allegiant$ARR_DELAY), mean(american$ARR_DELAY), mean(delta$ARR_DELAY), mean(endeavor$ARR_DELAY), mean(envoy$ARR_DELAY), mean(expressjet$ARR_DELAY), mean(frontier$ARR_DELAY), mean(hawaiian$ARR_DELAY), mean(jetblue$ARR_DELAY), mean(mesa$ARR_DELAY), mean(psa$ARR_DELAY), mean(republic$ARR_DELAY), mean(skywest$ARR_DELAY), mean(southwestern$ARR_DELAY), mean(spirit$ARR_DELAY), mean(united$ARR_DELAY), mean(virgin$ARR_DELAY))
carr_avg
carr_list
carr_code
#CREATE THE DATAFRAME
carrier_avg <- data_frame(carr_list, carr_code, carr_avg)
carrier_avg

#SCATTER PLOTS DO NOT SHOW DENSITY! We must use a box method.
#This Creates a Box Graph
ggplot(data=august2018, aes(x=OP_UNIQUE_CARRIER,y=ARR_DELAY))+
  geom_boxplot(outlier.shape=NA,outlier.size=0)+
  ylim(-55, 75)+
  ggtitle("Flight Delay Patterns by Carrier (Aug. 2018)")+
  xlab("Carrier Code")+
  ylab("Delays (minutes)")

#----------------------------------------------------------------------------------------------------------
#End of the script, end of project


ggplot()+
  annotate("text", x = 0, y = 0, label = "Thank You For a Great Semester, Professor Ibser!")
