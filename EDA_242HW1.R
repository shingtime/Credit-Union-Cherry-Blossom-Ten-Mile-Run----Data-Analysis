############## This part is for exploratory data analysis ###############
######### Question: Explore age distribution: histogram,median,mean,etc. ############
library(survival)
library(doBy)
mean_age_sex = summaryBy(ag ~ sex, data = data, FUN = list(mean,median),na.rm=TRUE)
mean_age_sex
## boxplot to show the result
## By sex
ggplot(data[-c(which(is.na(data$ag)))],aes(x=sex,y=ag,fill=sex))+geom_boxplot()+stat_summary(fun.y=mean, geom="point", shape=5, size=4)+ylab("Age")+xlab("Gender")+ggtitle("Age distribution by Gender")
### By year
ggplot(data,aes(x=year,y=ag,fill=year))+geom_boxplot()+stat_summary(fun.y=mean, geom="point", shape=5, size=4)+facet_grid(.~year,scales="free", space="free")+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ylab("age")+ggtitle("Age distribution by Year")
## age ranges are quite similiar in different males and females.
## average age by sex and year
mean_year = summaryBy(ag ~ year+sex, data = data, FUN = list(mean),na.rm=TRUE)
mean_year
mean_year[which(mean_year$sex=="female"),"ag.mean"]
mean_year[which(mean_year$sex=="male"),"ag.mean"]
## boxplot to visuliaze the result
ggplot(data,aes(x=sex,y=ag,fill=sex))+geom_boxplot()+stat_summary(fun.y=mean, geom="point", shape=5, size=4)+facet_grid(.~year,scales="free", space="free")
## ANSWER: The average ages of male runners are larger than female runners' in all years.
####### Plot the distributions of age  ###############
library(ggplot2)
## overall distribution
ggplot(data= data,aes(x=ag))+geom_histogram(aes(y=..density..),binwidth=0.5,color="black",fill="white")+geom_vline(aes(xintercept=mean(ag, na.rm=T)),color="red", linetype="dashed", size=1)+geom_vline(aes(xintercept=median(ag, na.rm=T)),color="blue", linetype="dashed", size=1) +xlab("age")+ggtitle("Overall Age Distribution ")
## overall distribution: right skewed.
## distribution of age by sex (histrogram)
ggplot(data=data,aes(x=ag,fill=sex))+geom_histogram(binwidth=0.7,alpha=0.5,position="identity")+ggtitle("Age distribution by sex")
#### male runners age distributions are right skewed.
## distribution  of age by year (histogram)
ggplot(data, aes(x=ag)) + geom_histogram(binwidth=.6, colour="blue", fill="white") + facet_grid(.~ year,scales="free", space="free")
#### each year's distributions are quite similiar. But there are more runners in the recent years.
## distribution in each year for male and female
ggplot(data, aes(x=ag)) + geom_histogram(binwidth=.6, colour="black", fill="white") + facet_grid(sex~year,scales="free", space="free")
## It seems that runners are younger in each year for female. Because ##the peak is left skewed. The distributions are all slightly right-skewed.
library(plyr)
######### Question:   Explore race time  difference #######
library(reshape2)
#### Any difference between male and female gun_time(net_time)
summaryBy(race_time~ sex, data = data, FUN = list(mean,median),na.rm=TRUE)
ggplot(data,aes(x=sex,y=race_time,fill=sex))+geom_boxplot()+stat_summary(fun.y=mean, geom="point", shape=7, size=4)+ylab("Race Time")+
  ggtitle("Box plot of Race Time by Sex")
## boxplot of race time
ggplot(data,aes(x=year,y=race_time,fill=year))+geom_boxplot()+stat_summary(fun.y=mean, geom="point", shape=5, size=4)+facet_grid(.~year,scales="free", space="free")+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ylab("Race Time")+ggtitle("Race time distribution by Year")
## histrogram of race time 
ggplot(data=data,aes(x=race_time,fill=sex))+geom_histogram(binwidth=0.7,alpha=0.5,position="identity")+ggtitle("Race Time distribution by sex")
ggplot(data=data,aes(x=race_time))+geom_histogram(binwidth=0.7,alpha=0.5,position="identity")+ggtitle("Overall Race Time Distribution")+geom_vline(aes(xintercept=mean(race_time, na.rm=T)),color="red", linetype="dashed", size=1)+geom_vline(aes(xintercept=median(race_time, na.rm=T)),color="blue", linetype="dashed", size=1) +xlab("Race Time")+ggtitle("Overall Race Time Distribution ") 
#### Mean and median of race time by year
time_year = summaryBy(race_time~ year, data = data, FUN = list(mean,median),na.rm=TRUE)
time_year
## time trend of mean of race time in each year
ggplot(time_year,aes(x=year,y=race_time.mean))+geom_line(linetype="dashed",color="blue",size=1)+ggtitle("Mean of race time in each year")+ylab("Race Time")+geom_smooth()
#### any difference in race_time for different sex
time_sex = summaryBy(race_time~ sex, data = data, FUN = list(mean,median),na.rm=TRUE)
## female runners' time is longer than male runners.
#### any difference in gun_time for different sex and year
mean_time_sex = summaryBy(race_time~ sex+year, data = data, FUN = list(mean),na.rm=TRUE)
mean_time_women = mean_time_sex[1:12,]
mean_time_men = mean_time_sex[13:24,]
### time trend of mean race time in each year
ggplot()+geom_line(data=mean_time_men,aes(x=year,y=race_time.mean,color="male"))+geom_line(data =mean_time_women,aes(x=year,y=race_time.mean,color="female"))+ggtitle("Average race time in each year")+ylab("average race time")+scale_color_discrete(name="Sex")
## time trend of mean race time by sex in each year
ggplot(data,aes(x=year,y=race_time,fill=year))+geom_boxplot()+stat_summary(fun.y=mean, geom="point", shape=5, size=4)+facet_grid(.~year,scales="free", space="free")+theme(axis.ticks = element_blank(), axis.text.x = element_blank())+ggtitle("boxplot of race_time")
## median gun_times along with year don't change a lot for both male and female runners.
### Male runners spend less time to finish the game. The average gun_time increases in each year.
########### Question: Is age and race_time positively related?
ggplot(data,aes(x=ag,y=race_time,fill=sex))+geom_point()+facet_grid(.~sex,scales="free", space="free")+geom_smooth(method="lm", fill=NA)+xlab("age")+ylab("race time")+ggtitle("Relation between race time and age")
## For both male and female runners, age has positive relation with gun_time, which means age has negative relation with speed.runners slow as they age
race_age = lm(race_time~ag,data=data)
a = summary(race_age)
## cut age into several group and explore the relation between age and race time in each group
age_group = cut(data$ag, breaks = c(seq(10, 90, 10)))
table(age_group)
## combine with original data
data = cbind(data,age_group)
DATA = data[which(!is.na(data$age_group)),]
## boxplot of race time with each year group
ggplot(DATA,aes(x=age_group,y=race_time))+geom_boxplot()+stat_summary(fun.y=mean, geom="point", shape=5, size=4,na.rm=TRUE)+
  ylab("Race Time")+ggtitle("Race time distribution by age group")+xlab("Age Group")+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+theme(axis.ticks = element_blank(), axis.text.x = element_blank())
######### Question : Who are the top local finishers in first two years and last two years?
index_local = grep(". [A-Z][A-Z]",data$hometown)
data_sub = data[index_local,]
data_sub = data_sub[order(data_sub$year,data_sub$race_time),]
Year = paste(1999:2010)
names = c()
for (i in 1:12){
  sub = subset(data_sub,data_sub$year==Year[i])
  names = append(names,paste(sub$year[1],sub$name[1],sub$hometown[1],sub$race_time[1],sub$sex[1]))
}
## save each year's domestic chamption in a data frame
names 
# Question: Who are the top international player in different years?

data_int = data[-index_local,]
name_int = c()
for (i in 1:12){
  sub = subset(data_int,data_int$year==Year[i])
  name_int = append(name_int,paste(sub$year[1],sub$name[1],sub$hometown[1],sub$race_time[1]))
}
name_int
######### Question:Who are the winners of women group in each year?
data_int_women = subset(data_int,data_int$sex=="female")
name_women_int = c()
for (i in 1:12){
  sub = subset(data_int_women,data_int_women$year==Year[i])
  name_women_int = append(name_women_int,paste(sub$year[1],sub$name[1],sub$hometown[1],sub$race_time[1]))
}
name_women_int
############# Question: scatter plot of race_time vs. age
smoothScatter(y = data$race_time, x = data$ag,
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Race Time (minutes)",bandwidth=c(2,1)/3,main="Scatter plot: Age vs. Race Time")
######## QUESTION 11: fitting models for average performance
race_age = lm(race_time~ag,data=data)
summary(race_age)
##### QUESTION How many entrants are there over the 14 years?
## write a function to clean the name and hometown variable
eli_blanks = function(vector) {
  nameclean = gsub("^[[:blank:]]+", "", vector)
  nameclean = gsub("[[:blank:]]+$", "", nameclean)
  nameclean = gsub("[[:blank:]]+", " ", nameclean)
  nameclean = tolower(nameclean)
  nameclean = gsub("[,.]", "", nameclean)
}
name_clean = eli_blanks(data$name)
name_clean
hometown_clean = eli_blanks(data$hometown)
## combind the clean version of name and hometown
data=data_back
data = cbind(data,name_clean)
data = cbind(data,hometown_clean)
## there are 113190 entrants in total
length(name_clean)
##### How many unique names are there among these entrants?
length(unique(name_clean))
## there are 75382 unique names
##### How many names appear twice, 3 times, 4 times, etc. and what #name occurs most often?
table(table(name_clean))
#We see that over 13000 names appear 2 times throughout the 12 races. #One name appears 26 times, and we know this name must correspond to #at least 2 people because we have only 12 years of race results.
################### create the new ID ########################
## use the clean version of name to create the unique ID
## generate a new variable:birth(represent birth year for each obs)
data$birth = data$year - data$ag
## generate the unique ID combing birth and name
data$id_birth = paste(data$name_clean,data$birth,sep = "_")
## how many unique IDs
length(unique(data$id_birth))
table(table(data$id_birth))
#Since our goal is to study how an athlete’s time changes with age, #let’s focus on those IDs that appear in at least 10 races.
races = tapply(data$year, data$id_birth, length)
races10 = names(races)[which(races >= 10)]
runner10 = data[ data$id_birth %in% races10, ]
runner10 = runner10[order(runner10$id_birth, runner10$year), ]
runner10
### how many unique ids for people who attends the game at least 10 times ?
length(unique(runner10$id_birth))
## what are these players' names and their year of birth?
unique(runner10$id_birth)
## explore people who attend 12 times
races12 = names(races)[which(races == 12)]
runner12 = data[ data$id_birth %in% races12, ]
runner12 = runner12[order(runner12$id_birth, runner12$year), ]
runner12

ggplot(aes(x=year,y=race_time,group=id_birth,color=id_birth),data=runner12)+ geom_step(direction = "hv")+geom_smooth()+
  facet_wrap(~id_birth,nrow=4)+ggtitle("Time Trend for players")
