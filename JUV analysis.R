library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
#get this to work for assoc
#library(vcd)

#import survey data
BBLS <- read.csv(file="BBLS.csv", header=TRUE, sep=",")
#remove variables not useful for analysis
BBLS <- BBLS[ -c(1), -c(2:9,13:26) ]
#rename relevant variables
colnames(BBLS)[2] <- "Q1"
colnames(BBLS)[3] <- "Q2"
colnames(BBLS)[4] <- "Q3"
colnames(BBLS)[5] <- "Q11"
colnames(BBLS)[6] <- "Race describe"
colnames(BBLS)[7] <- "Q12"
colnames(BBLS)[8] <- "Gender describe"
colnames(BBLS)[9] <- "Q13"
colnames(BBLS)[10] <- "Q14"
view(BBLS)

# race = BBLS$Q11
# race = factor(race)
# table(race)
# 
# gender = BBLS$Q12
# gender = factor(gender)
# table(gender)
# 
# states = BBLS$Q13
# states = factor(states)
# table(states)

BBLS$Q1 = factor(BBLS$Q1, levels=c("None", "1 time", "2-3 times",
                                       "4-5 times", "6-11 times", "12-23 times", 
                                       "24+ times"), ordered=TRUE)

# service = BBLS$Q2
# service = factor(service)
# table(service)
# 
# interest = BBLS$Q3
# interest = factor(interest)
# table(interest)

summary(BBLS)

#race pie
ggplot(BBLS, aes(x=factor(1), fill=Q11))+
  geom_bar(width = 1)+
  coord_polar("y")
#gender pie
ggplot(BBLS, aes(x=factor(1), fill=Q12))+
  geom_bar(width = 1)+
  coord_polar("y")

#simplify data to male and female respondents since they were vast majority
filtered_data <- BBLS %>%
  filter(Q12 %in% c("Female", "Male"))

#Density plot showing frequency and gender
ggplot(filtered_data) +
  geom_density(aes(filtered_data$Q1, fill = Q12), alpha=0.3)

summary(BBLS$Q1[BBLS$Q12 == 'Male'], basic = T)
summary(BBLS$Q1[BBLS$Q12 == 'Female'], basic = T)

#frequency differences between male and female

mytable <- xtabs(~Q1+Q12, data=filtered_data)
mytable <- mytable[, c(1,2)]
mytable
#plot for frequency/gender
barplot(mytable,horiz=T, xlim=c(0,100), col = brewer.pal(n = 8, name = "RdBu"), 
        las=1, legend=rownames(mytable), beside= TRUE, main = "Frequency of Movie Theater Attendance by Gender")

#SD 
apply(mytable[,1:2], 2, sd)
chisq.test(mytable)
assocplot(mytable)

#does not seem like we can say that the differences are meaningful 

#interest differences between males and females
mytable2 <- xtabs(~Q3+Q12, data=filtered_data)
ftable(mytable2) # print table 
mytable2 <- mytable2[c(1:6), c(1,2)]
mytable2
#Plot for interest/gender
barplot(mytable2,horiz=T, xlim=c(0,150), col = brewer.pal(n = 8, name = "RdBu"), 
        las=1, legend=rownames(mytable), beside= TRUE, main = "Interest by Gender")

chisq.test(mytable2)
mosaicplot(mytable2)
assocplot(mytable2, space=.3, xlab = " Initial Interest",
          ylab="Gender")
assoc(mytable2)
#interest in streaming services
mytable3 <- xtabs(~Q2+Q12, data=filtered_data)
mytable3 <- mytable3[c(1:5,7,8), c(1,2)]
mytable3
chisq.test(mytable3)

#create separate datasets for males and females
male_data <- BBLS %>%
  filter(Q12 %in% c("Male"))
female_data <- BBLS %>%
  filter(Q12 %in% c("Female"))

#frequency of movie by gender
ggplot(data.frame(male_data), aes(x=Q1)) +
  geom_bar()+
  labs(title = "Freuqency of Movie Theater Attendance Among Male Respondents")

ggplot(data.frame(female_data), aes(x=Q1)) +
  geom_bar()+
  labs(title = "Freuqency of Movie Theater Attendance Among Female Respondents")

#interest in movie by gender
par(mar=c(5,12,4,12)+.1)
ggplot(data.frame(male_data), aes(x=Q3)) +
  geom_bar()+
  labs(title = "Male Interest in Blinded by the Light")

ggplot(data.frame(female_data), aes(x=Q3)) +
  geom_bar() +
  labs(title = "Female Interest in Blinded by the Light")


#race information and frequency
racetable <- xtabs(~Q1+Q11, data=BBLS)
round(prop.table(racetable),3)*100
#gives percentages of total group
g3<-round(prop.table(racetable),3)*100
#is there a way i can make plots for g3
racetable <-racetable[, -c(5)]
racetable
#side by side barplot showing race and frequency
par(mar=c(5,12,4,1)+.1)
barplot(racetable,horiz=T, xlim=c(0,50), col = brewer.pal(n = 8, name = "RdBu"), 
        las=1, legend=rownames(g3), main="Frequency of Movie Theater Attendance by Race",
        beside= TRUE)

#testing for variation for race and Q1
mosaicplot(racetable, shade=TRUE)
chisq.test(racetable)


#race and services
racetable2 <- xtabs(~Q2+Q11, data=BBLS)
racetable2 <-racetable2[-c(6), -c(5)]
racetable2
chisq.test(racetable2)


#race and interest
racetable3 <- xtabs(~Q3+Q11, data=BBLS)
racetable3 <-racetable3[-c(7), -c(5)]
racetable3
#plot for race and interest
par(mar=c(5,12,4,1)+.1)
barplot(racetable3,horiz=T, xlim=c(0,50), las=1, col = brewer.pal(n = 8, name = "RdBu"),
        legend = rownames(racetable3), beside=TRUE, main="Interest by Race")
chisq.test(racetable3)

#Density Plot
ggplot(BBLS) +
  geom_density(aes(BBLS$Q1, fill = Q11), alpha=0.3) +
  labs(title = "Denisty Plot of Movie Theater Frequency Across Various Races")

#Black Data
black_data <- BBLS %>%
  filter(Q11 %in% c("Black/African-American"))
#white
white_data <- BBLS %>%
  filter(Q11 %in% c("White/Caucasian"))
#east asian
eastasian_data <- BBLS %>%
  filter(Q11 %in% c("East Asian-American"))
#south asian
southasian_data <- BBLS %>%
  filter(Q11 %in% c("South Asian-American"))
#hispanic
hispanic_data <- BBLS %>%
  filter(Q11 %in% c("Spanish, Hispanic, or Latino"))
#other
other_data <- BBLS %>%
  filter(Q11 %in% c("Other"))

#Look at interest across racial groups

ggplot(data.frame(black_data), aes(x=Q3)) +
  geom_bar() +
  labs(title = "Question 3 Across Black Respondents")
ggplot(data.frame(white_data), aes(x=Q3)) +
  geom_bar()+
  labs(title = "Question 3 Across White Respondents")
ggplot(data.frame(eastasian_data), aes(x=Q3)) +
  geom_bar()+
  labs(title = "Question 3 Across East Asian Respondents")
ggplot(data.frame(southasian_data), aes(x=Q3)) +
  geom_bar()+
  labs(title = "Question 3 Across South Asian Respondents")
ggplot(data.frame(hispanic_data), aes(x=Q3)) +
  geom_bar()+
  labs(title = "Question 3 Across Hispanic Respondents")
ggplot(data.frame(other_data), aes(x=Q3)) +
  geom_bar()+
  labs(title = "Question 3 Across Respondents Who Marked Race as Other")




servicetable <- xtabs(~Q2+Q11, data=BBLS)

#gives percentages of total group
g3<-round(prop.table(racetable),3)*100
#is there a way i can make plots for g3

#Regions
NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref)
BBLS$Q20 <- sapply(BBLS$Q13, 
                     function(x) names(region.list)[grep(x,region.list)])

BBLS$Q20 <-factor(BBLS$Q20, levels = c("South", "Northeast", "Midwest", "West"))

#regiontable about frequency
regionfrequency <- xtabs(~Q1+Q20, data=BBLS)
regionfrequency
chisq.test(regionfrequency)
#shows the variation
mosaicplot(regionfrequency, shade=TRUE)
assocplot(regionfrequency)

#region interest
regiontable3 <- xtabs(~Q3+Q20, data=BBLS)
regiontable3 <-regiontable3[-c(7), -c(5)]
regiontable3
chisq.test(regiontable3)
mosaicplot(regiontable3)
assocplot(regiontable3)

