library(forcats)
library(plyr)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(ggthemes)
library(grid)
library(gridExtra)
library(factoextra)
library(FactoMineR)
library(readxl)
library(PCAmixdata)
library(data.table)
library(arules)
library(export)
##Import database
Absenteeism_at_work <- read_excel("Downloads/Absenteeism_at_work_edited.xls")

class(Absenteeism_at_work)

#View it on a table format
View(Absenteeism_at_work)

#Show the dimension of our dataset
dim(Absenteeism_at_work)

#Show the type of every database variable
str(Absenteeism_at_work)

#Show the name of every variable
names(Absenteeism_at_work)

#Show the column name, the row names and the class/type of the dataset
attributes(Absenteeism_at_work)
attributes(test)

#Calculate mean, median, first and third quartiles
summary(Absenteeism_at_work)

#We delete every useless columnn of information in our dataset (Pet, Weigth, Heigth,...)
col <- c(5,8,11,19:21)
absent <- Absenteeism_at_work[-col]

col2 <- c(2:4,8,12,14:15)

# We are dealing with two type of variable, numeric and factors, let's convert what's in our dataframe
#into factors
absent[col2] <- lapply(absent[col2], factor)
absent <- absent %>%
  mutate(`Reason for absence` = fct_recode(`Reason for absence`,'infectious,parasitic diseases'='0',
                                         'Neoplasms'='1','Diseases of the blood'='2','Endocrine and metabolic diseases'='3','Mental and behavioural disorders'='4', 
                                         'Diseases of the nervous system'='5','Diseases of the eye and adnexa'='6','Diseases of the ear and mastoid process'='7',
                                         'Diseases of the circulatory system'='8','Diseases of the respiratory system'='9','Diseases of the digestive system'='10', 
                                         'Diseases of the skin and subcutaneous tissue'='11','Diseases of the musculoskeletal system and connective tissue'='12', 
                                         'Diseases of the genitourinary system'='13','Pregnancy, childbirth and the puerperium'='14','Certain conditions originating in the perinatal'='15', 
                                         'Congenital malformations, deformations and chromosomal abnormalities'= '16','Symptoms, signs and abnormal clinical  findings'='17',
                                         'Injury, poisoning and certain other consequences of external causes'= '18','causes of morbidity and mortality'='19',
                                         'Factors influencing health status and contact with health services'='21','patient follow-up'='22','medical consultation'='23','blood donation'='24',
                                         'laboratory examination'='25','unjustified absence'='26','physiotherapy'='27','dental consultation'='28'))

absent <- absent %>%
  mutate(`Month of absence`= fct_recode(`Month of absence`,'None'='0','Jan'='1','Feb'='2','Mar'='3','Apr'='4','May'='5',
                                      'Jun'='6','Jul'='7','Aug'='8','Sep'='9','Oct'='10','Nov'='11','Dec'='12') )

absent <- absent %>%
  mutate(`Day of the week` = fct_recode(`Day of the week`,"Monday"="2","Tuesday"="3","Wednesday"="4","Thursday"="5","Friday"="6"))

absent <- absent %>%
  mutate(Education = fct_recode(Education,'highschool'='1','graduate'='2','postgraduate'='3','master& doctrate'='4'))

absent <- absent %>%
  mutate(`Social drinker`= fct_recode(`Social drinker`,'No'='0','Yes'='1'))

absent <- absent %>%
  mutate(`Social smoker`= fct_recode(`Social smoker`,'No'='0','Yes'='1'))
summary(absent)
class(absent)
#We select every time where the personn has effectively been away
absent <- as.data.frame(absent)

s <- ggplot(absent, aes(x = Son, fill = Son)) + geom_bar()
boxplot(absent, by=`Absenteeism time in hours`)
t <- ggplot(absent, aes(x = absent$`Body mass index`, fill = absent$`Body mass index`)) + geom_bar()
plot_boxplot(absent, by = "Absenteeism time in hours")
pairs(absent)
#Here I choose to separate factor and numeric
col3 <- c(1,col2)
numVariable <- absent[-col2]
factVariable <- absent[col3]

numVariable <- group_by(numVariable,numVariable$ID)
factVariable <- group_by(factVariable,factVariable$ID)
factVariable

factVariableByPeople <- summarize(factVariable, ReasonByPeople = mean(`Reason for absence`))
factVariableByPeople
numVariable
numVarByPeople <- summarize(numVariable, dist = mean(`Distance from Residence to Work`, na.rm = T),
                    serviceTime = mean(`Service time`, na.rm = T),
                    age = mean(`Age`, na.rm = T),
                    workLoad = mean(`Work load Average/day`, na.rm = T),
                    HitTarget = mean(`Hit target`, na.rm = T),
                    son = mean(`Son`, na.rm = T),
                    bodyMassIndex = mean(`Body mass index`, na.rm = T),
                    AbsInHours = mean(`Absenteeism time in hours`, na.rm = T),
                    countOfAbs = n(),
                    totalAbs = sum(`Absenteeism time in hours`, na.rm = T))
numVarByPeople
str(absent)
numVarByPeopleFiltered <- as.data.frame( numVarByPeople %>% select(everything()) %>% filter(numVarByPeople$AbsInHours < 30))

#This is to see if there any correlation between the total hours of absence and the other variables
#We can also see that service time and age are connected.
pairs(numVarByPeople)

#This one is more particular, it exprims the relation between distance_from_work and total hour of absence
plot(numVarByPeople$dist,numVarByPeople$totalAbs)

#This gives the correlation between them
cor(numVarByPeople$totalAbs,numVarByPeople$dist)

#Let's check if we can find any linear combination to explain the variable totalAbs
linearReg <- lm(numVarByPeople$totalAbs~numVarByPeople$dist
                +numVarByPeople$age
                +numVarByPeople$workLoad
                +numVarByPeople$son
                +numVarByPeople$bodyMassIndex)
summary(linearReg)
#With the R-squared error we can say that only 7% of the variable is explain by the model, so the linear model is not a good choice.
#Since every data is numeric, we can perform ACP, first, we need the dataset to be a dataframe type. Let's cast it that way
df = as.data.frame(numVarByPeople)
resAcp <- PCAmix(df)
resAcp$eig
plot(resAcp,choice="cor")
plot(resAcp,choice="cor",axes = c(1,3))
plot(absent$`Body mass index`,absent$`Absenteeism time in hours`)

FirstPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 1))
TwoPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 2))
ThreePeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 3))
FourPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 4))
FivePeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 5))
SixPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 6))
SevenPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 7))
EightPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 8))
NinePeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 9))
TenPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 10))
ElevenPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 11))
TwelvePeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 12))
ThirteenPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 13))
FourteenPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 14))
FifteenPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 15))
SixteenPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 16))
SeventeenPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 17))
EigthteenPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 18))
NineteenFirstPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 19))
TwentyFirstPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 20))
Twenty1FirstPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 21))
Twenty2PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 22))
Twenty3PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 23))
Twenty4PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 24))
Twenty5PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 25))
Twenty6PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 26))
Twenty7PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 27))
Twenty8PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 28))
Twenty9PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 29))
ThirtyPeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 30))
Thirty1PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 31))
Thirty2PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 32))
Thirty3PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 33))
Thirty4PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 34))
Thirty5PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 35))
Thirty6PeopleFiltered <- as.data.frame( absent %>% select(everything()) %>% filter(absent$ID == 36))

summary(FirstPeopleFiltered)
summary(TwoPeopleFiltered)
summary(ThreePeopleFiltered)
summary(FourPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)
summary(FirstPeopleFiltered)

Reason <-  as.data.frame(FirstPeopleFiltered %>% group_by(FirstPeopleFiltered$`Reason for absence`) %>% 
                           summarise(count= n(), percent = round(count*100/nrow(FirstPeopleFiltered),1))%>% arrange(desc(count)))
Reason
ggplot(Reason,aes(x = reorder(FirstPeopleFiltered$`Reason for absence`,percent), y= percent, fill= FirstPeopleFiltered$`Reason for absence`)) + geom_bar(stat = 'identity') + coord_flip() + theme(legend.position='none') + geom_text(aes(label = percent), vjust = 0.5, hjust = 1.1) + xlab('Reason for absence')

#Clustering :
absent1 <- absent
absent1$`Reason for absence`<-NULL
absent1$`Month of absence`<-NULL
absent1$`Day of the week`<-NULL
absent1$Education<-NULL
absent1$`Social drinker`<-NULL
absent1$`Social smoker`<-NULL
absent1$`Type of Absenteism` <- NULL
kmeans.result<-kmeans(absent1,3)
kmeans.result
table(absent$`Day of the week`, kmeans.result$cluster)
table(absent$`Reason for absence`, kmeans.result$cluster)
table(absent$Education, kmeans.result$cluster)
table(absent$Son, kmeans.result$cluster)
plot(absent1[c("Body mass index", "Absenteeism time in hours")], col = kmeans.result$cluster)
#points(kmeans.result$centers[, c("Sepal.Length", "Sepal.Width")],
       #col = 1:3, pch = 8, cex = 2) 
rules.all <- apriori(absent[col])
inspect(rules.all)
export::table2excel(numVarByPeople)
