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
library(klaR)
library(ISLR)
library(class)
library(mclust)
##Import database
Absenteeism_at_work <- read_excel("Downloads/Absenteeism_at_work_edited.xls")
Rtable_with_reason_of_absence <- read_excel("Downloads/Rtable_with_reason_of_absence.xlsx")
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
boxplot(absent$`Absenteeism time in hours`)
plot_boxplot(absent, by = "Absenteeism time in hours")
pairs(absent)
#Here I choose to separate factor and numeric
col3 <- c(1,col2)
numVariable <- absent[-col2]
factVariable <- absent[col3]

numVariable <- group_by(numVariable,numVariable$ID)

numVarByPeople <- summarize(numVariable, dist = mean(`Distance from Residence to Work`, na.rm = T),
                    serviceTime = mean(`Service time`, na.rm = T),
                    age = mean(`Age`, na.rm = T),
                    workLoad = mean(`Work load Average/day`, na.rm = T),
                    HitTarget = mean(`Hit target`, na.rm = T),
                    son = mean(`Son`, na.rm = T),
                    bodyMassIndex = mean(`Body mass index`, na.rm = T),
                    meanOfAbs = mean(`Absenteeism time in hours`, na.rm = T),
                    numbOfAbs = n(),
                    totalAbs = sum(`Absenteeism time in hours`, na.rm = T))
numVarByPeople
str(absent)
plot(numVarByPeople$meanOfAbs,numVarByPeople$age)
points(numVarByPeople$meanOfAbs[numVarByPeople$meanOfAbs==32.75],numVarByPeople$age[numVarByPeople$age==58],
       col="red", pch = 16)
#Doing the same with factor variables
pairs(factVariable)

col4 <- c(2:4,9,10)
col5 <- c(5:10)
#This is to see if there any correlation between the total hours of absence and the other variables
#We can also see that service time and age are connected.
pairs(numVarByPeople[col5])
pairs(numVarByPeople[col4])
#This one is more particular, it exprims the relation between distance_from_work and total hour of absence
plot(numVarByPeople$dist,numVarByPeople$totalAbs)
cor(numVarByPeople$totalAbs,numVarByPeople$dist)

#This gives the correlation between service time and age
cor(absent$`Service time`,absent$Age)
res <- lm(numVarByPeople$serviceTime~numVarByPeople$age)
summary(res)

#Linear Regression :

#Let's check if we can find any linear combination to explain the variable totalAbs
linearReg <- lm(absent$`Absenteeism time in hours`~ absent$`Reason for absence`
                +absent$`Month of absence`
                +absent$`Day of the week`
                +absent$`Work load Average/day`
                +absent$`Hit target`
                +absent$`Distance from Residence to Work`
                +absent$`Service time`
                +absent$Age
                +absent$Education
                +absent$Son
                +absent$`Social drinker`
                +absent$`Social smoker`
                +absent$`Body mass index`)
linearReg3 <- lm(absent$`Absenteeism time in hours`~absent$`Day of the week`)
summary(linearReg3)
summary(linearReg)
step(linearReg)
linearReg$coefficients
linearReg2 <- lm(absent$`Absenteeism time in hours`~ absent$`Reason for absence`
                 +absent$`Day of the week`
                 +absent$`Distance from Residence to Work`
                 +absent$Age
                 +absent$Son
                 +absent$`Social drinker`)
summary(linearReg2)
#With the R-squared error we can say that only 7% of the variable is explain by the model, so the linear model is not a good choice.
#Since every data is numeric, we can perform ACP, first, we need the dataset to be a dataframe type. Let's cast it that way
df = as.data.frame(numVarByPeople)
resAcp <- PCAmix(df)
resAcp$eig
plot(resAcp,choice="cor")
plot(resAcp,choice="cor",axes = c(1,3))
plot(absent$`Body mass index`,absent$`Absenteeism time in hours`)


model1 <- lm(absent$`Absenteeism time in hours`~absent$Age+absent$`Social smoker`)
plot(absent$Age[absent$`Social smoker`=="No"],absent$`Absenteeism time in hours`[absent$`Social smoker`=="No"],
     col="blue",ylim = c(0,100), xlab = "Age", ylab = "Absenteism", main = "Absenteeism vs Age,Smoke")

points(absent$Age[absent$`Social smoker`=="Yes"],absent$`Absenteeism time in hours`[absent$`Social smoker`=="Yes"],
     col="red", pch = 16)
legend(25,100,legend = c("Non Smoker","Smoker"), col = c("blue","red"), pch = c(1,16), bty = "n")

abline(a=0.66,b=0.19, col = "blue", lwd = 3)
abline(a=0.34, b=0.19, col = "red", lwd = 3)
#This shows that Smoking is not really inluencing Absenteeism, to find the second abline, I substract the coef of social smoker 0.32 to intercept
#Clustering K-Modes :
kmodes.result <- kmodes(absent,10)
kmodes.result
kmodes.result$cluster
modes <- kmodes.result$modes
#IDkmodes
IDkmodes.result <- kmodes(numVarByPeople, 5)
IDkmodes.result
IDmodes <- IDkmodes.result$modes
newgoodDB <- as.data.frame(Rtable_with_reason_of_absence)
Rtable_with_reason_of_absence[2:3] <- lapply(Rtable_with_reason_of_absence[2:3], factor)
IDkmodes2.result <- kmodes(newgoodDB, 5)
IDkmodes2.result
IDmodes2 <- IDkmodes2.result$modes
#export
export::table2excel(numVarByPeople)
export::table2excel(modes)
export::table2excel(IDmodes)
export::table2excel(IDmodes2)
#Decision Tree :
require(tree)
hist(absent$`Absenteeism time in hours`)
tree.absent = tree(absent$`Type of Absenteism`~absent$, data = absent)


#independance de variables :
chisq.test(absent$`Absenteeism time in hours`,absent$`Reason for absence`)
chisq.test(absent$`Absenteeism time in hours`,absent$`Month of absence`)
chisq.test(absent$`Absenteeism time in hours`,absent$`Day of the week`)
chisq.test(absent$`Absenteeism time in hours`,absent$`Work load Average/day`)
chisq.test(absent$`Absenteeism time in hours`,absent$`Hit target`)
chisq.test(absent$`Absenteeism time in hours`,absent$`Type of Absenteism`)
chisq.test(absent$`Absenteeism time in hours`,absent$`Distance from Residence to Work`)
cor(numVarByPeople)
shapiro.test(absent)
shapiro.test(numVariable)
shapiro.test(Absenteeism_at_work$`Absenteeism time in hours`) #cette distribution n'est pas normale
shapiro.test(numVarByPeople$totalAbs) #cette distribution n'est pas normale


##Classification with nearest neighbourg

#First, we split our dataset in one training data and one testing data.
ind <- sample(2, nrow(Absenteeism_at_work), replace = TRUE, prob = c(0.7, 0.3))
train.data <- Absenteeism_at_work[ind == 1, ]
test.data <- Absenteeism_at_work[ind == 2, ]
#Then we transform the type of "Type of Absenteeism" from character to factor because kNN algo return a factor
Absenteeism_at_work[10] <- lapply(Absenteeism_at_work[10], factor)
test.data[10] <- lapply(test.data[10], factor)
train.data[10] <- lapply(train.data[10], factor)
#We create a vector with only type of Absenteeism label
train.labels <- train.data$`Type of Absenteism`
train.labels
col6 <- c(9,10)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
dfNormTrain <- as.data.frame(lapply(train.data[-col6], normalize))
dfNormTest <- as.data.frame(lapply(test.data[-col6], normalize))
#We launch the kNN algorithm, this algorithm will return a vector with the estimated value of test.data$'Type of Absenteeism'
new <- knn(train = dfNormTrain, test = dfNormTest, cl = train.labels,k=16)
#Then, thanks to library arsenal, we're able to compare, effective value and estimated value
library(arsenal)
compare(test.data$`Type of Absenteism`,new)
x<-data.frame("Real"=test.data$`Type of Absenteism`,"Calculated"=new)
#From the output of the algorithm, we can say that this model isn't a good one because the estimation of type of Absenteeism
#is only accurate at 64,25%

## Clustering Model
clusterDB <- data.frame(newgoodDB,)
classif <- newgoodDB$
table(classif)
col7 <- c(1:3)
X <- newgoodDB[-col7]
head(X)
clPairs(X,classif)
BIC<- mclustBIC(X)
plot(BIC)
index <- c(1:100)
plot(rnorm(100),index)

#Random Forest
set.seed(123)
library(randomForest)
fit <- randomForest(Absenteeism_at_work$`Type of Absenteism` ~ Absenteeism_at_work$`Reason for absence`
                    +Absenteeism_at_work$`Month of absence`
                    +Absenteeism_at_work$`Day of the week`
                    +Absenteeism_at_work$`Work load Average/day`
                    +Absenteeism_at_work$`Hit target`
                    +Absenteeism_at_work$`Distance from Residence to Work`
                    +Absenteeism_at_work$`Service time`
                    +Absenteeism_at_work$Age
                    +Absenteeism_at_work$Education
                    +Absenteeism_at_work$Son
                    +Absenteeism_at_work$`Social drinker`
                    +Absenteeism_at_work$`Social smoker`
                    +Absenteeism_at_work$`Body mass index`, data = Absenteeism_at_work, na.action = na.roughfix)
plot(fit)
print(fit)
