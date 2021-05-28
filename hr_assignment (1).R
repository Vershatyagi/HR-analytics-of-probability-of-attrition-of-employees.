in_time <- read.csv("in_time.csv" , stringsAsFactors = F)
out_time <- read.csv("out_time.csv" , stringsAsFactors = F)
e_data <- read.csv("employee_survey_data.csv" , stringsAsFactors = F)
g_data <- read.csv("general_data.csv" , stringsAsFactors = F)
m_s_data <- read.csv("manager_survey_data.csv" , stringsAsFactors = F)

require(dplyr)
require(ggplot2)
require(caret)
require(caTools)

##### let's take out the information from in_time data and out_time data regarding the employee's regularity

#### let's check for the equality of number rows and columns in both data frames

nrow(in_time)==nrow(out_time)
ncol(in_time)==ncol(out_time)

#### number of rows and columns in both the dataframe is same

#### let's check for the data entry columns in both data frame

sum(colnames(in_time)==colnames(out_time))

#### this shows columns of both dataframe are same and entry in both the data frame is corresponding to these columns only


##### let's check for the na values in both the data frames
sum(is.na(in_time))
sum(is.na(out_time))

#### number of na's is same in both dataframe , now let's check for columns wise na's

sum(colSums(is.na(in_time))==colSums(is.na(out_time)))

  #### this shows equal number of na's in corresponding columns of both dataframe

sum(rowSums(is.na(in_time))==rowSums(is.na(out_time)))

#### with this we can conclude na's are at same indexes in both data frame , that means employee was absent on that date 

sum(in_time[,1]==out_time[,1])


#### no mistake in first column which is nothing but id column

id <- in_time[,1]


##### (let's proceed to find the information about regularity,punctuality,total_time
#### avg_working_hour, to deal with the date and time 
#### format of given data we take the help of as.POSIXct() function
##### (data entry is in general date and time format)

in_time[,-c(1)] <- as.data.frame(lapply(in_time[,-c(1)], as.POSIXct))

out_time[,-c(1)] <- as.data.frame(lapply(out_time[,-c(1)], as.POSIXct))

time<- out_time[,-1]-in_time[,-1]
time <- as.data.frame(sapply(time,round))

time$id <- id
sum(is.na(time))

time[is.na(time)] <- 0


time$total_time <- rowSums(time[,-262], na.rm = T)
View(time$total_time)

time$avg_time <- round(time$total_time/261)
View(time$avg_time)
summary(time$avg_time)

sum(colSums(time)==0)
##### there are 12 company holidays

df <- rowSums(time==0)
View(df)

df <- df-12
### these are the leaves taken by each employee 

time$leave <- df
summary(time$leave)

##### let's try to find some information about punctuality about employee 
#### how many times employee work greater than or equal to it's standard work hours
###### given in the general data 

sum(is.na(g_data$StandardHours))



time$P <- rowSums(time[,-c(262,263,264,265)] >=g_data$StandardHours)
summary(time$P)


#### 89 is the median value of P values, so we can say if an employee work 
##### for it's standard working hours for more than 89 times than it is punctual otherwise not

time$Punctuality <- ifelse(time$P >= 89 , "Punctual", "Not_Punctual")
table(time$Punctuality)



##### let's try to find some information about REGULARITY of employee , for this
##### a function is customised which gives summation of 0's(which denotes leave) for every 9 days interval 
##### and will apply it to the each rows of time data frame



Summation <- function(vect){
  b <- rep(1,3)
  y=seq(1,length(vect) , by=3)
  j=1 
  n=1
  for(i in 1:length(y)) {
    k=y[n]
   a <-  vect[k:(k+2)]
   b[j] <- sum(a==0)
    j=j+1
    n=n+1
    
  }
  return(b)
  
}



which(colSums(time[,-c(262,263,264,265,266,267)])==0) ## this gives the columns of company holidays



##### used the customized function with dropping the columns of company holidays 
261-12
### factors of 249 are 1,3,83,249

d <- time[,-c(1,10,18,46,87,142,186,197,223,224,225,257,262,263,264,265,266,267)]


df <- apply(d, 1, Summation)
df2 <- matrix(df,ncol=83 , nrow=4410)
df2 <- as.data.frame(df2)

#### to get insight about regularity trend of an employee we customized a function which calculate the overall trend in different intervals
Leave <- function(vect){
  t=0
  for(i in 1:(length(vect)-1)){
    dif<-  vect[i+1]-vect[i]
    if(dif>0){
      t=t+1
    }else{
      if(dif<0){
        t=t-1
        }else{
          t=t
       }
     }
  }
  return(t)
}

Leave(df2[71,])

df2$Trend <- apply(df2,1,Leave)
summary(df2$Trend)
quantile(df2$Trend , seq(0,1,0.01))

##### this shows the trend in leave taking 

df2$Regularity <- ifelse( df2$Trend >0 , "Irregular" , "Regular")
table(df2$Regularity)
table(time$Punctuality)



t_data <- time[,c(262,263,264,265,267)]

t_data$Regularity <- df2$Regularity


table(g_data$Attrition)
table(t_data$Regularity)


##### now we have a data frame which contains information about employees' total working hours ,
#### average working hours ,leave taken, Punctuality and Regularity 

##### Now let's proceed with this information 

rm(d,df,df2,time,in_time, out_time)

master <- merge(g_data , e_data)
master <- merge(master,m_s_data)

t_data$EmployeeID <- t_data$id

master <- merge(master, t_data[,-1])


sum(is.na(master))
colSums(is.na(master))

sum(master=="" | master==" " , na.rm = T)

master <- mutate_if(master, is.character, toupper)

sum(duplicated(master[,-1]))

glimpse(master)


#### let,s deal with na values of environment satisfation column

df <- filter(master, is.na(master$EnvironmentSatisfaction))

#### there is pattern in df departments are research & development and sales  ,also almost all employees are regular
#### first check for Reasearch and development

table(df$Attrition)

d <- filter(master , Department=="RESEARCH & DEVELOPMENT" & Regularity=="REGULAR")

table(d$EnvironmentSatisfaction)

table(master$EnvironmentSatisfaction)
table(df$JobSatisfaction)


### This shows in environment satisfaction table of master data frame level of 3 is greater than 4 but
##### in environment satisfaction table of d data frame level of 4 is greater than the 3
#### thus we can conclude in Research & Development departmnet mostly employees' satisfaction
##### is of level 4  , so we replace na's value in research and development department with 4

sum(is.na(d$EnvironmentSatisfaction))

master$EnvironmentSatisfaction[is.na(master$EnvironmentSatisfaction) &
                                 master$Department=="RESEARCH & DEVELOPMENT"] <- 4


#### now check for sales department

e <- filter(master , Department=="SALES" & Attrition=="NO" & 
              Regularity=="REGULAR")
table(e$EnvironmentSatisfaction)

###### this shows in environment satisfaction table of e data frame level of 3 is highest
##### so  we can replace na's value in sales department with 3

master$EnvironmentSatisfaction[is.na(master$EnvironmentSatisfaction) & 
                                 master$Department=="SALES"] <- 3
sum(is.na(master$EnvironmentSatisfaction))


##### let's now deal with na values of job satisfaction column

df <- filter(master, is.na(master$JobSatisfaction))
 sapply(df, table)

 
 d <- filter(master , Department=="RESEARCH & DEVELOPMENT"  & Attrition=="NO" &
               JobLevel==1 & JobInvolvement>=2 & PerformanceRating>=3) 

 table(d$JobSatisfaction) 
table(master$JobSatisfaction) 

master$JobSatisfaction[is.na(master$JobSatisfaction) & 
                         master$Department=="RESEARCH & DEVELOPMENT"] <- 4
 
e <- filter(master , Department=="SALES"  & Attrition=="NO" &
             JobInvolvement>=2 & PerformanceRating>=3)

table(e$JobSatisfaction)

master$JobSatisfaction[is.na(master$JobSatisfaction) & 
                         master$Department=="SALES"] <- 4

sum(is.na(master$JobSatisfaction))


##### let's deal with na values in num companies worked

df <- filter(master , is.na(master$NumCompaniesWorked))

sapply(df, table)

table(master$NumCompaniesWorked)

d <- filter(master ,Department=="RESEARCH & DEVELOPMENT" & is.na(master$NumCompaniesWorked))
sapply(d, table)

##### by observing some particulars in table of d 

e <- filter(master , Department=="RESEARCH & DEVELOPMENT" & Regularity=="REGULAR" &
              PerformanceRating==3 & BusinessTravel=="TRAVEL_RARELY")

table(e$NumCompaniesWorked)

##### max level is of 1 , so we can replace na values in numcompaniesworked with 1

master$NumCompaniesWorked[is.na(master$NumCompaniesWorked) &
                            master$Department=="RESEARCH & DEVELOPMENT"] <- 1

sum(is.na(master$NumCompaniesWorked)) 

df <- filter(master , is.na(master$NumCompaniesWorked))
sapply(df , table)

e <- filter(master , Department=="SALES" & is.na(master$NumCompaniesWorked) )
sapply(e, table)

f <- filter(master , Department=="SALES" & Attrition=="NO" & PerformanceRating==3)
table(f$NumCompaniesWorked)

##### max level is of 1 , so we can replca ena values in numcompaniewworked in sales department with 1

master$NumCompaniesWorked[is.na(master$NumCompaniesWorked) & 
                            master$Department=="SALES"] <- 1

#### remaining is 1 na value corresponding to HR department for this
 g <- filter(master , Department=="HUMAN RESOURCES")
table(g$NumCompaniesWorked)

#### again max level is of 1 so we can replace this na value with 1

master$NumCompaniesWorked[is.na(master$NumCompaniesWorked) & 
                            master$Department=="HUMAN RESOURCES"] <- 1

sum(is.na(master$NumCompaniesWorked))

##### let's now deal with na values in work life balance

df <- filter(master , is.na(master$WorkLifeBalance))
sapply(df, table)

##### according this there are 3 departments in df , let's have a look in them one by one

e <- filter(master , Department=="RESEARCH & DEVELOPMENT" & is.na(master$WorkLifeBalance))

sapply(e, table)

f <- filter(master , Department=="RESEARCH & DEVELOPMENT" &
            Attrition=="NO" & PerformanceRating>=3 & Regularity=="REGULAR" &
            BusinessTravel=="TRAVEL_RARELY")


table(f$WorkLifeBalance)

##### with this we can observe max level is of 3 so we can replace na values with 3

master$WorkLifeBalance[is.na(master$WorkLifeBalance) & 
                         master$Department=="RESEARCH & DEVELOPMENT"] <- 3


#### now let's find out about sales department

e <- filter(master , Department=="SALES"& is.na(master$WorkLifeBalance))

sapply(e, table)

f <- filter(master , Department=="SALES" & Attrition=="NO" &
            PerformanceRating>=3 & Regularity=="REGULAR")

table(f$WorkLifeBalance)            

##### with this we can observe max level is of 3 so we can replace na values with 3

master$WorkLifeBalance[is.na(master$WorkLifeBalance) &
                         master$Department=="SALES"] <- 3


##### remaining 1 is of human resource department

g <- filter(master , Department=="HUMAN RESOURCES")
table(g$WorkLifeBalance)

##### again max level is of 3, so we can replace this na value with 3

master$WorkLifeBalance[is.na(master$WorkLifeBalance) &
                         master$Department=="HUMAN RESOURCES"] <- 3

sum(is.na(master$WorkLifeBalance))

sum(is.na(master))

##### now let's deal with na values in total working years

df <- filter(master, is.na(master$TotalWorkingYears))
sapply(df, table)

#### according to this table there are 3 departments with totalworkingyears as na

e <- filter(master , Department=="RESEARCH & DEVELOPMENT" & is.na(master$TotalWorkingYears))
sapply(e, table)

f <- filter(master , Department=="RESEARCH & DEVELOPMENT" & WorkLifeBalance==3 &
            BusinessTravel=="TRAVEL_RARELY" & Regularity=="REGULAR")



#### this shows mostly employee of with these condition have totalworkingyears is equal to 10

master$TotalWorkingYears[is.na(master$TotalWorkingYears) &
                           master$Department=="RESEARCH & DEVELOPMENT"] <- 10

#### for remaining we can check for the table of total working years of master data frame

sort(table(master$TotalWorkingYears))

#### this shows maximum level is of 10

master$TotalWorkingYears[is.na(master$TotalWorkingYears)] <- 10

sum(is.na(master))

#### all na values taken care of , now let's move further

sapply(master,n_distinct)

#### standard hours, employeecount , over 18 have distinct value of 1 they can be drop

master <- master[,-c(9,16,18)]
str(master)

master <- mutate_if(master , is.character , as.factor)

master[,c(7,10,16,22,23,24,25,26)] <- as.data.frame(sapply(master[,c(7,10,16,22,23,24,25,26)] , as.factor))

master1 <- master

str(master1)

master <- master2

str(master2)


##### let's do univariant and bivariant analysis

options(scipen=100)

#### 1. id , which is unique key

#### 2. age

histogram(master$Age)

ggplot(master , aes(x=Age , fill=as.factor(Attrition))) +geom_histogram(color="black")
ggplot(master , aes(x=Age , fill=as.factor(Attrition))) +geom_histogram(position = "fill", color="black")

### age can be the factor in terms of an employee's attrition as we can see maximum 
#### number of attritions are from employees in the age group of between 18-30

##### 3. Attrition which is a target variable

##### 4. Business travel

ggplot(master, aes(x=BusinessTravel)) +geom_bar()
ggplot(master , aes(x=BusinessTravel , fill=as.factor(Attrition))) +geom_bar()
ggplot(master , aes(x=BusinessTravel , fill=as.factor(Attrition))) +geom_bar(position = "fill")


 master %>% group_by(BusinessTravel) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)

 master %>% group_by(BusinessTravel) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
 table(master$BusinessTravel)
 
 ##### total employee in category who travel rarely is nearly 4 times who travel_frequently
 ##### but the number of attrition in travel_rarely category is just twice of travel_frequently
 ##### rate of attrition in tavel_frequently is more than the travel rarely and non travel
 
 
 
 
###### 5.Department
 ggplot(master, aes(x=Department)) +geom_bar()
 
 ggplot(master , aes(x=Department , fill=as.factor(Attrition))) +geom_bar()
 
 ggplot(master , aes(x=Department , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 
 master %>% group_by(Department) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 
 table(master$Department)
 
 master %>% group_by(Department) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)

 ##### rate of attrition in Human Resource department is the largest 
 
 
##### 6. Distance from home
 
 table(master$DistanceFromHome)
 
 ggplot(master, aes(x=DistanceFromHome)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=DistanceFromHome , fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=DistanceFromHome , fill=as.factor(Attrition))) +geom_bar(position="fill")
 
d <-  master %>% group_by(DistanceFromHome) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$DistanceFromHome)
 
##### 7. Education

 ggplot(master, aes(x=Education)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=Education , fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=Education , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(Education) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$Education)
 
 master %>% group_by(Education) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
 ### there is overall same pattern of attrition with respect to education
 
##### 8 .Education field
 
 ggplot(master, aes(x=EducationField)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=EducationField , fill=as.factor(Attrition))) +geom_bar() +coord_flip()
 ggplot(master , aes(x=EducationField , fill=as.factor(Attrition))) +geom_bar(position = "fill") +coord_flip()
 
 master %>% group_by(EducationField) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$EducationField)
 
 master %>% group_by(EducationField) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
 ##### rate of attrition in human resources education field is slighly larger than the others
 
###### 9.Gender
 ggplot(master, aes(x=Gender)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=Gender , fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=Gender , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(Gender) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$Gender)
 
 master %>% group_by(Gender) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
 
 ##### Overall same pattern , attrition is not biased in case of gender

 ##### 10. Joblevel
 
 ggplot(master, aes(x=JobLevel)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=JobLevel , fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=JobLevel , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(JobLevel) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$JobLevel)
 
 master %>% group_by(JobLevel) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
  
    ##### it is observed employees at the 2nd level of job are attritated slightly more
 

 ##### 11. Jobrole
 
 ggplot(master, aes(x=JobRole)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=JobRole , fill=as.factor(Attrition))) +geom_bar() +coord_flip()
 ggplot(master , aes(x=JobRole , fill=as.factor(Attrition))) +geom_bar(position = "fill") + coord_flip()
 
 master %>% group_by(JobRole) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$JobRole)
 
 master %>% group_by(JobRole) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
 #### rate of attrition in some of the job role type like research director is more than the others
 
##### 12. Marital Status
 ggplot(master, aes(x=MaritalStatus)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=MaritalStatus , fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=MaritalStatus , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(MaritalStatus) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$MaritalStatus)
 
 master %>% group_by(MaritalStatus) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
 ##### rate of attrition in singles is more
 
 
##### 13.Monthly income 
 ggplot(master, aes(x=MonthlyIncome)) +geom_histogram(fill="Red" , color="Black")
 ggplot(master , aes(x=MonthlyIncome , fill=as.factor(Attrition))) +geom_histogram(color="black")
 ggplot(master , aes(x=MonthlyIncome , fill=as.factor(Attrition))) +geom_histogram(position = "fill" , color="black")
 
 
##### 14.NumCompaniesWorked
 
 table(master$NumCompaniesWorked)
 
 ggplot(master, aes(x=NumCompaniesWorked)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=NumCompaniesWorked , fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=NumCompaniesWorked , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(NumCompaniesWorked) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)

 table(master$NumCompaniesWorked)
 
###### 15.Percent Salary hike
 
 ggplot(master, aes(x=PercentSalaryHike)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=PercentSalaryHike , fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=PercentSalaryHike , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(PercentSalaryHike) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)

###### 16. stock option level
 
 ggplot(master, aes(x=StockOptionLevel)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=StockOptionLevel , fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=StockOptionLevel , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(StockOptionLevel) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$StockOptionLevel)
 
 master %>% group_by(StockOptionLevel) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)

 ##### a maintained rate of attrition with respect to category of stock level
 
###### 17. total working years
 
 
 ggplot(master, aes(x=TotalWorkingYears)) +geom_histogram(fill="Red" , color="Black")
 ggplot(master , aes(x=TotalWorkingYears , fill=as.factor(Attrition))) +geom_histogram(color="black")
 ggplot(master , aes(x=TotalWorkingYears , fill=as.factor(Attrition))) +geom_histogram(position = "fill", color="black")
 
 master %>% group_by(TotalWorkingYears) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$TotalWorkingYears)
 

##### 18.Training time last year
 
 ggplot(master, aes(x=TrainingTimesLastYear)) +geom_histogram(fill="Red" , color="Black")
 ggplot(master , aes(x=TrainingTimesLastYear , fill=as.factor(Attrition))) +geom_histogram(color="black")
 ggplot(master , aes(x=TrainingTimesLastYear , fill=as.factor(Attrition))) +geom_histogram(position = "fill", color="black")
 
 master %>% group_by(TrainingTimesLastYear) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$TrainingTimesLastYear)
 
 master %>% group_by(TrainingTimesLastYear) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
  #### rate of attrition of employees with 0 training last year is more
 
##### 19.  Years At comapny
 
 ggplot(master, aes(x=YearsAtCompany)) +geom_histogram(fill="Red" , color="Black")
 ggplot(master , aes(x=YearsAtCompany , fill=as.factor(Attrition))) +geom_histogram(color="black")
 ggplot(master , aes(x=YearsAtCompany , fill=as.factor(Attrition))) +geom_histogram(position = "fill", color="black")
 
 master %>% group_by(YearsAtCompany) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$YearsAtCompany)
 
##### 20. years since last promotion
 
 ggplot(master, aes(x=YearsSinceLastPromotion)) +geom_histogram(fill="Red" , color="Black")
 ggplot(master , aes(x=YearsSinceLastPromotion , fill=as.factor(Attrition))) +geom_histogram(color="black")
 ggplot(master , aes(x=YearsSinceLastPromotion, fill=as.factor(Attrition))) +geom_histogram(position = "fill", color="black")
 
 master %>% group_by(YearsSinceLastPromotion) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$YearsSinceLastPromotion)

######21. Years with current manager
 
 ggplot(master, aes(x=YearsWithCurrManager)) +geom_histogram(fill="Red" , color="Black")
 ggplot(master , aes(x=YearsWithCurrManager , fill=as.factor(Attrition))) +geom_histogram(color="black")
 ggplot(master , aes(x=YearsWithCurrManager, fill=as.factor(Attrition))) +geom_histogram(position = "fill", color="black")
 
 master %>% group_by(YearsWithCurrManager) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$YearsWithCurrManager)
 
 
#######22. Environment satisfaction
 
 ggplot(master, aes(x=EnvironmentSatisfaction)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=EnvironmentSatisfaction, fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=EnvironmentSatisfaction , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(EnvironmentSatisfaction) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$EnvironmentSatisfaction)
 
 master %>% group_by(EnvironmentSatisfaction) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
 ##### rate of attrition of employee with enviroment satisfaction as 1 is more

###### 23.JobSatisfaction
 
 ggplot(master, aes(x=JobSatisfaction)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=JobSatisfaction, fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=JobSatisfaction , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(JobSatisfaction) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$JobSatisfaction)
 
 master %>% group_by(JobSatisfaction) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
 ##### rate of attrition of employee with job satisfation score is 1 is more and with 4 it is very less
 #### let's check if job satisfaction and environment satisfaction are related to each othe 
 
 ggplot(master, aes(x=JobSatisfaction , fill=EnvironmentSatisfaction)) +geom_bar(position = "fill")
 
##### 24. WorkLifeBalance
 
 ggplot(master, aes(x=WorkLifeBalance)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=WorkLifeBalance, fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=WorkLifeBalance , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(WorkLifeBalance) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$WorkLifeBalance)
 
 master %>% group_by(WorkLifeBalance) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
 #### there is very slight variation in ratio of total distribution among category  work life balance
 #### and ratio of attrition , employee with score of work life balance attrited more

###### 25.JobInvolvement
 
 ggplot(master, aes(x=JobInvolvement)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=JobInvolvement, fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=JobInvolvement , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(JobInvolvement) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$JobInvolvement)
 
 master %>% group_by(JobInvolvement) %>% summarise(perc=sum(Attrition=="YES")) %>% arrange(perc)
 
 ##### ratio of attrition is more in the employees with job involvement score as 1
 
##### 26. PerformanceRating
 
 ggplot(master, aes(x=PerformanceRating)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=PerformanceRating, fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=PerformanceRating , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(PerformanceRating) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$PerformanceRating)
 
 ##### overall same ratio of attrition maintained
 
##### 27. total_time
 
 ggplot(master, aes(x=total_time)) +geom_histogram(fill="Red" , color="Black")
 ggplot(master , aes(x=total_time , fill=as.factor(Attrition))) +geom_histogram(color="black")
 ggplot(master , aes(x=total_time, fill=as.factor(Attrition))) +geom_histogram(position = "fill", color="black")
 
 master %>% group_by(total_time) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$total_time)
 
###### 28. avg_time
 
 ggplot(master, aes(x=avg_time)) +geom_histogram(fill="Red" , color="Black")
 ggplot(master , aes(x=avg_time , fill=as.factor(Attrition))) +geom_histogram(color="black")
 ggplot(master , aes(x=avg_time, fill=as.factor(Attrition))) +geom_histogram(position = "fill", color="black")
 
 master %>% group_by(avg_time) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$avg_time)


####### 29.leave 
 
 ggplot(master, aes(x=leave )) +geom_histogram(fill="Red" , color="Black")
 ggplot(master , aes(x=leave  , fill=as.factor(Attrition))) +geom_histogram(color="black" , bins=20)
 ggplot(master , aes(x=leave , fill=as.factor(Attrition))) +geom_histogram(position = "fill", color="black")
 
 master %>% group_by(leave ) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$leave )
 
###### 30. Punctuality
 
 ggplot(master, aes(x=Punctuality)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=Punctuality, fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=Punctuality , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(Punctuality) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$Punctuality)

###### 31. Regularity
 
 ggplot(master, aes(x=Regularity)) +geom_bar(fill="Red" , color="Black")
 ggplot(master , aes(x=Regularity, fill=as.factor(Attrition))) +geom_bar()
 ggplot(master , aes(x=Regularity , fill=as.factor(Attrition))) +geom_bar(position = "fill")
 
 master %>% group_by(Regularity) %>% summarise(perc=sum(Attrition=="YES")/n()*100) %>% arrange(perc)
 
 table(master$Regularity)

##### with the eda through visualization we conclude that thre are some data deceiving factors some are most important factors
 
 ##### outlier treatment
### we use our customised fumction for outlier function for this
 
 outlier <- function(vect){
   Q <- as.numeric(quantile(vect))
   min <- Q[1] 
   Q1 <- Q[2] 
   Q2 <- Q[3]
   Q3 <- Q[4]
   max <-Q[5]
   IQR <- Q3-Q1
   LO <- Q1-1.5*IQR
   UO <- Q3+1.5*IQR
   for(i in 1:length(vect)){
     if (vect[i]>UO) {
       vect[i]=UO
     }else{
       if(vect[i] < LO){
         vect[i]=LO
       }else{
         vect[i]
       }
     }
     
   }
   return(vect)
   
 }

master[,c(2,6,13,15,17,18,19,20,21,27,28,29)] <- as.data.frame(sapply(master[,c(2,6,13,15,17,18,19,20,21,27,28,29)] , outlier))

boxplot(master[,c(2,6,15,17,18,19,20,21,28,29)])
boxplot(master[,13])
boxplot(master[,27])

#### outlier treated

##### now categories which are ordered factor we can treat them as integer numbers according
##### to the rule of label encoding

master[,c(7,10,16,22,23,24,25,26)] <- as.data.frame(sapply(master[,c(7,10,16,22,23,24,25,26)] , as.integer))


str(master)

master$Attrition <- as.character(master$Attrition)
master$Attrition[master$Attrition=="YES"] <- 1
master$Attrition[master$Attrition=="NO"] <- 0
master$Attrition <- as.factor(master$Attrition)


master1 <- master

##### scaling

master[,-1] <- mutate_if(master[,-1] , is.numeric , scale)


class(master$Gender)

##### model building
require(dummies)
table(master$Attrition)

str(master)

dumm_df <- dummy.data.frame(master[,-3])

dumm_df$Attrition <- master$Attrition

set.seed(123)

index <- sample.split(dumm_df$Attrition, SplitRatio = 0.70)

trn1 <- dumm_df[index, ]
val1 <- dumm_df[!index,]

prop.table(table(dumm_df$Attrition))*100
prop.table(table(trn1$Attrition))*100
prop.table(table(val1$Attrition))*100

#### logistics regression model
 rm(d,D,df,e,e_data,f,g,g_data,m_s_data,t_data)

model_1 <- glm(Attrition ~. , data=trn1[,-1] , family = "binomial")
summary(model_1)

## step reduction
model_2 <- step(model_1, direction = "both")
summary(model_2)

require(car)
 sort(vif(model_2))

#### vif of all predictors is ok , let,s find out the accuracy

pred_Attrition <- predict(model_2, newdata = val1, type="response")
summary(pred_Attrition)

pred_A <- ifelse(pred_Attrition >=0.175 , "1", "0") 
table(pred_A)

confusionMatrix(as.factor(pred_A) , val1$Attrition, positive = "1")

table(val1$Attrition)

#### 74 % balanced accuracy with logistic regression model

##### Tree model

View(master1)
str(master1)

master1[,c(7,10,16,22,23,24,25,26)] <- as.data.frame(sapply(master1[,c(7,10,16,22,23,24,25,26)] , as.factor))

table(master1$Attrition)

set.seed(1234)

index <- sample.split(master1$Attrition, SplitRatio = 0.70)

trn2 <- master1[index, ]
val2 <- master1[!index,]



prop.table(table(master1$Attrition))*100
prop.table(table(trn2$Attrition))*100
prop.table(table(val2$Attrition))*100

require(rpart)
require(rpart.plot)

tree_model <- rpart(Attrition ~. , data=trn2[,-c(1)] ,control = rpart.control(maxdepth = 5 , minsplit = 60), model=T)
prp(tree_model)

pred_Attrition <- predict(tree_model , newdata=val2)

summary(pred_Attrition)

pred_A <- ifelse(pred_Attrition[,2] >=0.15 ,"1", "0")

confusionMatrix(as.factor(pred_A) , val2$Attrition, positive = "1")

#### a balanced accuracy can not be obtained with tree model 
##### overall accuracy is 65%

#### rf model

require(randomForest)

rf_model <- randomForest(Attrition ~. , data=trn2[,-c(1)] , ntrees=100, do.trace=T)

summary(rf_model)

pred_Attrition <- predict(rf_model , newdata = val2, type="prob")
summary(pred_Attrition)

pred_A <- ifelse(pred_Attrition[,2] >=0.167, "1","0" )

confusionMatrix(as.factor(pred_A) , val2$Attrition, positive = "1" )

##### a balanced accuracy of 93.5% is obtained with random forest model

varImpPlot(rf_model)
varImp(rf_model)

##### svm model

require(e1071)
svm_model <- svm(Attrition ~. , data=trn2[,-1] , probability=T, kernel="linear")

pred_Attrition <- predict(svm_model, newdata = val2 , probability = T)

probs <- attr(pred_Attrition ,"probabilities")
summary(probs)

pred_A <- ifelse(probs[,2] >=0.16, "1","0" )


confusionMatrix(as.factor(pred_A) , val2$Attrition, positive = "1")

   #### balanced accuracy of 73% is obtained through svm alogorith with linear kernel

#### let's try with some different kernel

svm_model <- svm(Attrition ~. , data=trn2[,-1] , probability=T, kernel="radial")

pred_Attrition <- predict(svm_model, newdata = val2 , probability = T)

probs <- attr(pred_Attrition ,"probabilities")
summary(probs)

pred_A <- ifelse(probs[,2] >=0.135, "1","0" )


confusionMatrix(as.factor(pred_A) , val2$Attrition, positive = "1")


##### a balanced accuracy of 83% is obtatined from svm model with radial kernel

svm_model <- svm(Attrition ~. , data=trn2[,-1] , probability=T, kernel="polynomial")

pred_Attrition <- predict(svm_model, newdata = val2 , probability = T)

probs <- attr(pred_Attrition ,"probabilities")
summary(probs)

pred_A <- ifelse(probs[,2] >=0.116, "1","0" )


confusionMatrix(as.factor(pred_A) , val2$Attrition, positive = "1")

##### a balanced accuracy of 85% is obtatined from svm model with polynomial kernel


#### thus with random forest we are getting maximum accuracy , and important factors 
#### responsible for the employees' attrition can be concluded from the logistics model
##### and visualization through ggplot

####1. RegularityIRREGULAR
### 2.TrainingTimesLastYear 
#### 3.WorkLifeBalance                      
#### 4.Education 
#### 5.JobLevel     
#### 6.BusinessTravel (rate of attrition in Non travel category is more and then in travel frequently ) 
#### 7.JobInvolvement   
#### 8.Department ( rate of attrition in HUMAN RESOURCES department is more)
##### 9.EnvironmentSatisfaction           
##### 10.JobSatisfaction 
#### 11.leave 
##### 12.MaritalStatus (rate of attrition in singles is more)

##### 13.JobRole ( rate of attrition is in the order RESEARCH DIRECTOR then 
####                RESEARCH SCIENTIST  then SALES EXECUTIVE)
 

##### 14.NumCompaniesWorked 

#### 15.Age (rate of attrition is more in the employee of age group 18-30)   
#### 16.YearsSinceLastPromotion 

##### 17.YearsWithCurrManager       
##### 18.TotalWorkingYears 

##### 19.Punctuality             
##### 20.total_time 



