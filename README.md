# Public-Education-project
#school is back

data <- read.csv("MA_Public_Schools_2017.csv")
str(data)

#explore correlations across variables
install.packages("corrplot")
library(corrplot)
correlations<- cor(data)
corrplot(correlations, order="hclust")
#factors are present
summary(data$AP_Test.Takers)
summary(data$Grade) 

table(data$AP_Test.Takers)
prop.table(data$AP_Test.Takers)
#corr_data <- data[,18:ncol(data)]
#rm(corr_data)

hs_data <- subset(data, X12_Enrollment > 0)
num <- sapply(hs_data, is.numeric)
hs_corr_data <- hs_data[,num]

hs_corr_data$AP_Test.Takers <- as.numeric(hs_data$AP_Test.Takers)
hs_corr_data$AP_Tests.Taken <- as.numeric(hs_data$AP_Tests.Taken)
?cor

head(hs_corr_data)


hs_corr_data$X..MCAS_3rdGrade_Math_A

hs_corr_data_cl <-subset(hs_corr_data, colSums(is.na(hs_corr_data)) > 30)
list<- lapply(hs_corr_data, colSums(is.na(i)))
        
hs_corr_data_cl <- hs_corr_data[colSums(!is.na(hs_corr_data)) > 40]
#install.packages("sqldf")
library(sqldf)

#install.packages("dplyr")
library(dplyr)
hs_data_ready<- 
  select(hs_corr_data_cl, 
       #School.Code, Zip, 
       #District.Code,
       #SP_Enrollment, 
       #TOTAL_Enrollment, 
       X..First.Language.Not.English,
       X..High.Needs,
       X..Economically.Disadvantaged,
       X..African.American,
       X..Asian,
       X..Hispanic,
       X..White,
       #X..Multi.Race..Non.Hispanic,
       X..Males,
       X..Females,
       Average.Class.Size,
       Average.Salary,
       Average.In.District.Expenditures.per.Pupil,
       Average.Expenditures.per.Pupil,
       #X..in.Cohort,
       X..GED,
       X..Non.Grad.Completers,
       X..Dropped.Out,
       X..Attending.College,
       X..Private.Four.Year,
       X..Public.Four.Year,
       X..MA.Community.College,
       X..AP_Score.3.5,
       SAT_Tests.Taken,
       Average.SAT_Reading,
       Average.SAT_Math)


hs_corr_data_cl

hs_data_ready
?cor
correlations<-cor(x=hs_data_ready, use = "pairwise")
corrplot(correlations, method = "circle", order= "hclust", type = "upper")
?corrplot


cool<- subset(hs_data_ready, Average.Class.Size <16)
cool

#is there a trend in NA
#are economically disadvantaged schools not reporting key metrics
summary(hs_data_ready$Average.Salary)
summary(hs_data_ready$Average.Class.Size)
summary(hs_data_ready$Average.SAT_Math)
summary(hs_data_ready$X..Economically.Disadvantaged)
hist(hs_data_ready$X..Economically.Disadvantaged)
summary(hs_data_ready$X..Economically.Disadvantaged)

install.packages("LaplacesDemon")
library(LaplacesDemon)
joint.density.plot(hs_data_ready$Average.SAT_Math, hs_data_ready$X..Economically.Disadvantaged )

#use mice package to find pattern of missing data
#install.packages("mice")
library(mice)
md.pattern(hs_data_ready)

data <- data.frame(md.pattern(hs_data_ready))
data$col_na <- data$v26


##install.packages("VIM")
library(VIM)
#aggr_plot <- aggr(hs_data_ready, col=c('navyblue', 'red'), numbers = TRUE,
#                  sortVars=TRUE, labels=names(data), cex.axis=.7, 
#                  gap=3, ylab=c("Histogram of missing data","Pattern"))

#try with dplyr now
#GOT IT!
#schools with higher percentage of economically disadvantaged students
#tend to have missing values for avg SAT & Passing AP scores

Data_na_scores <-
  hs_data_ready %>% 
  select( everything() ) %>%
  filter(is.na(Average.SAT_Math) 
         & is.na(Average.SAT_Reading) 
         & is.na(X..AP_Score.3.5))
#compare dataset with missing SAT & AP scores with complete data
hist(hs_data_ready$X..Economically.Disadvantaged,
     xlab = "% Of Econ Disadvantaged Students",
     main = "All Schools")

hist(Data_na_scores$X..Economically.Disadvantaged, 
     xlab = "% Of Econ Disadvantaged Students",
     main = "Schools with Missing scores")

#use economically disadvantaged as reference since we know we have data
#for this var across all schools

summary(hs_data_ready$Average.Salary)
summary(hs_data_ready$Average.Class.Size)
summary(hs_data_ready$Average.SAT_Math)
summary(hs_data_ready$X..Dropped.Out)
summary(hs_data_ready$X..AP_Score.3.5)

#lol use this next time, way easier
summary(hs_data_ready)

#missing salary
na_salary <-
  hs_data_ready %>% 
  select( everything() ) %>%
  filter(is.na(Average.Salary))
  
hist(hs_data_ready$X..Economically.Disadvantaged,
     xlab = "% Of Econ Disadvantaged Students",
     main = "All Schools")

hist(na_salary$X..Economically.Disadvantaged,
     xlab = "% Of Econ Disadvantaged Students",
     main = "Missing Avg Teacher Salary")

#missing college
na_college <- 
  hs_data_ready %>% 
  select( everything() ) %>%
  filter(is.na(X..Attending.College))

hist(hs_data_ready$X..Economically.Disadvantaged,
     xlab = "% Of Econ Disadvantaged Students",
     main = "All Schools")

hist(na_salary$X..Economically.Disadvantaged,
     xlab = "% Of Econ Disadvantaged Students",
     main = "Missing Students Attending College")

#look into normalizing all predictors 
#look into imputing values 

#let us normalize & impute
library(caret)
#look at difference between imputed and non imputed normalized data
trans <- preProcess(hs_data_ready,
                    method = c("BoxCox", "center", "scale"))

#apply transformation using maximum likelihood
transformed <- predict(trans, hs_data_ready)

trans_impute <- preProcess(hs_data_ready,
                    method = c("BoxCox", "center", "scale", "knnImpute"))
transformed_imputed <- predict(trans_impute, hs_data_ready)

trans

#install.packages("RANN")
#library(RANN)
transformed <- predict(trans, hs_data_ready)
transformed_imputed <- predict(trans_impute, hs_data_ready)

summary(transformed)
summary(transformed_imputed)
# iam mostly concerned with scores and salaries since it accounted for most of our missing values

hist(transformed$X..AP_Score.3.5,
     xlab = "# of Passed AP Exams",
     main = "Non imputed AP Scores of 3-5")

hist(transformed_imputed$X..AP_Score.3.5,
     xlab = "# of Passed AP Exams",
     main = "Imputed AP Scores of 3-5")

hist(transformed$Average.SAT_Math,
     xlab = "Avg Math SAT Scores",
     main = "Non imputed Math SAT Scores")

hist(transformed_imputed$Average.SAT_Math,
     xlab = "Avg Math SAT Scores",
     main = "Imputed Math SAT Scores")

hist(transformed$Average.Salary,
     xlab = "Avg Teacher Salary",
     main = "Non imputed Salary")

hist(transformed_imputed$Average.Salary,
     xlab = "Avg Teacher Salary",
     main = "Imputed Salary")

#now lets take a closer look at the values that were previously missing
#normalize same way so we can join tables

trans_Scores <- preProcess(Data_na_scores,
                             method = c("BoxCox", "center", "scale"), na.remove = TRUE)
new_NA_scores<- predict(trans_scores, Data_na_scores)

#join by variables i know werent missing 
#i need school code and name as a unique identifier for this
join <- transformed_imputed %>% inner_join(new_na_sc, by = c("X..First.Language.Not.English", 
                                                                  "X..High.Needs", 
                                                                  "X..Economically.Disadvantaged", 
                                                                  "X..African.American", 
                                                                  "X..Asian", 
                                                                  "X..Hispanic", 
                                                                  "X..White", 
                                                                  "X..Males", 
                                                                  "X..Females"))
 join                                         
  

Data_na_scores

imputations<-transformed_imputed %>%
  select(everything()) %>% 
  filter(is.na(Average.SAT_Math))
                         


hist(transformed$X..Economically.Disadvantaged)
hist(transformed$Average.SAT_Math)
#beautiful

#now lets impute
?preProcess
