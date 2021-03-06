
#school is back
getwd()

data <- read.csv("MA_Public_Schools_2017.csv")
str(data)

#explore correlations across variables
#install.packages("corrplot")
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
         TOTAL_Enrollment, 
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
         Average.SAT_Math) #%>%
  #mutate(salary_to_size = Average.Salary/Average.Class.Size)




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



Data_na_scores <-
  hs_data_ready %>% 
  select( everything() ) %>%
  filter(is.na(Average.SAT_Math) 
         & is.na(Average.SAT_Reading) 
         & is.na(X..AP_Score.3.5))
#compare dataset with missing SAT & AP scores with complete data
library(ggplot2)

hs_data_ready %>%
  ggplot(aes(x=X..Economically.Disadvantaged)) + 
  ggtitle("All Schools") +
  geom_histogram(bins=20, fill = "red") +
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count") 

hist(hs_data_ready$X..Economically.Disadvantaged,
     xlab = "% Of Econ Disadvantaged Students",
     main = "All Schools")

Data_na_scores %>%
  ggplot(aes(x=X..Economically.Disadvantaged)) + 
  ggtitle("Schools with missing scores") +
  geom_histogram(bins=20, fill = "red") +
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count") 

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

#declaring score 
hs_data_ready$salary_class_size <- hs_data_ready$Average.Salary/Average.SAT_Math$Average.Class.Size

#let us normalize & impute
library(caret)


trans_impute <- preProcess(hs_data_ready,
                           method = c("BoxCox", "center", "scale", "knnImpute"))
transformed_imputed <- predict(trans_impute, hs_data_ready)

trans

#install.packages("RANN")
library(RANN)
transformed <- predict(trans, hs_data_ready)
transformed_imputed <- predict(trans_impute, hs_data_ready)

summary(transformed)
summary(transformed_imputed)
# iam mostly concerned with scores and salaries since it accounted for most of our missing values

#Checking distrubition of values now 

transformed_imputed %>%
  ggplot(aes(x=X..Economically.Disadvantaged)) + 
  ggtitle("Imputed & Normalized Data") +
  geom_histogram(bins=20, fill = "red") +
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count") 

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


#relationships of interest

#correlations on imputed values
hs_data_quick<- 
  select(transformed_imputed, 
         TOTAL_Enrollment,
         X..High.Needs,
         X..Economically.Disadvantaged,
         Average.Class.Size,
         Average.Salary,
         Average.In.District.Expenditures.per.Pupil,
         Average.Expenditures.per.Pupil,
         Average.SAT_Math)

correlations_quick<-cor(x=hs_data_quick, use = "pairwise")
corrplot(correlations_quick, method = "circle", order= "hclust")


a<- ggplot(transformed_imputed, aes(Average.SAT_Math, X..Economically.Disadvantaged))
a + geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "% of Economically Disadvantaged vs. Average SAT Math Scores")

b<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Salary))
b + geom_point() + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Average.Salary vs. Average SAT Math Scores")

c<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Class.Size))
c + geom_point()+ 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Average.Class.Size vs. Average SAT Math Scores")

d<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Expenditures.per.Pupil))
d + geom_point()+ 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Average.Expenditures.per.Pupil vs. Average SAT Math Scores")

e<- ggplot(transformed_imputed, aes(Average.SAT_Math, TOTAL_Enrollment))
e + geom_point() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Total Enrollment vs. Average SAT Math Scores")

#linear regressions to compare relationships while contorlling for economic disadvantage


#lets start exploring relationships
#lets try clustering schools

#hierarichal clustering using euclidean distance 
distances <- dist(transformed_imputed, method = "euclidean")
cluster_schools <- hclust(distances, method = "ward.D")
plot(cluster_schools)
#based on cluster dendogram we would pick 2 or 3 

#lets get better understanding of clusters
clusterGroups <- cutree(cluster_schools, k=4)

tapply(transformed_imputed$X..Economically.Disadvantaged, clusterGroups, mean)
tapply(transformed_imputed$X..AP_Score.3.5, clusterGroups, mean)
tapply(transformed_imputed$X..Hispanic, clusterGroups, mean)
tapply(transformed_imputed$Average.SAT_Math, clusterGroups, mean)
tapply(transformed_imputed$Average.Salary, clusterGroups, mean)
tapply(transformed_imputed$Average.Class.Size, clusterGroups, mean)
table(clusterGroups)

set.seed(88)
k=4
kmeansGroups <- kmeans(transformed_imputed, centers = k, iter.max = 1000)
school_k_clusters <- kmeansGroups$cluster
#how many schools fall into each cluster
table(school_k_clusters)



tapply(transformed_imputed$X..Economically.Disadvantaged, school_k_clusters, mean)
tapply(transformed_imputed$Average.SAT_Math, school_k_clusters, mean)
tapply(transformed_imputed$Average.Salary, school_k_clusters, mean)
tapply(transformed_imputed$Average.Class.Size, school_k_clusters, mean)
tapply(transformed_imputed$X..Hispanic, school_k_clusters, mean)
tapply(transformed_imputed$X..African.American, school_k_clusters, mean)

#summary of each variable by cluster
library(purrr)

transformed_imputed %>% split(.$kmeans) %>% map(summary)

#define new variable as cluster
transformed_imputed$kmeans <- as.factor(school_k_clusters)
transformed_imputed$kmeans
str(transformed_imputed$kmeans)

tapply(transformed_imputed, kmeans, mean)
summary(transformed_imputed)

summary <-transformed_imputed %>%
  group_by(kmeans) %>%
  summarize_all(funs(mean)) 

#graphing frequency of each cluster
ggplot(transformed_imputed, aes(x=kmeans)) +
  geom_bar(width = 0.5) +
  xlab("Clusters") + 
  ylab("Total Count") 



#correlations on clusters 
hs_data_Cluster1<- 
  select(transformed_imputed, 
         X..High.Needs,
         X..Economically.Disadvantaged,
         Average.Class.Size,
         Average.Salary,
         Average.In.District.Expenditures.per.Pupil,
         Average.Expenditures.per.Pupil,
         Average.SAT_Math,
         kmeans) %>%
  filter(kmeans == 1)

#linear regression cluster 1
linear_cluster1 <- lm(Average.SAT_Math~ X..Economically.Disadvantaged + Average.Salary + Average.Class.Size, data = hs_data_Cluster1)
summary(linear_cluster1)

correlations_cluster1<-cor(x=hs_data_Cluster1[1:7,], use = "pairwise")
corrplot(correlations_cluster1, method = "circle", order= "hclust")

hs_data_Cluster2<- 
  select(transformed_imputed, 
         X..High.Needs,
         X..Economically.Disadvantaged,
         Average.Class.Size,
         Average.Salary,
         Average.In.District.Expenditures.per.Pupil,
         Average.Expenditures.per.Pupil,
         Average.SAT_Math,
         kmeans) %>%
  filter(kmeans == 2)

hs_data_Cluster3<- 
  select(transformed_imputed, 
         X..High.Needs,
         X..Economically.Disadvantaged,
         Average.Class.Size,
         Average.Salary,
         Average.In.District.Expenditures.per.Pupil,
         Average.Expenditures.per.Pupil,
         Average.SAT_Math,
         kmeans) %>%
  filter(kmeans == 3)

hs_data_Cluster4<- 
  select(transformed_imputed, 
         X..High.Needs,
         X..Economically.Disadvantaged,
         Average.Class.Size,
         Average.Salary,
         Average.In.District.Expenditures.per.Pupil,
         Average.Expenditures.per.Pupil,
         Average.SAT_Math,
         kmeans) %>%
  filter(kmeans == 4)

#linear regression cluster 2 
linear_cluster1 <- lm(Average.SAT_Math~ X..Economically.Disadvantaged + Average.Salary + Average.Class.Size + 
                        Average.Salary * Average.Class.Size, data = hs_data_Cluster1)
summary(linear_cluster1)

linear_cluster2 <- lm(Average.SAT_Math~ X..Economically.Disadvantaged + Average.Salary + Average.Class.Size + Average.Salary * Average.Class.Size, data = hs_data_Cluster2)
summary(linear_cluster2)

linear_cluster3 <- lm(Average.SAT_Math~ X..Economically.Disadvantaged + Average.Salary + Average.Class.Size + Average.Salary * Average.Class.Size, data = hs_data_Cluster3)
summary(linear_cluster3)

linear_cluster4 <- lm(Average.SAT_Math~ X..Economically.Disadvantaged + Average.Salary + Average.Class.Size + Average.Salary * Average.Class.Size, data = hs_data_Cluster4)
summary(linear_cluster4)

library(Hmisc)
cor(hs_data_Cluster2)
pairs(X..Economically.Disadvantaged, Average.Salary)
rcorr(as.matrix(hs_data_Cluster2))

correlations_cluster2<-cor(x=hs_data_Cluster2, use = "pairwise")
corrplot(correlations_cluster2, method = "circle", order= "hclust")

#graph clusters 
p<- ggplot(transformed_imputed, aes(Average.Class.Size, Average.SAT_Math))
p + geom_point()
p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Class.Size))
p + geom_point()
p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, salary_to_size))
p + geom_point()
p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, TOTAL_Enrollment))
p + geom_point() + geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Total Enrollment vs. Average SAT Math Scores")
  
#p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, TOTAL_Enrollment))
p + geom_point()
p + geom_point(aes(color = factor(kmeans)))



p<- ggplot(transformed_imputed, aes(Average.SAT_Math, X..Economically.Disadvantaged))
p + geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "% of Economically Disadvantaged vs. Average SAT Math Scores")

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Salary))
p + geom_point() + 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Average.Salary vs. Average SAT Math Scores")

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Class.Size))
p + geom_point()+ 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Average.Class.Size vs. Average SAT Math Scores")

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Expenditures.per.Pupil))
p + geom_point()+ 
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Average.Expenditures.per.Pupil vs. Average SAT Math Scores")


p<- ggplot(transformed_imputed, aes(X..Economically.Disadvantaged, Average.Expenditures.per.Pupil))
p + geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  labs(title = "Average.Expenditures.per.Pupil vs. Economically Disadvantaged")


p<- ggplot(transformed_imputed, aes(Average.Class.Size, Average.SAT_Math))
p + geom_point()
p + geom_point(aes(color = factor(clusterGroups)))

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, X..Hispanic))
p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, X..Economically.Disadvantaged))
p + geom_point(aes(color = factor(kmeans))) +  
  labs(title = "% of Economically Disadvantaged vs. Average SAT Math Scores")

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Salary))
p + geom_point(aes(color = factor(kmeans))) +  
  labs(title = "Average Teacher Salary vs. Average SAT Math Scores")

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Class.Size))
p + geom_point(aes(color = factor(kmeans))) +  
  labs(title = "Average Class Size vs. Average SAT Math Scores")

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Expenditures.per.Pupil))
p + geom_point(aes(color = factor(kmeans))) +  
  labs(title = "Average Expenditure per Pupil vs. Average SAT Math Scores")

p<- ggplot(transformed_imputed, aes(Average.Expenditures.per.Pupil, X..Economically.Disadvantaged))
p + geom_point(aes(color = factor(kmeans))) +  labs(title = "% of Economically Disadvantaged vs. Average SAT Math Scores")

#p<- ggplot(transformed_imputed, aes(Average.SAT_Math, X..Economically.Disadvantaged))
#p + geom_point(aes(color = factor(clusterGroups)))

#p<- ggplot(transformed_imputed, aes(X..Hispanic, X..Economically.Disadvantaged))
#p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.Class.Size, X..Economically.Disadvantaged))
p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.Salary, X..Economically.Disadvantaged))
p + geom_point(aes(color = factor(kmeans))) 

p<- ggplot(transformed_imputed, aes(Average.Expenditures.per.Pupil, X..Economically.Disadvantaged))
p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.Salary, X..Economically.Disadvantaged))
p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Expenditures.per.Pupil))
p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.Salary, Average.Expenditures.per.Pupil))
p + geom_point(aes(color = factor(kmeans)))


p<- ggplot(transformed_imputed, aes(Average.SAT_Math, X..Males))
p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, X..Hispanic))
p + geom_point(aes(color = factor(kmeans)))

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Salary, color = kmeans))
p + geom_point(aes(color = factor(kmeans)))+ labs(title = "Average Teacher Salary vs. Average SAT Math Scores")+
  geom_smooth(aes(group=kmeans), method = "lm", se = FALSE) + 
  facet_wrap(~kmeans)

p<- ggplot(hs_data_ready, aes(Average.SAT_Math, Average.Salary)) 
p+geom_point() +
  geom_smooth(method = "lm", se = FALSE)

p<- ggplot(hs_data_ready, aes(Average.SAT_Math, Average.Class.Size)) 
p+geom_point(method = "lm")+
  geom_smooth(method = "lm", se = FALSE)


summary(hs_data_ready)
hist(hs_data_ready$Average.Class.Size)
hist(hs_data_ready$Average.SAT_Math)
hist(hs_data_ready$Average.Salary)

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Class.Size, color = kmeans))
p + geom_point(aes(color = factor(kmeans)))+ labs(title = "Average Class Size vs. Average SAT Math Scores")+
  geom_smooth(aes(group=kmeans), method = "lm", se = FALSE) +
  facet_wrap(~kmeans)

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Expenditures.per.Pupil))
p + geom_point(aes(color = factor(kmeans))) +  
  labs(title = "Average Expenditure per Pupil vs. Average SAT Math Scores")


p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Expenditures.per.Pupil, color = kmeans))
p + geom_point(aes(color = factor(kmeans)))+ labs(title = "Average Expenditure per Pupils vs. Average SAT Math Scores")+
  geom_smooth(aes(group=kmeans), method = "lm", se = FALSE) +
  facet_wrap(~kmeans)

p<- ggplot(transformed_imputed, aes(Average.SAT_Math, Average.Expenditures.per.Pupil, color = kmeans))
p + geom_point(aes(color = factor(kmeans)))+ labs(title = "Average SAT Math vs. Average Expenditure per Pupils")+
  geom_smooth(aes(group=kmeans), method = "lm", se = FALSE) +
  facet_wrap(~kmeans)

p<- ggplot(transformed_imputed, aes(X..Economically.Disadvantaged, Average.Expenditures.per.Pupil, color = kmeans))
p + geom_point(aes(color = factor(kmeans)))+ labs(title = "X..Economically.Disadvantaged vs. Average Expenditure per Pupils")+
  geom_smooth(aes(group=kmeans), method = "lm", se = FALSE) +
  facet_wrap(~kmeans)


p<- ggplot(transformed_imputed, aes(X..Economically.Disadvantaged, Average.Expenditures.per.Pupil, color = kmeans))
p + geom_point(aes(color = factor(kmeans)))+ labs(title = "Econ disadvtanged vs. Average Expenditure per Pupil")+
  geom_smooth(aes(group=kmeans), method = "lm", se = FALSE)

#clean facet wrap
p<- ggplot(transformed_imputed, aes(X..Economically.Disadvantaged, Average.Expenditures.per.Pupil, color = kmeans))
p + geom_point(aes(color = factor(kmeans)))+ labs(title = "Econ disadvtanged vs. Average Expenditure per Pupil")+
  geom_smooth(aes(group=kmeans), method = "lm", se = FALSE) +
  facet_wrap(~kmeans)

p<- ggplot(transformed_imputed, aes(X..High.Needs, Average.Expenditures.per.Pupil, color = kmeans))
p + geom_point(aes(color = factor(kmeans)))+ labs(title = "High Needs vs. Average Expenditure per Pupil")+
  geom_smooth(aes(group=kmeans), method = "lm", se = FALSE) +
  facet_wrap(~kmeans)

p<- ggplot(transformed_imputed, aes(X..Economically.Disadvantaged, Average.Salary, color = kmeans))
p + geom_point(aes(color = factor(kmeans)))+ labs(title = "Econ disadvantanged vs. Average Salary")+
  geom_smooth(aes(group=kmeans), method = "lm", se = FALSE) +
  facet_wrap(~kmeans)

p<- ggplot(transformed_imputed, aes(Average.Salary, Average.Class.Size, color = kmeans))
p + geom_point(aes(color = factor(kmeans)))+ labs(title = "Econ disadvtanged vs. Average Expenditure per Pupil")+
  geom_smooth(aes(group=kmeans), method = "lm", se = FALSE) +
  facet_wrap(~kmeans)

#linear regression
transformed_imputed$Salary_to_size = transformed_imputed$Average.Salary/transformed_imputed$Average.Class.Size
linear <- lm(Average.SAT_Math~ X..Economically.Disadvantaged + Average.Salary + Average.Class.Size + Average.Salary/Average.Class.Size , data = transformed_imputed)
print(linear)
summary(linear)

linear_others <- lm(Average.SAT_Math~ X..Economically.Disadvantaged + Average.Expenditures.per.Pupil + 
               TOTAL_Enrollment, data = transformed_imputed)
summary(linear_others)

linear_final <- lm(Average.SAT_Math~ X..Economically.Disadvantaged + Average.Expenditures.per.Pupil + 
                     Average.Class.Size + Average.Expenditures.per.Pupil * Average.Class.Size, data = transformed_imputed)
summary(linear_final)

#use rmarkdown to creat dashboard
#http://rmarkdown.rstudio.com/flexdashboard/

#find outliers with respect to economic disadvantage and good SAT scores
#find out what drives them -> teacher salary, class size
#notes: 
#Economic disadvantage seems to be the best variable for predicting SAT scores
#however teacher salary and class size does not change substantially with respect to economic
#disadvantage, neither does expenditure per student
#teacher salary, class size and expenditure per pupil seem to have a greater effect on 
#the cluster 4 
#graph the clusters vs. economic disadvantage in bar graph to get more insight on clusters

#adding a teacher salary per class size score
#transformed_imputed$salary_class_size <- transformed_imputed$Average.Salary/transformed_imputed$Average.Class.Size
p<- ggplot(transformed_imputed, aes(salary_class_size, Average.SAT_Math))
p + geom_point(aes(color = factor(kmeans)))

#dam this needs to be declared before normalizing
within(transformed_imputed, rm(salary_class_size))


