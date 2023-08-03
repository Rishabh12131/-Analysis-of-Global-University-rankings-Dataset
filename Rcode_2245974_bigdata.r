install.packages("readr")
install.packages("party")
library(partykit)
library(party)

library(readr)
rankings_2022 <- read_csv("//ikb/home/74/2245974/Downloads/rankings_2022.csv")
View(rankings_2022)

library(readr)
rankings_2023 <- read_csv("//ikb/home/74/2245974/Downloads/rankings_2023.csv")
View(rankings_2023)

combined_dataset <- rbind(rankings_2022,rankings_2023)
View(combined_dataset)

combined_numerical <- combined_dataset


combined_numerical$rank_order<- NULL
combined_numerical$name<- NULL
combined_numerical$rank<-NULL
combined_numerical$location<- NULL
combined_numerical$aliases<- NULL
combined_numerical$subjects_offered<-NULL
combined_numerical$closed <- NULL
combined_numerical$unaccredited<- NULL
combined_numerical$stats_pc_intl_students<-NULL
combined_numerical$stats_female_male_ratio<-NULL
combined_numerical$scores_overall<-NULL

View(combined_numerical)
str(combined_numerical)

### PCA Eligibility and functions

cor(combined_numerical)
mean(cor(combined_numerical))
PCA <- prcomp(combined_numerical, center = TRUE, scale. = TRUE)
PCA2<- princomp(combined_numerical)

##PCA Loadings
PCA$rotation

### calculate the proportion of exaplained variance (PEV) from the std values
PCA_Var <- PCA$sdev^2
PCA_PEV <- PCA_Var/ sum(PCA_Var)

### plot the cumulative PEV
opar <- par()
plot(
  cumsum(PCA_PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'cumulative PEV',
  pch = 20,
  col = 'green'
)

###### Biplot
opar = par()
biplot(

  scale = 0,
  col = c('grey40','orange')
)
par(opar)

PC<- PCA2$scores
View(PC)

screeplot(PCA)





###Prediction using a decision tree using R

### split the cleaned data frame in training/test set (70% / 30%)
set.seed(1234)
n_rows <- nrow(combined_numerical)
training_idx <- sample(n_rows, n_rows * 0.7)
training_rankings_2022_2023 <- combined_dataset[training_idx,]
test_rankings_2022_2023 <- combined_dataset[-training_idx,]

summary(combined_dataset)
str(combined_numerical)

mean(combined_dataset$scores_teaching)

is.factor(combined_numerical$C2tegory)
combined_numerical$C2tegory <- as.factor(combined_numerical$C2tegory)

is.factor(combined_numerical$scores_teaching)
combined_numerical$scores_teaching<- as.factor(combined_numerical$scores_teaching)


is.factor(combined_numerical$scores_research)
combined_numerical$scores_research<- as.factor(combined_numerical$scores_research)

Model <-C2tegory ~ scores_teaching + scores_research + scores_citations + scores_industry_income + scores_international_outlook

rankings_ctree<- ctree(Model, data = training_rankings_2022_2023)
table(predict(rankings_ctree,training_rankings_2022_2023))
plot(rankings_ctree)






#Clustering analysis


install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(rpart)

View(combined_numerical)  ### Because we need unsupervised data.

wssplot <- function(data, nc=15, seed = 1334)
{
  wss <- (nrow(data)-1)*sum(apply(data,2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers = i)$withinss)
  }
  plot(1:nc,wss,type="b", xlab= "Number of clusters",ylab = "Within groups sum of squares")
}


wssplot(combined_numerical)

###K-Means cluster analysis

KM <- kmeans(combined_numerical,3)
KM2<- kmeans(combined_numerical,4)

###evaluate the cluster analysis

autoplot(KM,combined_numerical,frame=TRUE)

autoplot(KM2,combined_numerical,frame=TRUE)
###Cluster Centers

KM$centers
KM2$centers

