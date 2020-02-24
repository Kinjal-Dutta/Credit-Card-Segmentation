# Credit Card Segmentation

rm(list = ls())
setwd("C:/Users/KINJAL/Desktop/EdWisor/Project 3")
# #loading Libraries
x = c("Hmisc" , "mice" , "factoextra" , "devtools" , "ggthemes" , "plotly" , "htmlwidgets" , "GGally" , "tidyr" , "ggplot2", "corrgram", "DMwR", "usdm", "caret", "randomForest", "e1071",
      "DataCombine", "doSNOW", "inTrees", "rpart.plot", "rpart",'MASS','xgboost','stats',"dplyr")
#load Packages
lapply(x, require, character.only = TRUE)
rm(x)



#CUST_ID: Credit card holder ID
#BALANCE: Monthly average balance (based on daily balance averages)
#BALANCE_FREQUENCY: Ratio of last 12 months with balance
#PURCHASES: Total purchase amount spent during last 12 months
#ONEOFF_PURCHASES: Total amount of one-off purchases
#INSTALLMENTS_PURCHASES: Total amount of installment purchases
#CASH_ADVANCE: Total cash-advance amount
#PURCHASES_ FREQUENCY: Frequency of purchases (Percent of months with at least one purchase)
#ONEOFF_PURCHASES_FREQUENCY: Frequency of one-off-purchases
#PURCHASES_INSTALLMENTS_FREQUENCY: Frequency of installment purchases
#CASH_ADVANCE_ FREQUENCY: Cash-Advance frequency
#AVERAGE_PURCHASE_TRX: Average amount per purchase transaction
#CASH_ADVANCE_TRX: Average amount per cash-advance transaction
#PURCHASES_TRX: Average amount per purchase transaction
#CREDIT_LIMIT: Credit limit
#PAYMENTS: Total payments (due amount paid by the customer to decrease their statement balance) in the period
#MINIMUM_PAYMENTS: Total minimum payments due in the period.
#PRC_FULL_PAYMEN: Percentage of months with full payment of the due statement balance
#TENURE: Number of months as a customer





# loading datasets
ccs = read.csv("credit-card-data.csv", header = T, na.strings = c(" ", "", "NA"))


================================#Exploratory Data Analysis#===============================
# Structure of data
str(ccs)
summary(ccs)
head(ccs)
tail(ccs)
names(ccs)




================================#Missing Value Analysis#=================================
# We need to check the Missing values and if there are any we have to subsequently drop the missing values for better results

missing_val = data.frame(apply(ccs_new,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(ccs_new)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
missing_val



complete.cases(ccs)

which(complete.cases(ccs))
which(!complete.cases(ccs))

ccs_na = which(!complete.cases(ccs))
ccs_new = ccs[-ccs_na,]
ccs_new


================================#Feature Selection#=======================================

#We will take out the CUST_ID since it was a unique variable and we can't get further information from it.
#The scale on most of the variables is different, but before scale it, we will take a closer look at the data, we will deal with the scaling at PCA Session.

ccs_new <- ccs_new %>% 
  select(-CUST_ID) %>% 
  drop_na()

head(ccs_new,10)

================================#Data Exploration#========================================

#Corelation between variables

ggcorr(ccs_new, 
       label = T, 
       label_size = 3,
       label_round = 2,
       hjust = 1,
       size = 3, 
       color = "royalblue",
       layout.exp = 5,
       low = "dodgerblue", 
       mid = "gray95", 
       high = "red2",
       name = "Correlation")

===============================#Data Distribution#========================================

#We have to see whether there is some interesting finding on the data distribution, especially on Balance, Purchase, Credit Limit, and Tenure.

#Customer Balance

plot1 =  ggplot(ccs_new, aes(x=BALANCE)) +
  geom_histogram(col = "cyan", fill = "dodgerblue", bins = 30) +
  labs(x = "Balance", y = "Frequency", title = "Histogram of Customer Balance") +
  theme_igray()
ggplotly(plot1)

#Customer Purchase

plot2 = ggplot(ccs_new, aes(x=PURCHASES)) +
  geom_histogram(col = "lawngreen", fill = "springgreen4", bins = 40) +
  labs(x = "Purchase", y = "Frequency", title = "Histogram of Customer Purchase") +
  theme_igray()
ggplotly(plot2)


#Credit Limit

plot3 = ggplot(ccs_new, aes(x=CREDIT_LIMIT)) +
  geom_histogram(col = "yellow2", fill = "orangered2", bins = 30) +
  labs(x = "Credit Limit", y = "Frequency", title = "Histogram of Credit Limit") +
  theme_igray()
ggplotly(plot3)


#Tenure

plot4 =  ggplot(ccs_new, aes(x=TENURE)) +
  geom_bar(col = "magenta", fill = "maroon3") +
  labs(x = "Tenure", y = "Frequency", title = "Bar Chart of Tenure") +
  theme_igray()
ggplotly(plot4)


=====================================#Clustering#========================================

#Principal Component Analysis

#We need to conduct PCA to reduce the dimensionality but maintain information as much as possible.

scaled.ccs = scale(ccs_new) #Scaling the data
ccs_pca <- prcomp(scaled.ccs)


#PC Intrepretation

ccs_pca

#Variance Interpretation

summary(ccs_pca)

=================================#Individual Data Plot Interpretation#===================

#Interpretation of BiPlot PC1 and PC2
fviz_pca_biplot(ccs_pca, 
                axes = c(1:2), 
                col.var = "orange",
                col.ind = "royalblue",
                labelsize = 3) +
  theme_igray() +
  labs(title = "Biplot of PC1 and PC2")

========================#Individual Data Plot Interpretation - Outlier#===================

#Checking the Outliers
fviz_pca_biplot(ccs_pca, 
                axes = c(1:2), 
                col.var = "orange",
                col.ind = "red",
                labelsize = 3,
                select.ind = list(contrib = 5)) +
  theme_igray() +
  labs(title = "Outlier of PC1 and PC2")
# 5 outliers in the plot

ccs_new[c("502","551","1257","1605","2160"), ]


====================================#Variable Data Plot#==================================

fviz_pca_var(ccs_pca, col.var="orange") +
  theme_igray() +
  labs(title = "Variables Factor Map - PC 1 & PC2")


======================================#Dimensionality Reduction#==========================

#Based on interepertations from above, we will decide to take only 10 dimensions and put it to new dataset called ccs_new1
ccs_new1 <- ccs_pca$x[,1:10]

=======================================#K-Means Clustering#=================================


#After defining which dimensions that are going to used in Clustering, now we will use K-Means to determine how many Clusters do we need to divide Customers, which may represents their profile and hopefully we can determine what kind of tretment should be given to them.

#Elbow method

fviz_nbclust(ccs_new1, 
             kmeans, 
             method = "wss",
             linecolor = "green4") +
  geom_vline(xintercept = c(4,7), linetype = 2, col = "red") +
  theme_igray()

#K-Means

#We will perform the loop to find out the Within sum of Squares(WSS) and Between sum of squares clustering(BSS) clustering

for(i in 4:7){
  set.seed(289)
  model <- kmeans(ccs_new1, i)
  print(paste("WSS of K",i, "=",model$tot.withinss))
  print(paste("BSS Proportion of K",i, "=", model$betweenss/model$totss))
  print(paste("Cluster Size of K",i, "="))
  print(paste(model$size))
  print(fviz_cluster(model, ccs, palette = "Set1") +
          theme_igray())
}

#WSS of K 4 = 90683.9635948779
#BSS Proportion of K 4 = 0.328354376549163
#Cluster Size of K 4 =
#"3546" "682"  "3871" "537"

#WSS of K 5 = 83275.4959511983
#BSS Proportion of K 5 = 0.383224771182373
#Cluster Size of K 5 =
#"3352" "669"  "3641" "943"  "31"

#WSS of K 6 = 71417.3149301612
#BSS Proportion of K 6 = 0.47105171509991
#Cluster Size of K 6 =
#"2765" "616"  "3472" "734"  "30"   "1019"

#WSS of K 7 = 66984.174207859
#BSS Proportion of K 7 = 0.503885519956275
#Cluster Size of K 7 =
#"2219" "599"  "3180" "579"  "30"   "966"  "1063"


#we will use K = 6 as the number of Clusters, since we don't want too many Clusters and focusing our treatment to the Customers
ccs_km <- kmeans(ccs_new1, 6)


ccs_new$CLUSTER <- ccs_km$cluster
head(ccs_new, 10)








