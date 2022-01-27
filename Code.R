require(ggplot2)
library(ggfortify)
library(cluster)
library(fmsb)
library(GGally)
library(gridExtra)
library(randomForest)
library(gbm) 
library(MASS) 
library(klaR)
library(cowplot)


Food_Supply_Quantity_kg_Data <- read.csv("C:/Users/Asus/Desktop/Food_Supply_Quantity_kg_Data.csv")


df = Food_Supply_Quantity_kg_Data

#======================================================================================================#
##### Data Pre-processing

## Drop records with multiple missing values
df = df[-c(53,110,81,82,156),]


## Gather information on missing values from the web and input into the dataframe
df$Obesity[df$Country == 'Taiwan*'] = 22.8
df$Confirmed[df$Country == 'Myanmar'] = 250000/54704000
df$Deaths[df$Country == 'Myanmar'] = 9000/54704000

df$Undernourished[df$Undernourished == '<2.5'] = 1.25
df$Undernourished[df$Country == 'Antigua and Barbuda'] = 20.5
df$Undernourished[df$Country == 'Bahamas'] = 5.1
df$Undernourished[df$Country == 'Grenada'] = 25.5
df$Undernourished[df$Country == 'Republic of Moldova'] = 4
df$Undernourished[df$Country == 'Saint Kitts and Nevis'] = 10.2
df$Undernourished[df$Country == 'Saint Lucia'] = 27.3
df$Undernourished[df$Country == 'Tajikistan'] = 33.2

## Identify weak variables and drop them (average below 1 percent)
useless_variables = c()

for( i in seq(2, 23)){
  if(mean ( df[,i] ) <=1 ){
    useless_variables = c(useless_variables, i)
  }
}

df <- df[ -c(3, 5, 7, 12, 13, 14, 15, 16, 18, 20, 21, 22) ]

df$Undernourished = as.double(df$Undernourished)

df <- df[, -c(15, 17, 18, 19) ]

#======================================================================================================#

###### Data Description


## Histogram

hist_mr = ggplot(data=df, aes(Deaths)) + geom_histogram(color='black', fill='purple')


## Correlation Matrix

cor_df = cor(df[,-c(1)])


## PCA

pca=prcomp(df[c(2:12)], scale=TRUE)
autoplot(pca, data = df[c(2:12)], loadings = TRUE, loadings.label = TRUE)

#======================================================================================================#

#### Model Creation and Selection Part I: Clustering 

df_clustering = df[,2:12]

rownames(df_clustering)=df$Country

df_clustering_std = as.data.frame(scale(df_clustering))

## Finding Optimal Number of Clusters with Elbow Method

#Elbow Method for finding the optimal number of clusters
# Compute and plot wss for k = 2 to k = 8.



set.seed(123)

fviz_nbclust(df_clustering_std, kmeans, method = "wss", linecolor = 'purple')


### Optimal Number of Clusters: 3 or 4

km.4=kmeans(df_clustering_std, 4, nstart = 20) #4 clusters 


df_clustering_std$cluster=as.factor(km.4$cluster) 

#======================================================================================================#

#### Model Creation and Selection Part II: Prediction  

#Splitting Data set to Training and Test Set


## 70% of the sample size
smp_size <- floor(0.7 * nrow(df))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]


# Random Forest Regression with All Predictors, with Summary and Importance Plot

myforest=randomForest(Deaths~Alcoholic.Beverages+Animal.Products+Cereals...Excluding.Beer+Fish..Seafood+Fruits...Excluding.Wine+Meat+Milk...Excluding.Butter+Starchy.Roots+Sugar...Sweeteners+Vegetables+Vegetal.Products, ntree=1000, data=train, importance=TRUE, na.action = na.omit)
myforest
importance(myforest)
varImpPlot(myforest)



# Random Forest Regression without 5 Last Significant Predictors, with Summary and Importance Plot (put in the appendix)

myforest=randomForest(Deaths~Alcoholic.Beverages+Animal.Products+Cereals...Excluding.Beer+Fish..Seafood+Milk...Excluding.Butter+Vegetal.Products, ntree=1000, data=train, importance=TRUE, na.action = na.omit)
myforest
varImpPlot(myforest)


#Gradient Boosting Regression Model using All Predictors


gbmforest=gbm(Deaths~Alcoholic.Beverages+Animal.Products+Cereals...Excluding.Beer+Fish..Seafood+Fruits...Excluding.Wine+Meat+Milk...Excluding.Butter+Starchy.Roots+Sugar...Sweeteners+Vegetables+Vegetal.Products, data=train, distribution="gaussian", n.trees=10000, interaction.depth=4)
summary(gbmforest)

# Testing Gradient Boosting Regression Model using All Predictors with Test Set

predicted_score=predict(gbmforest, newdata=test)
mean((predicted_score - test$Deaths)^2) 

# Gradient Boosting Regression Model without 5 Least Significant Predictors (put in the appendix)

gbmforest=gbm(Deaths~Alcoholic.Beverages+Animal.Products+Cereals...Excluding.Beer+Fish..Seafood+Milk...Excluding.Butter+Vegetal.Products, data=train, distribution="gaussian")
summary(gbmforest)

# Testing Gradient Boosting Regression Model without 5 Least Significant Predictors with Test Set

predicted_score=predict(gbmforest, newdata=test)
mean((predicted_score - test$Deaths)^2) 



#======================================================================================================#

## Create a Radar (Spyder) Diagram


# Create Dataframes to draw the Radar Chart for Clusters 

max_min <- data.frame(
  Alcoholic.Beverages = c( max(df_clustering_std$Alcoholic.Beverages), min(df_clustering_std$Alcoholic.Beverages)),
  Animal.Products = c( max(df_clustering_std$Animal.Products), min(df_clustering_std$Animal.Products)),
  Cereals = c( max(df_clustering_std$Cereals...Excluding.Beer), min(df_clustering_std$Cereals...Excluding.Beer)),
  Seafood = c( max(df_clustering_std$Fish..Seafood), min(df_clustering_std$Fish..Seafood)),
  Fruits = c( max(df_clustering_std$Fruits...Excluding.Wine), min(df_clustering_std$Fruits...Excluding.Wine)),
  Meat = c( max(df_clustering_std$Meat), min(df_clustering_std$Meat)),
  Milk = c( max(df_clustering_std$Milk...Excluding.Butter), min(df_clustering_std$Milk...Excluding.Butter)),
  Starchy.Roots = c( max(df_clustering_std$Starchy.Roots), min(df_clustering_std$Starchy.Roots)),
  Sugar = c( max(df_clustering_std$Sugar...Sweeteners), min(df_clustering_std$Sugar...Sweeteners)),
  Vegetables = c( max(df_clustering_std$Vegetables), min(df_clustering_std$Vegetables)),
  Vegetal.Products = c( max(df_clustering_std$Vegetal.Products), min(df_clustering_std$Vegetal.Products))
)

rownames(max_min) <- c("Max", "Min")

clusters <- data.frame(
  Alcoholic.Beverages = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Alcoholic.Beverages"]), 
                          mean(df_clustering_std[df_clustering_std$cluster==2,"Alcoholic.Beverages"]),
                          mean(df_clustering_std[df_clustering_std$cluster==3,"Alcoholic.Beverages"]),
                          mean(df_clustering_std[df_clustering_std$cluster==4,"Alcoholic.Beverages"])
                          ),
  Animal.Products = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Animal.Products"]), 
                          mean(df_clustering_std[df_clustering_std$cluster==2,"Animal.Products"]),
                          mean(df_clustering_std[df_clustering_std$cluster==3,"Animal.Products"]),
                          mean(df_clustering_std[df_clustering_std$cluster==4,"Animal.Products"])
  ),
  Cereals = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Cereals...Excluding.Beer"]), 
                          mean(df_clustering_std[df_clustering_std$cluster==2,"Cereals...Excluding.Beer"]),
                          mean(df_clustering_std[df_clustering_std$cluster==3,"Cereals...Excluding.Beer"]),
                          mean(df_clustering_std[df_clustering_std$cluster==4,"Cereals...Excluding.Beer"])
  ),
  Seafood = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Fish..Seafood"]), 
                          mean(df_clustering_std[df_clustering_std$cluster==2,"Fish..Seafood"]),
                          mean(df_clustering_std[df_clustering_std$cluster==3,"Fish..Seafood"]),
                          mean(df_clustering_std[df_clustering_std$cluster==4,"Fish..Seafood"])
  ),
  Fruits = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Fruits...Excluding.Wine"]), 
                          mean(df_clustering_std[df_clustering_std$cluster==2,"Fruits...Excluding.Wine"]),
                          mean(df_clustering_std[df_clustering_std$cluster==3,"Fruits...Excluding.Wine"]),
                          mean(df_clustering_std[df_clustering_std$cluster==4,"Fruits...Excluding.Wine"])
  ),
  Meat = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Meat"]), 
                          mean(df_clustering_std[df_clustering_std$cluster==2,"Meat"]),
                          mean(df_clustering_std[df_clustering_std$cluster==3,"Meat"]),
                          mean(df_clustering_std[df_clustering_std$cluster==4,"Meat"])
  ),
  Milk = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Milk...Excluding.Butter"]), 
                          mean(df_clustering_std[df_clustering_std$cluster==2,"Milk...Excluding.Butter"]),
                          mean(df_clustering_std[df_clustering_std$cluster==3,"Milk...Excluding.Butter"]),
                          mean(df_clustering_std[df_clustering_std$cluster==4,"Milk...Excluding.Butter"])
  ),
  Starchy.Roots = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Starchy.Roots"]), 
                          mean(df_clustering_std[df_clustering_std$cluster==2,"Starchy.Roots"]),
                          mean(df_clustering_std[df_clustering_std$cluster==3,"Starchy.Roots"]),
                          mean(df_clustering_std[df_clustering_std$cluster==4,"Starchy.Roots"])
  ),
  Sugar = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Sugar...Sweeteners"]), 
                    mean(df_clustering_std[df_clustering_std$cluster==2,"Sugar...Sweeteners"]),
                    mean(df_clustering_std[df_clustering_std$cluster==3,"Sugar...Sweeteners"]),
                    mean(df_clustering_std[df_clustering_std$cluster==4,"Sugar...Sweeteners"])
  ),
  Vegetables = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Vegetables"]), 
                    mean(df_clustering_std[df_clustering_std$cluster==2,"Vegetables"]),
                    mean(df_clustering_std[df_clustering_std$cluster==3,"Vegetables"]),
                    mean(df_clustering_std[df_clustering_std$cluster==4,"Vegetables"])
  ),
  Vegetal.Products = c(mean(df_clustering_std[df_clustering_std$cluster==1,"Vegetal.Products"]), 
                 mean(df_clustering_std[df_clustering_std$cluster==2,"Vegetal.Products"]),
                 mean(df_clustering_std[df_clustering_std$cluster==3,"Vegetal.Products"]),
                 mean(df_clustering_std[df_clustering_std$cluster==4,"Vegetal.Products"])
  )
)

rownames(clusters) <- c("Adequately-Balanced", "Animal Consumers","Animal Lovers","Vegetable Enjoyers")


spyder_df = rbind(max_min, clusters)


## Draw the Radar  Chart
#install.packages('fmsb')

spyder_df <- spyder_df[c("Max", "Min", "Adequately-Balanced", "Animal Consumers","Animal Lovers","Vegetable Enjoyers"), ]


## Create Beautiful Radar Chart

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 0,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))
par(mfrow=c(1,1))
# Create the radar charts
create_beautiful_radarchart(
  data = spyder_df,
  color = c("#800080", "#EE82EE", "#4B0082","#FF00FF")
)
# Add an horizontal legend
legend(
  x = "topright", legend = rownames(spyder_df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c("#800080", "#EE82EE", "#4B0082","#FF00FF"),
  text.col = "black", cex = 0.8, pt.cex = 1.5
)
par(op)



# Define colors and titles
colors <- c("#800080", "#EE82EE", "#4B0082","#FF00FF")
titles <- rownames(spyder_df[-c(1,2),])

# Reduce plot margin using par()
# Split the screen in 3 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))

# Create the radar chart
for(i in 1:4){
  create_beautiful_radarchart(
    data = spyder_df[c(1, 2, i+2), ],
    color = colors[i], title = titles[i]
  )
}
par(op)


#======================================================================================================#

#### Get mean of Obesity and Undernourished for different clusters

df$cluster = as.factor(km.4$cluster) 
Obesity_mean_cluster_1 = mean(df[df$cluster == 1,"Obesity"])
Undernourished_mean_cluster_1 = mean(df[df$cluster == 1,"Undernourished"])

Obesity_mean_cluster_2 = mean(df[df$cluster == 2,"Obesity"])
Undernourished_mean_cluster_2 = mean(df[df$cluster == 2,"Undernourished"])

Obesity_mean_cluster_3 = mean(df[df$cluster == 3,"Obesity"])
Undernourished_mean_cluster_3 = mean(df[df$cluster == 3,"Undernourished"])

Obesity_mean_cluster_4 = mean(df[df$cluster == 4,"Obesity"])
Undernourished_mean_cluster_4 = mean(df[df$cluster == 4,"Undernourished"])




