################### Regression Project #############

#Library's
# install.packages("ggplot2")
# install.packages("caret")
# install.packages("data.table")
# install.packages("corrplot")
# install.packages("ROSE")
# install.packages("randomForest")
library('caret')
library('data.table')
library('corrplot')
library('ggplot2')
library('randomForest')


#Data Dictionary
# Semana — Week number (From Thursday to Wednesday)
# Agencia_ID — Sales Depot ID
# Canal_ID — Sales Channel ID
# Ruta_SAK — Route ID (Several routes = Sales Depot)
# Cliente_ID — Client ID
# NombreCliente — Client name
# Producto_ID — Product ID
# NombreProducto — Product Name
# Venta_uni_hoy — Sales unit this week (integer)
# Venta_hoy — Sales this week (unit: pesos)
# Dev_uni_proxima — Returns unit next week (integer)
# Dev_proxima — Returns next week (unit: pesos)
# Demanda_uni_equil — Adjusted Demand (integer) (This is the target you will predict)

#Loading Datasets
set.seed(283)
train <- fread('grupo-bimbo-inventory-demand/train.csv')
test <- fread('grupo-bimbo-inventory-demand/test.csv')
sample <- fread('grupo-bimbo-inventory-demand/sample_submission.csv')

#Data Exploration
View(sample)

str(train)
str(test)
### Have a diference between train and test dataset

#Removing Venta_uni_hoy, Dev_uni_proxima, Dev_proxima, Venta_hoy, CLient_ID & ID from train and test
traindf <- train
testdf <- test
traindf$Venta_hoy = NULL
traindf$Venta_uni_hoy = NULL
traindf$Dev_proxima = NULL
traindf$Dev_uni_proxima = NULL
traindf$Cliente_ID = NULL
testdf$id = NULL
testdf$Cliente_ID = NULL

#Slicing datasets
indextrain <- sample(nrow(traindf), size=0.001 * nrow(traindf))

df_train = traindf[indextrain,]
str(df_train)

indextest = sample(nrow(testdf), size = 0.007 * nrow(testdf))

df_test <- testdf[indextest,]
str(df_test)

#Na's & Null's
table(is.na(df_train))
table(is.null(df_train))

#Data Distribution
summary(df_train)

################## Scale #######################

#Function for  Normalization
normfunc <- function(dataset, features){
  for(feature in features){
    dataset[[feature]] <- scale(dataset[[feature]], center = T, scale = T)
  }
  return(dataset)
}
#Features
features <- c('Ruta_SAK', 'Producto_ID', 'Agencia_ID', 'Canal_ID')

#Scale datasets
str(df_train)
str(df_test)

df_train = normfunc(df_train, features)

str(df_train)

df_test = normfunc(df_test, features)

str(df_test)

#Correlation
corrplot(cor(df_train))

# Feature Selection
modelfeature = randomForest(Demanda_uni_equil ~., data = df_train)

varImpPlot(modelfeature)

##################### Machine Learning Model #####################3

modelv1 <- randomForest(Demanda_uni_equil ~., data = df_train, ntree = 750, nodesize = 250)

modelv1

#New data 

predict <- predict(modelv1, df_test)
str(predict)

predict <- round(predict)
ID = sample(48994)
ID = sort(ID)

#Submission

sub = data.frame('ID'= ID,
                 'Predict' = predict)
View(sub)


#The prediction have some outliers