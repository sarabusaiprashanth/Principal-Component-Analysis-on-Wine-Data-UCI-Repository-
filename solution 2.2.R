wine_data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"))
wine_data
summary(wine_data)
head(wine_data)


#Column names of the winedataset
#Alcohol - 1
#Malic acid - 2
#Ash - 3
#Alcalinity of ash - 4  
#Magnesium - 5
#Total phenols - 6
#Flavanoids - 7
#Nonflavanoid phenols - 8
#Proanthocyanins - 9
#Color intensity - 10
#Hue - 11
#OD280/OD315 of diluted wines - 12
#Proline - 13

colnames(wine_data)
names(wine_data)[1] <- paste("Type")
names(wine_data)[2] <- paste("Alcohol")
names(wine_data)[3] <- paste("Malic_acid")
names(wine_data)[4] <- paste("Ash")
names(wine_data)[5] <- paste("Alcalinity_of_ash")
names(wine_data)[6] <- paste("Magnesium")
names(wine_data)[7] <- paste("Total_phenols")
names(wine_data)[8] <- paste("Flavanoids")
names(wine_data)[9] <- paste("Non_Flavonoids_phenols")
names(wine_data)[10] <- paste("Proanthocyanins")
names(wine_data)[11] <- paste("color_intensity")
names(wine_data)[12] <- paste("Hue")
names(wine_data)[13] <- paste("OD280/OD315_of_diluted_wines")
names(wine_data)[14] <- paste("Proline")
wine_data
colnames(wine_data)
attach(wine_data) 
summary(wine_data)
head(wine_data)

#install.packages("factoextra")
library(factoextra)
#library(car)
wine_data <- wine_data[,-1]
wine_data
colnames(wine_data)
wine_PCA <- princomp(wine_data, cor = TRUE, scores = TRUE, covmat = NULL)

summary(wine_PCA, cor = TRUE)

plot(wine_PCA)
biplot(wine_PCA) #The feature opposite to the "hue" is "malic acid"


#Scaling is not required because all the numbers are in numerical
#format only.
#Scaling puts the true variables on axis. 

#attributes(model) 

#install.packages("devtools")
#library(devtools)
#install.packages("BiplotGUI")
#library(BiplotGUI)

#The PCA calculates a new projection of your data set. And the new axis are based on the standard deviation of your variables. So a variable with a high standard deviation will have a higher weight for the calculation of axis than a variable with a low standard deviation. If you normalize your data, all variables have the same standard deviation, thus all variables have the same weight and your PCA calculates relevant axis

#Visualization and interpretation

#install.packages(c("FactoMineR", "factoextra"))


PCA(wine_data, scale.unit = TRUE, ncp = 5, graph = TRUE)

library(FactoMineR)
library(factoextra)
res.pca <- PCA(wine_data, graph = FALSE)
print(res.pca)
get_eigenvalue(res.pca)
fviz_eig(res.pca)
get_pca_ind(res.pca)
get_pca_var(res.pca)
fviz_pca_ind(res.pca)
fviz_pca_var(res.pca)
fviz_pca_biplot(res.pca)


eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
var

get_pca_var(res.pca)
var$coord #Co-ordinates
var$cos2  #Quality of the factor map
var$contrib #Contributions to the principal components

#Co-ordinates of the variables
head(var$coord, 4)

#To Plot Variables

fviz_pca_var(res.pca, col.var = "black")

#Quality of representation
head(var$cos2, 4)


library("corrplot")

corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2) 

# Color by cos2 values: quality on the factor map

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

head(var$contrib, 4)
corrplot(var$contrib, is.corr=FALSE)


#Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)



#Dimension Description

res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1


res.desc$Dim.1 #Correlation of dimension 1
res.desc$Dim.2 #Correlation of dimension 2

#Graph of individual contribution

ind <- get_pca_ind(res.pca)
ind

#Coordinates of individuals
head(ind$coord)
#Quality of individuals
head(ind$cos2)
#Contributions of individuals
head(ind$contrib)
fviz_pca_ind(res.pca)

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_cos2(res.pca, choice = "ind")

fviz_contrib(res.pca, choice = "ind", axes = 1:2)

cor(Hue, Malic_acid) #Negative Correlation


