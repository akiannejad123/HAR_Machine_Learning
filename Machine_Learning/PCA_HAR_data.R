
library(caret)
library(kernlab)
library(tidyverse)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(AppliedPredictiveModeling)
library(Hmisc)
install.packages("rlist")
library(rlist)
trainingURL <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testingURL <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

weightLiftingTraining <-read.csv(trainingURL)

train_vars<-colnames(weightLiftingTraining)

dat<-weightLiftingTraining[,grep("^roll_", colnames(weightLiftingTraining))]
  
weightLiftingTraining<-weightLiftingTraining%>%
   select(
      grep("^roll_", colnames(weightLiftingTraining)),
      grep("^pitch_", colnames(weightLiftingTraining)),
      grep("^yaw_", colnames(weightLiftingTraining)),
      classe
   )%>%
   select(-roll_dumbbell, -pitch_arm, -roll_forearm, -yaw_arm, classe)
   
   
   
######################
 

 modelfit <-train(classe~., data=weightLiftingTraining ,method='glm')
 
 
 
 res.pca <-PCA(weightLiftingTraining[-9], scale.unit=TRUE, ncp=9, graph=FALSE)
 
 fviz_eig(res.pca)
 fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
 
 f <-res.pca[[1]][[2]]
 fviz_pca_var(res.pca, col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE # Avoid text overlapping
 ) 

 set.seed(123)
 my.cont.var <- rnorm(20)
 # Color variables by the continuous variable
 fviz_pca_var(res.pca, col.var = my.cont.var,
             # gradient.cols = c("green", "blue", "yellow", "red", "grey"),
              legend.title = "Cont.Var")



########################## new reduced vars based on >0.5 cos
reduced_var<-as.vector(new.res.pca$names)

weightLiftingTraining<-weightLiftingTraining%>%
   select(reduced_var, classe)
new.res.pca <-PCA(weightLiftingTraining[-11], scale.unit=TRUE, ncp=9, graph=FALSE)

fviz_eig(new.res.pca)
fviz_eig(new.res.pca, addlabels = TRUE, ylim = c(0, 50))

f <-res.pca[[1]][[2]]
 fviz_pca_var(res.pca, col.var = "cos2",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                   repel = TRUE # Avoid text overlapping
)     

 
 
###############
 var <- get_pca_var(res.pca)
 var 
 library("corrplot")
 corrplot(var$cos2, is.corr=FALSE)
 
 
 
##############################
 fviz_pca_ind(res.pca, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
               # Avoid text overlapping (slow if many points)
 )
#################
 
 fviz_cos2(res.pca, choice = "ind")
 
 # Total contribution on PC1 and PC2
 
 fviz_contrib(res.pca, choice = "ind", axes = 1:2)
 
 
 fviz_pca_ind(res.pca,
              geom.ind = "point", # show points only (nbut not "text")
              col.ind = weightLiftingTraining$classe, # color by groups
              palette = c("#00AFBB", "#E7B800", "#FC4E07", "grey", "yellow"),
              addEllipses = TRUE, # Concentration ellipses
              legend.title = "Groups"
 )
 
 
 res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
 # Description of dimension 1
 res.desc$Dim.1
 
 set.seed(123)
 res.km <- kmeans(var$coord, centers = 3, nstart = 25)
 grp <- as.factor(res.km$cluster)
 # Color variables by groups
 fviz_pca_var(res.pca, col.var = grp, 
              palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
              legend.title = "Cluster")
 
 weightLiftingTesting <-read.csv(testingURL)
 ######################
 weightLiftingTesting<-weightLiftingTesting%>%
   select(
   grep("^roll_", colnames(weightLiftingTraining)),
   grep("^pitch_", colnames(weightLiftingTraining)),
   grep("^yaw_", colnames(weightLiftingTraining))
 )
 adpredict <-predict((preproc), weightLiftingTesting$classe)
 mean(adpredict)
 
 ind.p <- fviz_pca_ind(res.pca, geom = "point", col.ind = weightLiftingTraining$classe)
 ggpubr::ggpar(ind.p,
               title = "Principal Component Analysis",
               subtitle = "Iris data set",
               caption = "Source: factoextra",
               xlab = "PC1", ylab = "PC2",
               legend.title = "Species", legend.position = "top",
               ggtheme = theme_gray(), palette = "jco"
 )
 
 fviz_pca_biplot(res.pca, repel = TRUE,
                 col.var = "#2E9FDF", # Variables color
                 col.ind = "#696969"  # Individuals color
 )
 
 ldamodel <-train(classe~., weightLiftingTraining, "lda", max.ov)
 nbmodel <-train(classe~., weightLiftingTraining, "nb")
 
 
 