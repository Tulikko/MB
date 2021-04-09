
## Step 1: Tell R where you want any output (like graphs) saved. The command is called "set working drive." 
## What you have to do is replace #$# with the file path to the folder of your choice. For example:
## setwd("~/Desktop/Marta")

setwd("~/Desktop/R/Graph")

## Step 2: Open these packages which are already installed on your computer. Just highlight the two lines
## of code and hit Apple + Enter

library(devtools)
library("factoextra")

## Step 3: Import the spreadsheet with your data (R calls spreadsheets 'dataframes') 
## Replace #$# with the filepath where the data is saved **in .csv format** For example:
## data <- read.csv("~/Desktop/Marta/Rtest reduced.csv")

data <- read.csv("~/Desktop/R/Gourniaxrf.csv")

## Step 4: Reduce the dataframe to the columns you want to work with. This will make it easier in the future
## and you can always go back and add columns by repeating step 3 and 4 again. 
## Replace #$# with the appropriate column numbers: c(1,2) means 'select column 1 and 2', c(1:9), means
## 'select columns 1 to 9', c(1:3, 5) means 'select columns 1,2,3,5. For example:
## data.wanted <- data[,c(2:5,12:14,16,19)]

data.wanted <- data[,c(1,4,8:10,12,14,16,30,32,34,46)]

## Step 5: remove cells with missing data in your new dataframe by running the following line. 

summary(data.wanted)
data.wanted <- na.omit(data.wanted)
head(data.wantedneo)

## Step 6: to see if any rows of data were removed in the previous step, compare the outputs of these 
## two following lines of script. The argument length tells you how many rows there are in a given dataframe.


length(data$Site)
length(data.wanted$Site)


## Step 7: run the pricipal component analysis by creating an object, called 'outcome', where the results of
## the analysis will be stored. Replace #$# with the column numbers that identify all the **numerical columns**
## that you want to include in the analysis. For example:
## outcome <- prcomp(data.wanted[c(, c(2:9)], cor=FALSE, scores=TRUE). In this example, we are not including
## column 1, because it is not numberical. It contains the names of the sites, which will be used later as 
## a grouping variable.

#percentage around 0
outcome <- princomp(data.wanted[, c(6:12)], cor=FALSE, scores=TRUE)

#right percentage scale=TRUE is automatically scaling
outcome1 <- prcomp(data.wanted[, c(6:12)], center=TRUE, scale=TRUE)

#right percentage scale=FALSE is not automatically scaling-giving you around 99 % coverage
outcome1 <- prcomp(data.wanted[, c(6:12)], center=TRUE, scale=FALSE)

## Step 8: to look at the results of the analysis, run the following line:

summary(outcome1)

#IMPORTANT try with just numbers samples

fviz_pca_ind(outcome)

# Change title and axis labels
fviz_pca_ind(outcome) + labs(title ="PCA", x = "PC1", y = "PC2")

# Change axis limits by specifying the min and max
fviz_pca_ind(outcome) + xlim(-4, 4) + ylim (-4, 4)

# Use points only
fviz_pca_ind(outcome, geom="point")

# Change the size of points
fviz_pca_ind(outcome, geom="point", pointsize = 4)

# Change point color and theme
fviz_pca_ind(outcome, col.ind = "blue")+
  theme_minimal()

# GRAPH Color individuals by groups (first choose which variable in the quali.sup function)
quali.sup <- as.factor(data.wanted[, 4])
fviz_pca_ind(outcome, label="none", habillage=quali.sup)

# Add ellipses
p <- fviz_pca_ind(outcome, label="none", habillage=quali.sup,
                  addEllipses=TRUE, ellipse.level=0.95)
print(p)


#end try

## Step 9: create a biplot. Replace #$# with the column number of the column that contains the 'grouping
## variable (such as site, or time period). For example:
## quali.sup <- as.factor(data.wanted[, 1]). Change nothing else.

quali.sup <- as.factor(data.wanted[, 4])
head(quali.sup)
fviz_pca_ind(outcome1, habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.68, label="none") +
  theme_minimal()

## Step 10: save your graph in the directory you selected in Step 1. Replace #$# with the desired name. It can 
## contain spaces. For example:
## dev.copy2pdf(file="Main.pdf", encoding="WinAnsi")

dev.copy2pdf(file="GourniaContextnew1.pdf", encoding="WinAnsi")
