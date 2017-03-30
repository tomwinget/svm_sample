#Script loads p53 genome data and runs boruta feature selection to determine important attributes
library(Boruta);
setwd("~/git/cse5522/");
source("p53_problem.R");

data <- read.csv('Data Sets/K9.data', header = F, stringsAsFactors = F, na.strings = "?");

print('Number of inactive:');
print(length(data[data[,5409] == 'inactive', 5409]));
print('Number of active:');
print(length(data[data[,5409] == 'active', 5409]));

names(data) <- gsub("?", "", names(data));

data[data == ""] <- NA;

data <- data[colSums(!is.na(data)) >0]
data <- data[complete.cases(data),]

convert <- c(2:6, 11:13);
data[,convert] <- data.frame(apply(data[convert],2,as.factor));

set.seed(123);
boruta.train <- Boruta(V1~.-V5409, data = data[1:100,], doTrace=2);
print(boruta.train);