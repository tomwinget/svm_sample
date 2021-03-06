#Script loads p53 genome data and runs boruta feature selection to determine important attributes
#install.packages('boruta')
#install.packages('rpart')
#install.packages('e1071')

hyperparameter <- 75;

sink(gsub(' ', '', paste('output_boruta_', hyperparameter, '.txt')))
print(paste('Hyperparameter value is =', hyperparameter))
if(!exists("relabel", mode="function")) source("pca_util.R")
if(!exists("d")) d <-loadData();

library(Boruta);
library(e1071);
library(rpart);

#sampling <- 1:20; # sample first 20 points
sampling <- randomSample(percent = 0.1)

samples <- length(d[,5409]); # Number of samples (31240)
x <- 1:samples;
y <- 1:5409;
l <- sapply(d[,5409], relabel);

#active_locations <- x[d[,5409] == 'active'];

subd <- d[sampling, 1:5408]; # Grab a subset of the data
#write.table(subdata, file='test.csv')
l2 <- l[sampling];

# Remove constant / zero columns
nonzero.sub <- subd[, sapply(subd, function(v) var(v, na.rm=TRUE)!=0)]

# Remove samples with missing values
l2 <- l2[complete.cases(nonzero.sub)];
nonzero.sub <- nonzero.sub[complete.cases(nonzero.sub), ];

#Run boruta on subset of data
set.seed(123);
#boruta.train <- Boruta(V1~.-V5408, data = nonzero.sub, doTrace=2);
#print(boruta.train);

#Print significant attributes as found by boruta
boruta_signif <- names(boruta.train$finalDecision[boruta.train$finalDecision %in% c("Confirmed")])
print(boruta_signif)

boruta.points <- nonzero.sub[c(boruta_signif)]

# Do SVM with standard (nonBoruta) data
svm.results <- svm(nonzero.sub, y=l2); # pca.subdata
print(svm.results);

# Training error
training.results <- predict(svm.results, nonzero.sub); # pca.subdata
training.error <- sum(error(training.results, l2)) / length(l2);
print(paste('Percent training error for standard svm on normal data =', training.error));

# Do SVM with Boruta data
svm.boruta.results <- svm(boruta.points, y=l2); # pca.subdata
print(svm.boruta.results);

# Training error with Boruta
training.results.boruta <- predict(svm.boruta.results, boruta.points); # boruta.subdata
training.error.boruta <- sum(error(training.results.boruta, l2)) / length(l2);
print(paste('Percent training error for svm on Boruta data =', training.error.boruta))

#Print out the classification of the positive points
print('Training classification of positive points on normal data')
print(training.results[l2 > 0])
print('Training classification of positive points on Boruta data')
print(training.results.boruta[l2 > 0])

#Calculate sensitivity
true_pos = sum(l2 > 0)
pos = sum(training.results[l2 > 0] > 0)
print(paste('Sensitivity of normal svm =', (pos / true_pos)))
pos.boruta = sum(training.results.boruta[l2 > 0] > 0)
print(paste('Sensitivity of boruta svm =', (pos.boruta / true_pos)))


# Testing Data
# Format data
resampling <- randomSample(percent = 0.2)
testing <- x[resampling] # Grab the remainder of the data
subd.testing <- d[testing, 1:5408]; # Grab the data
l.testing <- l[testing]; # Grab the labels
nonzero.sub.testing <- subd.testing[, sapply(subd.testing, function(v) var(v, na.rm=TRUE)!=0)] # remove zero/constant columns
l.testing <- l.testing[complete.cases(nonzero.sub.testing)]; # remove samples not needed
nonzero.sub.testing <- nonzero.sub.testing[complete.cases(nonzero.sub.testing), ]; # remove samples not needed

# Testing data on normal data with svm
testing.results <- predict(svm.results, nonzero.sub.testing); # pca.subdata
testing.error <- sum(error(testing.results, l.testing)) / length(l.testing);
print(paste('Percent testing error for standard svm on normal data =', testing.error));

# Testing data on data with Boruta
boruta.points.testing <- nonzero.sub.testing[c(boruta_signif)]

# Do SVM with Boruta data
testing.results.boruta <- predict(svm.boruta.results, boruta.points.testing); # TODO: CHANGE TO svm.pca.results # svm.pca.testing
testing.error.boruta <- sum(error(testing.results.boruta, l.testing)) / length(l.testing);
print(paste('Percent testing error for svm on Boruta data =', testing.error.boruta))


#Calculate sensitivity
true_pos.test = sum(l.testing > 0)
pos.test = sum(testing.results[l.testing > 0] > 0)
print(paste('Testing sensitivity of normal svm =', (pos.test / true_pos.test)))
pos.test.boruta = sum(testing.results.boruta[l.testing > 0] > 0)
print(paste('Testing sensitivity of boruta svm =', (pos.test.boruta / true_pos.test)))

sink()