# install.packages('e1071')
# install.packages('rpart')


hyperparameter  <- 20;
sink(gsub(' ', '', paste('output_', hyperparameter, '.txt')))
print(paste('Hyperparameter value is =', hyperparameter))
if(!exists("relabel", mode="function")) source("pca_util.R")
if(!exists("d")) d <-loadData();

library(e1071)
library(rpart)

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

# Do SVM with standard (nonPCA) data
svm.results <- svm(nonzero.sub, y=l2); # pca.subdata
print(svm.results);

# Training error
training.results <- predict(svm.results, nonzero.sub); # pca.subdata
training.error <- sum(error(training.results, l2)) / length(l2);
print(paste('Percent training error for standard svm on normal data =', training.error));

# Do PCA on the data
pca.subd <- prcomp(nonzero.sub, center = TRUE, scale. = TRUE);
pca.points <- pca.subd$x;
print("Principal Component analysis complete.")

# Do SVM with PCA data
svm.pca.results <- svm(pca.points, y=l2); # pca.subdata
print(svm.pca.results);

# Training error with PCA
training.results.pca <- predict(svm.pca.results, pca.points); # pca.subdata
training.error.pca <- sum(error(training.results.pca, l2)) / length(l2);
print(paste('Percent training error for svm on PCA data =', training.error.pca))

#Print out the classification of the positive points
print('Training classification of positive points on normal data')
print(training.results[l2 > 0])
print('Training classification of positive points on PCA data')
print(training.results.pca[l2 > 0])

#Calculate sensitivity
true_pos = sum(l2 > 0)
pos = sum(training.results[l2 > 0] > 0)
print(paste('Sensitivity of normal svm =', (pos / true_pos)))
pos.pca = sum(training.results.pca[l2 > 0] > 0)
print(paste('Sensitivity of pca svm =', (pos.pca / true_pos)))


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


# Testing data on data with PCA
# Evaluate from the current PCA on the new data
new.pca.d <- predict(pca.subd, newd = nonzero.sub.testing)
# Do SVM with PCA data
#svm.pca.testing <- svm(new.pca.d, y=l.testing); # pca.subdata
#print(svm.pca.testing);
testing.results.pca <- predict(svm.pca.results, new.pca.d); # TODO: CHANGE TO svm.pca.results # svm.pca.testing
testing.error.pca <- sum(error(testing.results.pca, l.testing)) / length(l.testing);
print(paste('Percent testing error for svm on PCA data =', testing.error.pca))


#Calculate sensitivity
true_pos.test = sum(l.testing > 0)
pos.test = sum(testing.results[l.testing > 0] > 0)
print(paste('Testing sensitivity of normal svm =', (pos.test / true_pos.test)))
pos.test.pca = sum(testing.results.pca[l.testing > 0] > 0)
print(paste('Testing sensitivity of pca svm =', (pos.test.pca / true_pos.test)))

sink()
