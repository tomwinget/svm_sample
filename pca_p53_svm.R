# install.packages('e1071')
# install.packages('rpart')

#data <- read.csv('Data Sets/K9.data', header = FALSE, stringsAsFactors = FALSE, na.strings = "?");

library(e1071)
library(rpart)

sampling <- 1:20; # sample first 20 points


relabel <- function (x) {
  if (x == 'inactive') {
    return (-1);
  } else if (x == 'active') {
    return (2);
  }
  return (-1);
}

error <- function (x, y) {
  # x is approximation
  # y is correct label
  
  return ((x <= 0 & y == 1) | (x > 0 && y == -1));
}

randomSample <- function () {
  active_locations <- x[data[,5409] == 'active'];
  inactive_locations <- x[data[,5409] == 'inactive'];
  
}

samples <- length(data[,5409]); # Number of samples (31240)
x <- 1:samples;
y <- 1:5409;
l <- sapply(data[,5409], relabel);

active_locations <- x[data[,5409] == 'active'];

subdata <- data[sampling, 1:5408]; # Grab a subset of the data
l2 <- l[sampling];

# Remove constant / zero columns
nonzero.sub <- subdata[, sapply(subdata, function(v) var(v, na.rm=TRUE)!=0)]

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
pca.subdata <- prcomp(nonzero.sub, center = TRUE, scale. = TRUE);
pca.points <- pca.subdata$x;

# Do SVM with PCA data
svm.pca.results <- svm(pca.points, y=l2); # pca.subdata
print(svm.pca.results);

# Training error with PCA
training.results.pca <- predict(svm.pca.results, pca.points); # pca.subdata
training.error.pca <- sum(error(training.results.pca, l2)) / length(l2);
print(paste('Percent training error for svm on PCA data =', training.error.pca))

