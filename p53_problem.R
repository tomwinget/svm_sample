data <- read.csv('Data Sets/K9.data', header = FALSE, stringsAsFactors = FALSE, na.strings = "?");

print('Number of inactive:')
print(length(data[data[,5409] == 'inactive', 5409]));
print('Number of active:')
print(length(data[data[,5409] == 'active', 5409]));

samples <- length(data[,5409]);
x <- 1:samples;
y <- 1:5409;

active_locations <- x[data[,5409] == 'active'];


subdata <- data[1:20, 1:5408]; # Grab a subset of the data


# Remove any constant (or zero) columns 
# df[,apply(df, 2, var, na.rm=TRUE) != 0]
# df[,sapply(df, function(v) var(v, na.rm=TRUE)!=0)]
#names(subdata[, sapply(subdata, function(v) var(v, na.rm=TRUE)==0)])

# Remove constant / zero columns
nonzero.sub <- subdata[, sapply(subdata, function(v) var(v, na.rm=TRUE)!=0)]

# Remove samples with missing values
nonzero.sub <- nonzero.sub[complete.cases(nonzero.sub), ]
pca.subdata <- prcomp(nonzero.sub, center = TRUE, scale. = TRUE)


#write.table(nonzero.sub, file='tmp.csv')
