data <- read.csv('Data Sets/K9.data', header = FALSE);

# Create a gaussian distribution for each of the 5409 attributes
print('Number of inactive:')
print(length(data[data[,5409] == 'inactive', 5409]));
print('Number of active:')
print(length(data[data[,5409] == 'active', 5409]));
