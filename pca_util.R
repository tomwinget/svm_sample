loadData <- function () {
  data <- read.csv('Data Sets/K9.data', header = FALSE, stringsAsFactors = FALSE, na.strings = "?");
  return (data);
}

relabel <- function (x) {
  if (x == 'inactive') {
    return (-1);
  } else if (x == 'active') {
    return (hyperparameter);
  }
  return (-1);
}

error <- function (x, y) {
  # x is approximation
  # y is correct label
  
  return ((x <= 0 & y >= 1) | (x > 0 && y == -1));
}

# must run the following line of code before running the randomSample function
# if(!exists("d")) d <-loadData();
randomSample <- function (percent = 0.5) {
  x <- 1:31420;
  active_locations <- sample(x[d[,5409] == 'active']);
  inactive_locations <- sample(x[d[,5409] == 'inactive']);
  act <- length(active_locations);
  inact <- length(inactive_locations);
  act_sam <- active_locations[1:(percent * act)];
  inact_sam <- inactive_locations[1:(percent * inact)];
  print(paste('number of active samples =', length(act_sam), 'number of inactive samples =', length(inact_sam), '(before filtering)'))
  return (c(act_sam, inact_sam));
}