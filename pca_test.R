datat <- read.csv('tmp.data', header = FALSE, stringsAsFactors = FALSE, na.strings = "?");

pca.datat <- prcomp(datat, center = TRUE, scale. = TRUE) 

print(pca.datat)
