datat <- read.csv('tmp.data', header = FALSE, stringsAsFactors = FALSE, na.strings = "?");

pca.datat <- prcomp(datat, center = TRUE, scale. = TRUE) ;

print(pca.datat);
l <- datat[,4];
datatt <- datat[,1:3];

tt <- svm(datatt, y=l);
t3 <- predict(tt, datatt);


print(tt);
print(t3);