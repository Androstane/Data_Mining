library(pca3d)
pca = prcomp(data[, -1], sacle. = TRUE)
gr = factor(data[,1])
plot(sort(pca$sdev^2, decreasing = TRUE))
i = 2
sorted = sort(pca$sdev^2, decreasing = TRUE)
k = list()
k[1] = sorted[1]
while (i < 364){
  k[i] = k[[i-1]] + sorted[i]
  i = i + 1
}
for (i in c(1:363)){
  k[i] = k[[i]]/k[[363]]
}
plot(c(1:363), k)
pca3d(pca, group = gr)
pca2d(pca, group = gr, biplot = TRUE, biplot.vars = 3)

#if run PCA on people
pca_people = prcomp(dataframe_norm)
plot(sort(pca_people$sdev^2, decreasing = TRUE))

