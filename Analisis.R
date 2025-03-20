#install packages
install.packages('factoextra')
install.packages('cluster')
install.packages('tidyverse')
install.packages("corrplot")
library(factoextra) 
library(cluster) 
library(tidyverse)
library(corrplot)

#import data
df <- read.delim("clipboard")
View(df)

#membuat tabel data baru untuk memisahkan kolom wilayah
df_new = df[,-1]
View(df_new)
#memasukkan kolom wilayah ke kolom paling kiri
row.names(df_new) = df[,1]

#melihat struktur data
str(df_new)
#melihat ringkasan data, melihat statistik deskriptif
summary(df_new)

#mencari matrix korelasi
corrmatrix <- cor(df_new)
corrplot(corrmatrix, method = 'number')

#menyamakan skala data, apabila scala data berbeda
df_fix = scale(df[2:4])
View(df_fix)

#menentukan jumlah cluster optimum menggunakan metode silhouette
fviz_nbclust(df_new, kmeans, method = "silhouette")
final = kmeans(df_new, 3)
final

#mencari nilai silhouette
silhouette_scores <- silhouette(final$cluster, dist(df_new))
fviz_silhouette(silhouette_scores)

#membuat cluster plot
fviz_cluster(final, data = df_new)

#membuat data cluster 
finalakhir = data.frame(df_new, final$cluster)
View(finalakhir)

#mengurutkan sesuai cluster 
df_final = finalakhir[order(finalakhir$final.cluster),]
View(df_final)

#mencari mean dari setiap cluster per faktor penyebab
df_new%>% mutate(cluster = final$cluster)%>%
  group_by(cluster)%>%summarise_all("mean")

#mencari nilai silhouette k-cluster
silhouette_scores <- vector()  
for (k in 2:10){
  kmeans_result = kmeans(df_new, centers = k)
  silhouette_score <- silhouette(kmeans_result$cluster, dist(df_new))
  silhouette_scores <- c(silhouette_scores, mean(silhouette_score$widths))
}
print(silhouette_scores)
