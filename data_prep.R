# STATS 780 Project
# Fraser Walker (001219429)

setwd(paste(
  "C:/Users/frase/Documents/School Work/MSc/Winter 2018/STATS 780/Project/R",
  sep = "/"))

pckgs <- c("dplyr", "ggplot2", "tidyr", "tidytext", "gutenbergr",
           "stringr", "tm", "qdap", "mclust", "cluster",'proxy',
           "e1071", "caret")
lapply(pckgs, library, character.only = TRUE)
rm(pckgs)

# data prep ----
#gutenberg_works(author == "Austen, Jane") # 1
#gutenberg_works(author == "Shakespeare, William") # 2
#gutenberg_works(author == "Dostoyevsky, Fyodor") # 3
#gutenberg_works(author == "Dickens, Charles") # 4

id <- c(121, 141, 158, 161, 1342,
        1513, 1519, 2264, 2265, 2266,
        600, 2197, 2554, 2638, 28054,
        46, 98, 730, 1400, 1023)

# gutenberg_works(gutenberg_id == )
# for referencing a book title

text <- gutenberg_download(id)

# remove author names within text, since they amount to labels
j <- c()
j <- append(j,grep("Austen",text$text))
j <- append(j,grep("Shakespeare",text$text))
j <- append(j,grep("Dostoyevsky|DOSTOYEVSKY|Dostoevsky",text$text))
j <- append(j,grep("Dickens",text$text))
text <- text[-c(j),]

text_df <- text %>% # convert to df
  unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  anti_join(stop_words) %>% # removing stop words
  count(gutenberg_id, word) # add a word counts column

text_df <- text_df[complete.cases(text_df),]

# turn into a TDM
text_dtm <- cast_dtm(text_df, gutenberg_id, word, n) %>%
  weightTfIdf()

# drop empty columns, may or may not appear
# ignore for now
k <- c()
for (i in 1:dim(text_dtm)[2]){
 if (colSums(as.matrix(text_dtm[,i])) == 0){
   k <- append(k,i)
 }
}
text_dtm <- text_dtm[,-k]
rownames(text_dtm) <- as.numeric(rownames(text_dtm))

# normalize document vectors, account for lengths.
for (i in 1:dim(text_dtm)[1]){
  sum_doc <- sqrt(sum((text_dtm[i,])^2))
  print(sum_doc)
  text_dtm[i,] <- (text_dtm[i,])/(sum_doc)
}

# count unique terms ----

unique <- data.frame(rep(0,20))
k <- 1
for (i in id){
  unique[k,1] <- length(termFreq(text_df$word[text_df$gutenberg_id == i]))
  rownames(unique)[k] <- gutenberg_works(gutenberg_id == i)$title
  k <- k+1
}
colnames(unique) <- c("unique_terms")



# append row labels ----
# author label might not be necesarry right now
text_mat <- as.matrix(text_dtm)

for (i in 1:dim(text_dtm)[1]){
  j <- match(id[i], rownames(text_dtm))
  print(j)
  #print(gutenberg_works(gutenberg_id == id[i]))
  rownames(text_mat)[j] <- as.String(gutenberg_works(gutenberg_id == id[i])$title)
  rownames(text_dtm)[j] <- as.String(gutenberg_works(gutenberg_id == id[i])$title)
}

rownames(text_mat)[1] <- "A Christmas Carol" # leave it to Dickens to write a title that long
rownames(text_dtm)[1] <- rownames(text_mat)[1]

# principal component analysis ----
# attempt dimension reduction with PCA, could make DTM less sparse

pca <- prcomp(text_dtm, scale = TRUE)
text_pca <- as.matrix(text_dtm) %*% pca$rotation[,1:14]



# k-means clustering ----

kmeans_text <- kmeans(text_mat, 4, nstart = 5) # 4 clusters: 4 authors
kmeans_text$cluster
kmeans_text$betweenss

kmeans_pca <- kmeans(text_pca, 4, nstart = 5) # 4 clusters: 4 authors
kmeans_pca$cluster

# k-medoids clustering ----

kmedoids_text <- pam(text_dtm, 4, metric = "cosine")
kmedoids_text$clustering
# perfect classification
kmedoids_pca <- pam(text_pca, 4, metric = "euclidean")
kmedoids_pca$clustering
# pca is now truuuuush

# agglomerative hierarchical clustering ----

# full matrix
dist_taxi <- dist(text_mat, method = "manhattan")
dist_euc <- dist(text_mat, method = "Euclidean")
dist_cosine <- dist(text_mat, method = "cosine")
hc_text_taxi <- hclust(dist_taxi, "ward.D2")
hc_text_euc <- hclust(dist_euc, "ward.D2")
hc_text_cosine <- hclust(dist_cosine, "ward.D2")
#plot(hc_text_taxi)
hc_class_taxi <- cutree(hc_text_taxi, k=4)
hc_class_euc <- cutree(hc_text_euc, k=4)
hc_class_cosine <- cutree(hc_text_cosine, k=4)

# pca
dist_taxi_pca <- dist(text_pca, method = "manhattan")
dist_euc_pca <- dist(text_pca, method = "Euclidean")
dist_cos_pca <- dist(text_pca, method = "cosine")
hc_pca_taxi <- hclust(dist_taxi_pca, "ward.D2")
hc_pca_euc <- hclust(dist_euc_pca, "ward.D2")
hc_pca_cosine <- hclust(dist_cos_pca, "ward.D2")
plot(hc_pca_cosine)
hc_class_taxi_pca <- cutree(hc_pca_taxi, k=4)
hc_class_euc_pca <- cutree(hc_pca_euc, k=4)
hc_class_cosine_pca <- cutree(hc_pca_cosine, k=4)

# divisive clustering ----

# full matrix
dhc.text.taxi <- diana(text_mat, metric = "taxi", stand = TRUE)
dhc.text.euc <- diana(text_mat, metric = "euclidean", stand = TRUE)
dhc.text.cosine <- diana(text_mat, metric = "cosine", stand = TRUE)
dhc.class.taxi <- cutree(as.hclust(dhc.text.taxi), k = 4)
dhc.class.euc <- cutree(as.hclust(dhc.text.euc), k = 4)
dhc.class.cosine <- cutree(as.hclust(dhc.text.cosine), k = 4)

# pca
dhc.pca.taxi <- diana(text_pca, metric = "manhattan", stand = TRUE)
dhc.pca.euc <- diana(text_pca, metric = "euclidean", stand = TRUE)
dhc.pca.cosine <- diana(text_pca, metric = "cosine", stand = TRUE)
#plot(divisive.pca)
dhc.class.taxi.pca <- cutree(as.hclust(dhc.pca.taxi), k = 4)
dhc.class.euc.pca <- cutree(as.hclust(dhc.pca.euc), k = 4)
dhc.class.cosine.pca <- cutree(as.hclust(dhc.pca.cosine), k = 4)

# Model Based Clustering (GPCM family) ----

# rerun text_pca just to have numbers and no character strings
pca <- prcomp(text_dtm, scale = TRUE)
text_pca <- as.matrix(text_dtm) %*% pca$rotation[,1:14]

mclust_pca <- Mclust(text_pca)
summary(mclust_pca)
# doesn't do a great job, chooses 6 clusters
# jane austen separates out really well it looks like
# Classification Tables, Comparisons, Graphics ----

# true author labels. #1 - Austen, #2 - Shakespeare, #3 - Dostoyevsky, #4 - Dickens
true <- c(4,4,1,1,1,1,3,4,4,1,4,2,2,3,2,2,2,3,3,3)

# agglomerative hierarchical
table_hc <- data.frame(hc_class_taxi,hc_class_euc,hc_class_cosine)
table_hc_pca <- data.frame(hc_class_taxi_pca, hc_class_euc_pca, hc_class_cosine_pca)

# divisive hierarchical
table_dhc <- data.frame(dhc.class.taxi,dhc.class.euc,dhc.class.cosine)
table_dhc_pca <- data.frame(dhc.class.taxi.pca, 
                            dhc.class.euc.pca, 
                            dhc.class.cosine.pca)
# ahc with cosine similarity after pca
# to agree with values in true
tab1 <- table(true,hc_class_cosine_pca)
tab1 <- cbind(tab1[,3],tab1[,1],tab1[,4],tab1[,2])
classAgreement(tab1)

# kmeans
tab2 <- table(true, kmeans_text$cluster)
tab2 <- cbind(tab2[,3],tab2[,4],tab2[,2],tab2[,1])
classAgreement(tab2)

par(mfrow=c(1,1))# Back to displaying a single figure
x<-text_mat
K<-20
wss<-rep(0,K)
for (k in 1:K){
  wss[k] <- sum(kmeans(x,k)$withinss)
}
plot(1:K,wss,typ="b",ylab="Total within cluster sum of squares",
     xlab="Number of clusters (k)")

# k-medoids
tab3 <- table(true,kmedoids_text$clustering)
tab3 <- cbind(tab3[,2],tab3[,4],tab3[,3],tab3[,1])
classAgreement(tab3)

sil1 <- silhouette(kmedoids_text$clustering, dist(text_mat, method="euclidean"))
rownames(sil1) <- rownames(text_dtm)
sil2 <- silhouette(kmedoids_text$clustering, dist(text_mat, method="cosine"))
rownames(sil2) <- rownames(text_dtm)
plot(sil2)

# curious, sil3 performed on AHC with PCA and cosine similarity.
sil3 <- silhouette(hc_class_cosine_pca, dist_cos_pca)
rownames(sil3) <- rownames(text_dtm)
plot(sil3)
# plot of word frequencies ----
# cite https://www.tidytextmining.com/tidytext.html
# not separated by book
text_df_2 <- text %>% # convert to df
  unnest_tokens(word, text) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
word_freq <- filter(text_df_2, n > 1500) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
word_freq

# comparing word frequencies of authors----
# cite https://gist.github.com/dfalster/5589956 
library(scales)
text_df_3 <- text
source("addNewData.r")
allowedVars <- c("author")
text_df_3 <- addNewData("dataNew.csv", text_df_3, allowedVars)

text_prop <- text_df_3 %>% # convert to df
  unnest_tokens(word, text) %>% # retain capital letters
  mutate(word = str_extract(word, "[a-z']+")) %>%
  anti_join(stop_words) %>% # removing stop words
  count(author, word) %>% # add a word counts column
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>%
  gather(author, proportion, `Charles Dickens`,`Fyodor Dostoyevsky`) # leave out at least one author to do comparison with

ggplot(text_prop, aes(x = proportion, y = `William Shakespeare`, color = abs(`William Shakespeare` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 3) +
  theme(legend.position="none") +
  labs(y = "William Shakespeare", x = NULL)
# great!

# correlations between Dickens and others vocabulary
cor.test(data = text_df_3[text_df_3$author == "Jane Austen",],
         ~ proportion + `Charles Dickens`)
cor.test(data = text_df_3[text_df_3$author == "William Shakespeare",],
         ~ proportion + `Charles Dickens`)
cor.test(data = text_df_3[text_df_3$author == "Fyodor Dostoyevsky",],
         ~ proportion + `Charles Dickens`)