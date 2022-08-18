# df <- data.frame(x = c(1, 1.1, 1.05, 1.5, 1.4),
#                  y = c(0.9, 1.0, 1.05, 1.02, 1.01),
#                  z = c(0.1, 0.2, -0.1, 0.1, 0.15))
# 
# row.names(df) <- c("A", "B", "C", "D", "E")
# library(proxy)
# round(dist(df, df, method = "euclidean"), 3)

df1 <- data.frame(Advantages = c("handles non-linear data efficiently", "preserves local and global structure", ""),
                  Disadvantages = c("Computationally Complex", "Non-deterministic", "Requires Hyperparameter Tuning"))
df1
library(plotly)
df <- data.frame(x = c(1, 1.1, 1.05, 1.5, 1.4),
                 y = c(0.9, 1.0, 1.05, 1.02, 1.01),
                 z = c(0.1, 0.2, -0.1, 0.1, 0.15))
# library(Rtsne)
# library(ggplot2)
# library(dplyr)
# data(iris)
# iris <- iris[!duplicated(iris), ]
# iris$Species <- as.character(iris$Species)
# iris$Species[iris$Species == 'setosa'] <- "s"
# iris$Species[iris$Species == 'versicolor'] <- "ve"
# iris$Species[iris$Species == 'virginica'] <- "vi"
# iris$Species <- as.factor(iris$Species)
# tsne2 <- Rtsne(iris[,c(1,2,3,4)], dims = 2, perplexity=5, verbose=FALSE, max_iter = 500)
row.names(df) <- c("A", "B", "C", "D", "E")

fig <- plot_ly(data = df[1:5, ], x = ~x, y = ~y, z = ~z,
               mode = "markers",
               type = "scatter3d",
               marker = list(size = 4, color = "blue"),
               text = row.names(df[1:5, ])) 

t <- list(
  family = "sans serif",
  size = 14,
  color = toRGB("grey50"))

fig <- fig %>% add_text(textfont = t, textposition = "top right")

for (i in 2:5) {

    fig <- fig %>%
        add_trace(
            data = df[c(1,i), ],
            text = row.names(df[c(1,i), ]),
            x = ~x,
            y = ~y,
            z = ~z,
            mode = "lines",
            line = list(size = 4, color = "gray", dash = 'dash', legend = NULL), legend = NULL)

}

fig <- fig %>%
    add_trace(
    data = df[1, ],
    x = ~x,
    y = ~y,
    z = ~z,
    mode = "markers",
    type = "scatter3d",
    marker = list(size = 8, color = "red"),
    text = row.names(df[1, ]), legend = NULL) %>%
    
    layout(
        showlegend = F, 
        legend = list(orientation = 'h')
    )

fig


df <- data.frame(x = c(1, 1.1, 1.05, 1.5, 1.4),
                 y = c(0.9, 1.0, 1.05, 1.02, 1.01),
                 z = c(0.1, 0.2, -0.1, 0.1, 0.15))
dists <- round(dist(df, df, method = "euclidean"), 3)[1, ]
dists_y <- dnorm(x = dists, mean = 0, sd = 0.5)

xs <- seq(-1, 1, 0.01)
ys <- dnorm(x = xs, mean = 0, sd = 0.5)

df <- data.frame(xs, ys)

library(ggplot2)
ggplot(data=df, aes(x = xs, y = ys)) +
    geom_line(size = 1) + theme_bw() + 
    geom_line(data = data.frame(xs, ys = 0), size = 1) + 
    geom_point(data = data.frame(dists, ys = 0), aes(dists, ys), size = 3) +
    geom_text(data = data.frame(dists, ys = 0, 
                                label = c("A", "B", "C", "D", "E")), 
              aes(dists, ys, label = label),
              size = 5, vjust = -0.9, hjust = 1.1, color = c("red", "blue", "blue", "blue", "blue")) +
    geom_segment(aes(x = dists[1], y = 0, xend = dists[1], yend = dists_y[1]), linetype = "dotted") +
    geom_segment(aes(x = dists[2], y = 0, xend = dists[2], yend = dists_y[2]), linetype = "dotted") +
    geom_segment(aes(x = dists[3], y = 0, xend = dists[3], yend = dists_y[3]), linetype = "dotted") +
    geom_segment(aes(x = dists[4], y = 0, xend = dists[4], yend = dists_y[4]), linetype = "dotted") +
    geom_segment(aes(x = dists[5], y = 0, xend = dists[5], yend = dists_y[5]), linetype = "dotted") +
    
    geom_segment(aes(x = -1, y = dists_y[1], xend = dists[1], yend = dists_y[1]), linetype = "dotted") +
    geom_segment(aes(x = -1, y = dists_y[2], xend = dists[2], yend = dists_y[2]), linetype = "dotted") +
    geom_segment(aes(x = -1, y = dists_y[3], xend = dists[3], yend = dists_y[3]), linetype = "dotted") +
    geom_segment(aes(x = -1, y = dists_y[4], xend = dists[4], yend = dists_y[4]), linetype = "dotted") +
    geom_segment(aes(x = -1, y = dists_y[5], xend = dists[5], yend = dists_y[5]), linetype = "dotted") +
    
    ggtitle("Normal distribution") + 
    geom_text(data = data.frame(xs = -1, ys = dists_y), 
                                label = c(expression(P["A"]), 
                                          expression(P["B"]), 
                                          expression(P["C"]), 
                                          expression(P["D"]), 
                                          expression(P["E"])),
              color = c("red", "blue", "blue", "blue", "blue"),
              hjust = 1.1, size = 5)

df <- data.frame(x = c(1, 1.1, 1.05, 1.5, 1.4),
                 y = c(0.9, 1.0, 1.05, 1.02, 1.01),
                 z = c(0.1, 0.2, -0.1, 0.1, 0.15))

xs <- round(dist(df, df, method = "euclidean"), 3)[1, ]
ys <- round(dnorm(x = round(dist(df, df, method = "euclidean"), 3)[1, ], mean = 0, sd = 0.5), 3)

df2 <- data.frame(xs, ys, round(ys / sum(ys[2:5]), 3) )
df2 <- t(df2)

colnames(df2) <- c("A", "B", "C", "D", "E")
row.names(df2) <- c("dist", 'probability', "norm. prob")
df2[2,1] <- 0
df2[3,1] <- 0
df2


df <- data.frame(x = c(1, 1.1, 1.05, 1.5, 1.4),
                 y = c(0.9, 1.0, 1.05, 1.02, 1.01),
                 z = c(0.1, 0.2, -0.1, 0.1, 0.15))

df_results <- NULL

for(i in 1:5){

xs <- round(dist(df, df, method = "euclidean"), 3)[i, ]
ys <- round(dnorm(x = xs, mean = 0, sd = 0.5), 3)
ys[i] <- 0
ys <- ys / sum(ys)
df_results <- rbind(df_results, round(ys, 3))

}

df_results <- data.frame(df_results)

names(df_results) <- c("Point A", "Point B", "Point C", "Point D", "Point E")
rownames(df_results) <- c("Point A", "Point B", "Point C", "Point D", "Point E")






df <- data.frame(xs = rnorm(n = 5, mean = 0, sd = 0.1),
                 ys = rnorm(n = 5, mean = 0, sd = 0.1))

p <- ggplot(data=df, aes(x = xs, y = ys)) + geom_point() + theme_bw() + 
    geom_text(data = data.frame(df), label = c("A", "B", "C", "D", "E"),
              size = 5, vjust = -0.9, hjust = 1.1)
)
p
ggplotly(p)

            
row.names(df) <- c("A", "B", "C", "D", "E")

fig <- plot_ly(data = df[1:5, ], x = ~x, y = ~y, size = 10) 
fig <- fig %>% add_text(data = df[1:5, ], x = ~x, y = ~y, size = 10,
                        text = row.names(df[1:5, ]), textfont = t, textposition = "top right")
fig

set.seed(123)


############


df <- data.frame(x = c(1, 1.1, 1.05, 1.5, 1.4),
                 y = c(0.9, 1.0, 1.05, 1.02, 1.01),
                 z = c(0.1, 0.2, -0.1, 0.1, 0.15))

df_results <- NULL

for(i in 1:5){
    
    xs <- round(dist(df, df, method = "euclidean"), 3)[i, ]
    ys <- round(dnorm(x = xs, mean = 0, sd = 0.5), 3)
    ys[i] <- 0
    ys <- ys / sum(ys)
    df_results <- rbind(df_results, round(ys, 3))
    
}

df_results <- data.frame(df_results)

names(df_results) <- c("Point A", "Point B", "Point C", "Point D", "Point E")
rownames(df_results) <- c("P_A", "P_B", "P_C", "P_D", "P_E")
P_dist <- df_results

set.seed(123)
df <- data.frame(xs = rnorm(n = 5, mean = 0, sd = 0.1),
                 ys = rnorm(n = 5, mean = 0, sd = 0.1))
df_results <- NULL

for(i in 1:5){
    
    xs <- round(dist(df, df, method = "euclidean"), 3)[i, ]
    ys <- round(dnorm(x = xs, mean = 0, sd = 0.5), 3)
    ys[i] <- 0
    ys <- ys / sum(ys)
    df_results <- rbind(df_results, round(ys, 3))
    
}

df_results <- data.frame(df_results)

names(df_results) <- c("Point A", "Point B", "Point C", "Point D", "Point E")
rownames(df_results) <- c("Q_A", "Q_B", "Q_C", "Q_D", "Q_E")

Q_dist <- df_results

df <- data.frame(x = c(1, 1.1, 1.05, 1.5, 1.4),
                 y = c(0.9, 1.0, 1.05, 1.02, 1.01),
                 z = c(0.1, 0.2, -0.1, 0.1, 0.15))

row.names(df) <- c("A", "B", "C", "D", "E")
df_res <- round(dist(df, df, method = "euclidean"), 3)

final_dist <- rbind(P_dist, Q_dist)[c(1,6, 2,7,3,8,4,9,5,10), ]




df <- data.frame(x = c(1, 1.1, 1.05, 1.5, 1.4),
                 y = c(0.9, 1.0, 1.05, 1.02, 1.01),
                 z = c(0.1, 0.2, -0.1, 0.1, 0.15))

row.names(df) <- c("A", "B", "C", "D", "E")
df_res <- round(data.frame(as.data.frame.matrix(as.matrix(dist(df, df, method = "euclidean")))), 3)

install.packages("Rtsne")
library(Rtsne)
train<- read.csv(file.choose()) 

Labels<-train$label
train$label<-as.factor(train$label)
colors = rainbow(length(unique(train$label)))
names(colors) = unique(train$label)


tsne <- Rtsne(train[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)
exeTimeTsne<- system.time(Rtsne(train[,-1], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500))

plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=train$label, col=colors[train$label])



iris$Species <- as.character(iris$Species)
table(iris$Species)
iris$Species[iris$Species == 'setosa'] <- "s"
iris$Species[iris$Species == 'versicolor '] <- "ve"
iris$Species[iris$Species == 'virginica  '] <- "vi"

tsne2 <- Rtsne(iris[,c(1,2,3,4)], dims = 2, perplexity=10, verbose=TRUE, max_iter = 500)

plot(tsne2$Y, t='n', main="tsne")
text(tsne2$Y, labels=train$label, col=colors[train$label])







library(Rtsne)
library(ggplot2)
library(dplyr)

df <- data.frame(x = c(1, 1.1, 1.05, 1.5, 1.4),
                 y = c(0.9, 1.0, 1.05, 1.02, 1.01),
                 z = c(0.1, 0.2, -0.1, 0.1, 0.15))

df2 <- data.frame(x = rep(df$x, 10), y = rep(df$x, 10), z = rep(df$z, 10))
df2$x <- df2$x + rnorm(n = length(df2$x), mean = 0, sd = 0.05)
df2$y <- df2$y + rnorm(n = length(df2$y), mean = 0, sd = 0.05)
df2$z <- df2$z + rnorm(n = length(df2$z), mean = 0, sd = 0.05)

colors <- rainbow(5)
cols <- c(rep(colors, 10))
df2$cols <- cols


library(plotly)
fig <- plot_ly(data = df2, x = ~x, y = ~y, z = ~z, color = ~cols,
               mode = "markers",
               type = "scatter3d",
               marker = list(size = 4),
               text = rep(letters[1:5], 10)
               ) 
fig
t <- list(
  family = "sans serif",
  size = 14)

fig <- fig %>% add_text(textfont = t, textposition = "top right") %>%
  
  layout(
    showlegend = F, 
    legend = list(orientation = 'h')
  )

fig


tsne2 <- Rtsne(df2, dims = 2, perplexity = 10, verbose = FALSE, max_iter = 500)
df3 <- data.frame(cbind(tsne2$Y, rep(letters[1:5], 10)))
names(df3) <- c("x", "y", "species")



ggplot(data = df3, aes(x = x, y = y)) +
  geom_text(aes(label = species)) + theme_bw() + theme(legend.position = "none") + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())
       

### Swiss roll
head(iris)

library("KODAMA")


df <- data.frame(swissroll(N=1000))
cols <- colorRampPalette(c("yellow", "black", "red"))(100)
df$cols2 <- (1:nrow(df)) / nrow(df)

names(df)
fig <- plot_ly(data = df, x = ~x, y = ~y, z = ~z, color = ~cols2,
               mode = "markers",
               type = "scatter3d", size = 1)
fig


fig <- plot_ly(data = df, x = ~x, y = ~y, z = ~z, 
               aes(color = ~cols2),
               mode = "markers",
               type = "scatter3d", size = 1)
  

ggplot(df, aes(x, y, z)) +
  geom_point(aes(colour = cols2)) +
  scale_colour_gradientn(colours = terrain.colors(10))


train<- read.csv("C:/Users/garri/Desktop/Moje/R stuff/mnist_test.csv")
head(train)
