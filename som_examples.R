library("kohonen")
library("dplyr")
library("plotly")

### Simple example

# prepare the data
{
    
    x1 <- rnorm(n = 1000, mean = 1, sd = 0.2)
    x2 <- rnorm(n = 1000, mean = 1, sd = 0.1)
    x3 <- rnorm(n = 1000, mean = 4, sd = 0.2)
    x4 <- rnorm(n = 1000, mean = 4, sd = 0.2)
    
    y1 <- rnorm(n = 1000, mean = 1, sd = 0.2)
    y2 <- rnorm(n = 1000, mean = 4, sd = 0.3)
    y3 <- rnorm(n = 1000, mean = 1, sd = 0.2)
    y4  <- rnorm(n = 1000, mean = 4, sd = 0.2)
    
    x <- c(x1, x2, x3, x4)
    y <- c(y1, y2, y3, y4)
    z <- rnorm(n = length(x), mean = 0, sd = 0.01)
    plot(x, y)
    
    df <- data.frame(x, y)
    
}
    
# Create the map
{
    map_dim <- 10
    g <- somgrid(xdim = map_dim, ydim = map_dim, 
                 # topo = "hexagonal",
                 toroidal = FALSE)
    
    df <- data.frame(xs = x, ys = y, zs = 1)
    df2 <- scale(df, center = FALSE, scale = FALSE)
    
    map <- som(df2,
               grid = g,
               # alpha = c(0),
               # radius = 0.7, 
               rlen = 100)
    
    df_map <- map$codes
    df_map <- data.frame(df_map)
    
    fig <- plot_ly(data = df, x = ~xs, y = ~ys, z = ~zs, 
                   mode = "markers",
                   type = "scatter3d",
                   marker = list(size = 1, color = "red")) %>%
        
        add_trace(
            data = df_map, 
            x = ~xs, 
            y = ~ys, 
            z = ~zs, 
            mode = "markers", 
            type = "scatter3d", 
            marker = list(size = 3, color = "blue"))
}
    fig
    
    
# draw the grid
    
    
    map1 <- map$grid$pts
    map1 <- data.frame(map1)
    adj_points <- list()

    for(i in 1:nrow(map1)) {
        print(i)
        x_tmp <- map1[i, 1]
        y_tmp <- map1[i, 2]

        poss1 <- c(max(x_tmp - 1, 1), y_tmp)
        poss2 <- c(min(x_tmp + 1, map_dim), y_tmp)

        poss3 <- c(x_tmp, max(y_tmp - 1, 1))
        poss4 <- c(x_tmp, min(y_tmp + 1, map_dim))

        vec_tmp <- c(which(paste0(map1$x, "_", map1$y) == paste0(poss1[1], "_", poss1[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss2[1], "_", poss2[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss3[1], "_", poss3[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss4[1], "_", poss4[2])))


        vec_tmp <- c(which(paste0(map1$x, "_", map1$y) == paste0(poss1[1], "_", poss1[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss2[1], "_", poss2[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss3[1], "_", poss3[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss4[1], "_", poss4[2])))

        adj_points[[i]] <- vec_tmp


    }


    for(i in 1:length(adj_points)) {
        fig <- fig %>%
            add_trace(
                data = df_map[c(i, adj_points[[i]][1]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                line = list(size = 1, color = "blue")) %>%
            add_trace(
                data = df_map[c(i, adj_points[[i]][2]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                line = list(size = 1, color = "blue")) %>%
            add_trace(
                data = df_map[c(i, adj_points[[i]][3]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                line = list(size = 1, color = "blue")) %>%
            add_trace(
                data = df_map[c(i, adj_points[[i]][4]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                line = list(size = 1, color = "blue"))
    }

    fig <-  fig %>%
        layout(
            showlegend = FALSE,
            legend = list(orientation = 'h')
        )
    fig
    
# Plots
    
    plot(map, type="changes")
    plot(map, type="count", main="Node Counts", palette.name = coolBlueHotRed)    

    
################################################################################
################################################################################
################################################################################    
# Example 2    
    
# prepare the data
{
    
    x1 <- rnorm(n = 1000, mean = 1, sd = 0.2)
    x2 <- rnorm(n = 1000, mean = 1, sd = 0.1)
    x3 <- rnorm(n = 1000, mean = 4, sd = 0.2)
    x4 <- rnorm(n = 1000, mean = 4, sd = 0.2)
    x5 <- rnorm(n = 500, mean = 2.5, sd = 0.2)
    
    y1 <- rnorm(n = 1000, mean = 1, sd = 0.2)
    y2 <- rnorm(n = 1000, mean = 4, sd = 0.3)
    y3 <- rnorm(n = 1000, mean = 1, sd = 0.2)
    y4  <- rnorm(n = 1000, mean = 4, sd = 0.2)
    x5 <- rnorm(n = 500, mean = 6, sd = 0.2)
    
    x <- c(x1, x2, x3, x4, x5)
    y <- c(y1, y2, y3, y4, y5)
    z <- rnorm(n = length(x), mean = 0, sd = 0.01)
    plot(x, y)
    
    df <- data.frame(x, y)
    
}

# Create the map
{
    map_dim <- 50
    g <- somgrid(xdim = map_dim, ydim = map_dim, 
                 # topo = "hexagonal",
                 toroidal = FALSE)
    
    df <- data.frame(xs = x, ys = y, zs = 1)
    df2 <- scale(df, center = FALSE, scale = FALSE)
    
    map <- som(df2,
               grid = g,
               # alpha = c(0),
               # radius = 0.7, 
               rlen = 100)
    
    df_map <- map$codes
    df_map <- data.frame(df_map)
    
    fig <- plot_ly(data = df, x = ~xs, y = ~ys, z = ~zs, 
                   mode = "markers",
                   type = "scatter3d",
                   marker = list(size = 1, color = "red")) %>%
        
        add_trace(
            data = df_map, 
            x = ~xs, 
            y = ~ys, 
            z = ~zs, 
            mode = "markers", 
            type = "scatter3d", 
            marker = list(size = 3, color = "blue"))
}
fig

# Plots

plot(map, type="changes")
plot(map, type="count", main="Node Counts", palette.name = coolBlueHotRed)  





################################################################################
################################################################################
################################################################################    
# Example 3


# 1/4 Sphere
    phi <- runif(n = 10000, min = 0, max = pi)
    theta <- runif(n = 10000, min = 0, max = pi / 2)
    r <- runif(n = 10000, min = 0.98, max = 1.02)
    
    
    xs = r * sin(phi) * cos(theta)
    ys = r * sin(phi) * sin(theta)
    zs = r * cos(phi)
    
    df <- data.frame(xs, ys, zs)
    fig <- plot_ly(df, x = ~xs, y = ~ys, z = ~zs, marker = list(size = 1, color = "red"))
    fig

    
    # Create the map
    {
        map_dim <- 10
        g <- somgrid(xdim = map_dim, ydim = map_dim, 
                     toroidal = FALSE)
        df2 <- scale(df, center = FALSE, scale = FALSE)
        
        map <- som(df2,
                   grid = g,
                   # alpha = c(0),
                   # radius = 0.7, 
                   rlen = 100)
        
        df_map <- map$codes
        df_map <- data.frame(df_map)
        
        fig <- plot_ly(data = df, x = ~xs, y = ~ys, z = ~zs, 
                       mode = "markers",
                       type = "scatter3d",
                       marker = list(size = 1, color = "red")) %>%
            
            add_trace(
                data = df_map, 
                x = ~xs, 
                y = ~ys, 
                z = ~zs, 
                mode = "markers", 
                type = "scatter3d", 
                marker = list(size = 4, color = "blue"))
    }
    fig
    
    
    ### draw the grid

    map1 <- map$grid$pts
    map1 <- data.frame(map1)
    adj_points <- list()
    
    for(i in 1:nrow(map1)) {
        print(i)
        x_tmp <- map1[i, 1]
        y_tmp <- map1[i, 2]
        
        poss1 <- c(max(x_tmp - 1, 1), y_tmp)
        poss2 <- c(min(x_tmp + 1, map_dim), y_tmp)
        
        poss3 <- c(x_tmp, max(y_tmp - 1, 1))
        poss4 <- c(x_tmp, min(y_tmp + 1, map_dim))
        
        vec_tmp <- c(which(paste0(map1$x, "_", map1$y) == paste0(poss1[1], "_", poss1[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss2[1], "_", poss2[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss3[1], "_", poss3[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss4[1], "_", poss4[2])))
        
        
        vec_tmp <- c(which(paste0(map1$x, "_", map1$y) == paste0(poss1[1], "_", poss1[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss2[1], "_", poss2[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss3[1], "_", poss3[2])),
                     which(paste0(map1$x, "_", map1$y) == paste0(poss4[1], "_", poss4[2])))
        
        adj_points[[i]] <- vec_tmp
        
        
    }
    
    
    for(i in 1:length(adj_points)) {
        fig <- fig %>%
            add_trace(
                data = df_map[c(i, adj_points[[i]][1]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                line = list(size = 1, color = "blue")) %>%
            add_trace(
                data = df_map[c(i, adj_points[[i]][2]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                line = list(size = 1, color = "blue")) %>%
            add_trace(
                data = df_map[c(i, adj_points[[i]][3]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                line = list(size = 1, color = "blue")) %>%
            add_trace(
                data = df_map[c(i, adj_points[[i]][4]), ],
                x = ~xs,
                y = ~ys,
                z = ~zs,
                mode = "lines",
                type = "scatter3d",
                line = list(size = 1, color = "blue"))
    }
    
    fig <-  fig %>%
        layout(
            showlegend = FALSE,
            legend = list(orientation = 'h')
        )
    fig
    
    
    
    
################################################################################
################################################################################
################################################################################    
# Example 4
    
    
    alpha <- runif(n = 10000, min = 0, max = 2 * pi)
    beta <- runif(n = 10000, min = 0, max = 2 * pi)
    r <- 3
    R <- 12

    xs <- (R + r * cos(alpha)) * cos(beta)
    ys <- (R + r * cos(alpha)) * sin(beta)
    zs <- sin(alpha)
    
    df <- data.frame(xs, ys, zs)
    fig <- plot_ly(df, x = ~xs, y = ~ys, z = ~zs, marker = list(size = 1, color = "red"))
    fig
    }
    
    # Create the map
    {
        map_dim <- 40
        g <- somgrid(xdim = map_dim, ydim = map_dim, 
                     toroidal = FALSE)
        df2 <- scale(df, center = FALSE, scale = FALSE)
        
        map <- som(df2,
                   grid = g,
                   # alpha = c(0),
                   # radius = 0.7, 
                   rlen = 100)
        
        df_map <- map$codes
        df_map <- data.frame(df_map)
        
        fig <- plot_ly(data = df, x = ~xs, y = ~ys, z = ~zs, 
                       mode = "markers",
                       type = "scatter3d",
                       marker = list(size = 1, color = "red")) %>%
            
            add_trace(
                data = df_map, 
                x = ~xs, 
                y = ~ys, 
                z = ~zs, 
                mode = "markers", 
                type = "scatter3d", 
                marker = list(size = 4, color = "blue"))
    }
    fig
    
    
    ### draw the grid
    # 
    # map1 <- map$grid$pts
    # map1 <- data.frame(map1)
    # adj_points <- list()
    # 
    # for(i in 1:nrow(map1)) {
    #     print(i)
    #     x_tmp <- map1[i, 1]
    #     y_tmp <- map1[i, 2]
    #     
    #     poss1 <- c(max(x_tmp - 1, 1), y_tmp)
    #     poss2 <- c(min(x_tmp + 1, map_dim), y_tmp)
    #     
    #     poss3 <- c(x_tmp, max(y_tmp - 1, 1))
    #     poss4 <- c(x_tmp, min(y_tmp + 1, map_dim))
    #     
    #     vec_tmp <- c(which(paste0(map1$x, "_", map1$y) == paste0(poss1[1], "_", poss1[2])),
    #                  which(paste0(map1$x, "_", map1$y) == paste0(poss2[1], "_", poss2[2])),
    #                  which(paste0(map1$x, "_", map1$y) == paste0(poss3[1], "_", poss3[2])),
    #                  which(paste0(map1$x, "_", map1$y) == paste0(poss4[1], "_", poss4[2])))
    #     
    #     
    #     vec_tmp <- c(which(paste0(map1$x, "_", map1$y) == paste0(poss1[1], "_", poss1[2])),
    #                  which(paste0(map1$x, "_", map1$y) == paste0(poss2[1], "_", poss2[2])),
    #                  which(paste0(map1$x, "_", map1$y) == paste0(poss3[1], "_", poss3[2])),
    #                  which(paste0(map1$x, "_", map1$y) == paste0(poss4[1], "_", poss4[2])))
    #     
    #     adj_points[[i]] <- vec_tmp
    #     
    #     
    # }
    # 
    # 
    # for(i in 1:length(adj_points)) {
    #     fig <- fig %>%
    #         add_trace(
    #             data = df_map[c(i, adj_points[[i]][1]), ],
    #             x = ~xs,
    #             y = ~ys,
    #             z = ~zs,
    #             mode = "lines",
    #             type = "scatter3d",
    #             line = list(size = 1, color = "blue")) %>%
    #         add_trace(
    #             data = df_map[c(i, adj_points[[i]][2]), ],
    #             x = ~xs,
    #             y = ~ys,
    #             z = ~zs,
    #             mode = "lines",
    #             type = "scatter3d",
    #             line = list(size = 1, color = "blue")) %>%
    #         add_trace(
    #             data = df_map[c(i, adj_points[[i]][3]), ],
    #             x = ~xs,
    #             y = ~ys,
    #             z = ~zs,
    #             mode = "lines",
    #             type = "scatter3d",
    #             line = list(size = 1, color = "blue")) %>%
    #         add_trace(
    #             data = df_map[c(i, adj_points[[i]][4]), ],
    #             x = ~xs,
    #             y = ~ys,
    #             z = ~zs,
    #             mode = "lines",
    #             type = "scatter3d",
    #             line = list(size = 1, color = "blue"))
    # }
    # 
    # fig <-  fig %>%
    #     layout(
    #         showlegend = FALSE,
    #         legend = list(orientation = 'h')
    #     )
    # fig
    # 