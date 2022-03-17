library(ggplot2)
library(dplyr)
library(sf)
library(ggtextures)

path_image <- "http://www.hypergridbusiness.com/wp-content/uploads/2012/12/rocks2-256.jpg"

library(ggplot2)
# devtools::install_github("clauswilke/ggtextures")
ggplot(mtcars, aes(cyl, mpg)) +
    ggtextures::geom_textured_bar(stat = "identity", image = path_image)

# circular

library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)

mat = matrix(runif(200),10,20)
colnames(mat) = paste0("col",1:ncol(mat))
rownames(mat) = paste0("row",1:nrow(mat))


matriz <-
    mat |>
    data.frame() |>
    rownames_to_column("row") |>
    as_tibble() |>
    pivot_longer(!row) |>
    mutate(
        name = factor(name, levels = colnames(mat))
        , row = factor(row, levels = rownames(mat))
        )
row_num <- length(levels(matriz$row))

matriz |>
    ggplot() +
    aes(x = name, y = as.numeric(row), fill = value) +
    geom_tile() +
    coord_polar(start = -.15) +
    xlim(c("", colnames(mat))) +
    ylim(c(-row_num/1.5, row_num + 1)) +
    theme_void() +
    theme(
        legend.position = c(.5, .5)
        , legend.key.size = unit(.2, "cm")
    ) +
    annotate(
        x = ""
        , y = 1:row_num
        , label = levels(matriz$row)
        , geom = "text"
    )

