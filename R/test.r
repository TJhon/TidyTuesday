library(ggplot2)
library(dplyr)
library(sf)
library(ggtextures)

path_image <- "http://www.hypergridbusiness.com/wp-content/uploads/2012/12/rocks2-256.jpg"

library(ggplot2)
# devtools::install_github("clauswilke/ggtextures")
ggplot(mtcars, aes(cyl, mpg)) +
    ggtextures::geom_textured_bar(stat = "identity", image = path_image)
