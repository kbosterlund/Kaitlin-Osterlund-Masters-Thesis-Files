# Temperature Cycles and Spatial Synchrony
# Clustering Analysis
# Kaitlin Osterlund

# import libraries
library(tidyverse)
library(ggalt)
library(vegan)

# set seed
set.seed(1)

load_logged_data <- function(file) {
  # load in logged data

  clustering_tibble <-
    read.csv(file, header = T, row.names = 1)
  clustering_tibble <- clustering_tibble+1
  clustering_tibble <- log(clustering_tibble)

}

create_cluster_plot <- function(data, clusters) {
  # generate cluster plot to Kaitlin's specification

  # arguments:

    # data: tibble containing observations broken down by
    #       temperature, A through F and enrichment, H and L
    # clusters: number of k means clusters to create

  tibble_NMDS <-
    vegan::metaMDS(data, distance = "bray", k=clusters, try=1000, autotransform=F)

  kmeans_clusters <- kmeans(data, clusters)

  tibble_kmeans <-
    kmeans_clusters$cluster %>%
    as.list() %>%
    tibble::as_tibble() %>%
    tidyr::pivot_longer(cols = everything()) %>%
    dplyr::rename(jars = name, cluster = value)

  tibble_plot <-
    vegan::scores(tibble_NMDS) %>%
    dplyr::as_tibble(rownames = 'jars') %>%
    dplyr::left_join(tibble_kmeans) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(jar_temp = str_sub(jars, 0, 1)) %>%
    dplyr::mutate(jar_enrich = str_sub(jars, 2, 2))

  temp_palette <- c("A" = "red", "B" = "orange", "C" = "yellow", "D" = "green", 'E' = 'blue', 'F' = 'purple')

  tibble_plot %>%
    ggplot2::ggplot(aes(x = NMDS1, y = NMDS2)) +
    ggplot2::geom_point(data = tibble_plot %>% filter(jar_enrich == 'L'), aes(colour = jar_temp), shape = 17, size = 3) +
    ggplot2::geom_point(data = tibble_plot %>% filter(jar_enrich == 'H'), aes(colour = jar_temp), size = 3) +
    ggplot2::scale_colour_manual('Temperature', values = temp_palette) +
    ggalt::geom_encircle(s_shape=0.1, expand=0.1, spread=0.1, colour="black", aes(group = cluster))

}

# create plots
data <- load_logged_data("C:/Users/mattb/Desktop/NO PRED Tetrahymena for clustering.csv")
create_cluster_plot(data, 6) # set clusters to 2 or 6
ggsave("C:/Users/mattb/Desktop/NMDSplot.png")

kmeans_list <- seq(2, 8, 2)
for (kmean in kmeans_list) {

  create_cluster_plot(data, kmean)
  ggsave(paste0("C:/Users/mattb/Desktop/example_for_kaitlin", as.character(kmean), ".png"))

}
