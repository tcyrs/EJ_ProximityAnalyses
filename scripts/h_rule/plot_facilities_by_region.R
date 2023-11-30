
####################################################
#########################   Plot Production facility
####################################################

## create map regions dataframe

states <- states()
regions <- fac_map_by_region %>% distinct(NAME, DIVISIONCE)

##function to create maps

for (i in 1:nrow(regions)) {
  map_region <- regions[[i, "NAME"]]
  map_region_id <- regions[[i, "DIVISIONCE"]]
  map_region_sts <- states[states$DIVISION == map_region_id,] %>% select("STUSPS") %>% st_drop_geometry()
  map_region_sts_s <- map_region_sts$STUSPS


  map_data <- fac_map_by_region[fac_map_by_region$NAME == map_region,]

  facilities_map_t <- usmap_transform(map_data)

  fac_map <- plot_usmap(include=c(map_region_sts_s),
                      labels=TRUE,
                      fill = "#C5CFE3",
                      alpha = 0.5) +
  ggrepel::geom_label_repel(data = facilities_map_t,
                            aes(x = x, y = y,
                                label = Label),
                            size = 2, alpha = 0.8,
                            label.r = unit(0.5, "lines"), label.size = 0.3,
                            segment.color = "#A020F0", segment.size = 0.3,
                            min.segment.length = 0.2,
                            seed = 1002) +

  geom_point(data = facilities_map_t,
             aes(x = x, y = y),
             color = "#A020F0", size = 1) +
  theme(legend.position = c(0.85, 0.1))

  fac_map

  ggsave(paste("output/H Rule/h_rule_facilities_map", map_region, ".png"), fac_map)

}
