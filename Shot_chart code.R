library(grid)
library(png)
library(RCurl)
library(tidyverse)
#half court image
court <- rasterGrob(readPNG("NCAA_Court.png"),
                    width=unit(1,"npc"), height=unit(1,"npc"))


ggplot(shot_locations, aes(loc_x, loc_y, color = shot_outcome, shape = three_pt)) +
  annotation_custom(court, 0, 50, 2, 46) +
  geom_point() + ggtitle("All PLayer Shot Locations") + xlim(0, 50) + xlab("") + ylab("")
