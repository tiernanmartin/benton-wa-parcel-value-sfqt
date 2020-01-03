library(tidyverse)
library(sf) 
library(mapdeck) 
library(measurements)
library(htmltools)

df <- st_read("data/ParcelsAndAssess/ParcelsAndAssess.shp",
              stringsAsFactors = FALSE) %>% 
  st_transform(4326)

df_ready <- df %>% 
  transmute(Parcel_ID,
            OWNER = owner_name,
            ADDRESS = situs_addr,
            ASSESSED_VALUE = appraised_,
            ACRES = if_else(legal_acre == 0,1,legal_acre),
            FT2 = conv_unit(ACRES,"acre","ft2"),
            VALPERFT = ASSESSED_VALUE/FT2, 
            VALPERFT_SCALED = VALPERFT/100, 
            VALPERFT_LABEL = 
              paste0(dollar(VALPERFT),"/ft^2"),
            TOOLTIP = paste0(OWNER,"<br>",ADDRESS, "<br>", VALPERFT_LABEL)
  ) %>% 
  filter(VALPERFT_SCALED >= 0.01)
  

df_ready %>% st_drop_geometry() %>% glimpse()


key <- 'pk.eyJ1IjoidGllcm5hbiIsImEiOiJjazR4OHg4cjgxanJiM2ttemVtNTc4bW15In0.vOZdpgRx9Xs6gI_wzAjjlA'
 

mapdeck(token = key, style = mapdeck_style('dark')) %>% 
  add_polygon(data = df_ready 
              , layer = "polygon_layer"
              , fill_colour = "VALPERFT"
              , elevation = "VALPERFT"
              , tooltip = "TOOLTIP"
              , legend = TRUE)

