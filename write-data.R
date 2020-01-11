
# SETUP -------------------------------------------------------------------

library(tidyverse)
library(sf) 
library(mapdeck) 
library(measurements)
library(htmltools)
library(scales)
library(colourvalues)
library(spatialwidget)
library(htmlwidgets)


# devtools::install_github("SymbolixAU/colourvalues", force = TRUE )
# devtools::install_github("SymbolixAU/spatialwidget", force = TRUE )
# devtools::install_github("SymbolixAU/mapdeck", force = TRUE )


# NOTES -------------------------------------------------------------------


# Data available here: http://benton.municipalcms.com/pview.aspx?id=725&catid=45


# PROCESS DATA ------------------------------------------------------------

ggmap::geocode("Richland, WA")


df <- st_read("data/ParcelsAndAssess/ParcelsAndAssess.shp",
              stringsAsFactors = FALSE) %>% 
  st_transform(4326)

scale_breaks <- c(0,10,30,60,150,300,Inf)

scale_labels <- c("X. $0-$10/ft^2",
                  "1. $10-$30/ft^2",
                  "2. $30-$60/ft^2",
                  "3. $60-$150/ft^2",
                  "4. $150-$300/ft^2",
                  "5. $300 or greater")

df_ready <- df %>% 
  filter(legal_acre > 0) %>% 
  transmute(PIN = Parcel_ID,
            OWNER = owner_name,
            ADDRESS = situs_addr,
            ASSESSED_VALUE = appraised_,
            ACRES = legal_acre,
            FT2 = conv_unit(ACRES,"acre","ft2"),
            VALPERFT = round(ASSESSED_VALUE/FT2),  
            VALPERACRE = round(ASSESSED_VALUE/ACRES, -3),
            VALPERFT_FCT = cut(VALPERFT,
                               breaks = scale_breaks,
                               labels = scale_labels,
                               right = FALSE,
                               ordered_result = TRUE),
            VALPERFT_CHR = as.character(VALPERFT_FCT),
            VALPERFT_LABEL = 
              paste0(dollar(VALPERFT)," per ft^2"),
            VALPERACRE_LABEL = 
              paste0(dollar(VALPERACRE)," per acre"),
            TOOLTIP = paste0(OWNER,"<br>",
                             ADDRESS, "<br>", 
                             "Parcel ID: ",PIN, "<br>", 
                             "<br>",
                             "Size: ", round(ACRES, 1)," acres (", comma(round(FT2,-2)), " ft^2)","<br>",
                             "Assessed Value: ", dollar(ASSESSED_VALUE),"<br>",
                             VALPERACRE_LABEL, "<br>", 
                             VALPERFT_LABEL)
  ) 

df_map <- df_ready %>%filter(VALPERFT >= 10)

df_map %>% st_drop_geometry() %>% glimpse()



# WRITE DATA --------------------------------------------------------------

# st_write(df_ready,"data/benton-wa-assessor-data.gpkg",layer = "parcel", layer_options = "OVERWRITE=TRUE")


# MAP DATA ----------------------------------------------------------------


key <- 'pk.eyJ1IjoidGllcm5hbiIsImEiOiJjazR4OHg4cjgxanJiM2ttemVtNTc4bW15In0.vOZdpgRx9Xs6gI_wzAjjlA'

set_token(key)

map <- mapdeck(style = mapdeck_style('dark'),
        location = c(-119, 46.3),
        zoom = 9) %>% 
  add_polygon(data = df_map  
              , layer = "polygon_layer"
              , fill_colour = "VALPERFT_CHR"
              , elevation = "VALPERFT"
              , elevation_scale = 2
              , auto_highlight = TRUE
              , tooltip = "TOOLTIP"
              , legend = TRUE
              , update_view = FALSE)


# SAVE MAP ----------------------------------------------------------------


saveWidget(widget = map,
           file = here::here("maps/benton-county-parcel-value-over10persqft.html"), 
           selfcontained = FALSE)
