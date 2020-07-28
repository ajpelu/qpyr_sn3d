# load library
library(rayshader)
library(sp)
library(sf)
library(raster)
library(scales)
library(tidyverse)


####### PREPARE DATA 
# 1. Prepare data of Elevation 
# Elevation SRTM30 Download from https://dwtkns.com/srtm30m/ 
elev3603 <- raster::raster("rawdata/srtm30/N36W003.hgt")
elev3703 <- raster::raster("rawdata/srtm30/N37W003.hgt")
elev3604 <- raster::raster("rawdata/srtm30/N36W004.hgt")
elev3704 <- raster::raster("rawdata/srtm30/N37W004.hgt")

elev <- raster::merge(elev3603, elev3604, elev3703, elev3704)
rm(elev3603, elev3604, elev3703, elev3704)

# Plot elevation 
# height_shade(raster_to_matrix(elev)) %>% plot_map()

# 2. Prepare LANDAST data 
sn_r <- raster::raster("rawdata/landsat/LC08_L1TP_200034_20200627_20200708_01_T1_B4.TIF")
sn_g <- raster::raster("rawdata/landsat/LC08_L1TP_200034_20200627_20200708_01_T1_B3.TIF")
sn_b <- raster::raster("rawdata/landsat/LC08_L1TP_200034_20200627_20200708_01_T1_B2.TIF")


sn_rgb <- raster::stack(sn_r, sn_g, sn_b)
# raster::plotRGB(sn_rgb, scale=255^2)

# Apply gamma correction
sn_rgb_corrected <- sqrt(raster::stack(sn_r, sn_g, sn_b))
# raster::plotRGB(sn_rgb_corrected)


# 3. QP population data 
qp <- st_read("/Users/ajpelu/Google Drive/_phd/_geoinfo/dist_SN/q_pyr_sn_4326.shp") 



# Utilizar otra informacion de raster 
# Idea. Incluir también datos LIDAR https://github.com/tylermorganwall/rayshader/issues/103 

# 4. PNOA
pnoa <- raster::raster("/Users/ajpelu/Downloads/PNOA_MA_OF_ETRS89_HU30_h50_1027.ecw")

o <- raster::raster("/Users/ajpelu/Downloads/JP2/4604088.JP2")

o <- readGDAL("/Users/ajpelu/Downloads/JP2/4604088.JP2")




# Set CRSs 
raster::crs(sn_r)
raster::crs(elev)
raster::crs(qp)

# Reproject 
elev_utm <- raster::projectRaster(elev, crs = crs(sn_r), method = "bilinear")
qp_utm <- st_transform(qp, crs = crs(sn_r))


######## CROP BY POPULATION 

# get pop 
cam <- as(qp_utm %>% filter(loc == "CAM"), "Spatial")
can <- as(qp_utm %>% filter(loc == "CAN"), "Spatial")
dil <- as(qp_utm %>% filter(loc == "DIL"), "Spatial")
dur <- as(qp_utm %>% filter(loc == "DUR"), "Spatial")
gen <- as(qp_utm %>% filter(loc == "GEN"), "Spatial")
mon <- as(qp_utm %>% filter(loc == "MON"), "Spatial")
poq <- as(qp_utm %>% filter(loc == "POQ"), "Spatial")
tre <- as(qp_utm %>% filter(loc == "TRE"), "Spatial")



# Ejemplo con CAM 
# necesitamos pop, elev_utm, sn_rgb_corrected

#... Crop 
ex <- raster::extent(dil)
elev_pop <- raster::crop(elev_utm, ex)
rgb_pop <- raster::crop(sn_rgb_corrected, ex)
names(rgb_pop) = c("r","g","b")

#... Create 3-layer RGB array of intensities 
r_pop <- rayshader::raster_to_matrix(rgb_pop$r)
g_pop <- rayshader::raster_to_matrix(rgb_pop$g)
b_pop <- rayshader::raster_to_matrix(rgb_pop$b)

elev_matrix <- rayshader::raster_to_matrix(elev_pop)

rgb_array <- array(0,dim=c(nrow(r_pop),ncol(r_pop),3))

rgb_array[,,1] <- r_pop/255 #Red layer
rgb_array[,,2] <- g_pop/255 #Blue layer
rgb_array[,,3] <- b_pop/255 #Green layer

rgb_array <- aperm(rgb_array, c(2,1,3))
plot_map(rgb_array)

# Improve contrast
rgb_contrast <- scales::rescale(rgb_array,to=c(0,1))
plot_map(rgb_contrast)


plot_3d(rgb_contrast, elev_matrix, 
        windowsize = c(1100,900), zscale = 15, shadowdepth = 0,
        zoom=0.9, phi=45,theta=-45,fov=10, background = "#F2E1D0", shadowcolor = "#523E2B")






elev_matrix %>%
  sphere_shade(texture = "bw") %>%
  add_overlay(dil) %>% 
  plot_map() 

  elev_matrix %>%
    sphere_shade() %>%
    add_shadow(ray_shade(elev_matrix)) %>%
    add_shadow(ambient_shade(elev_matrix)) %>%
    plot_3d()





# create 3-layer RGB array of intensities 
# sn_r_cropped <- rayshader::raster_to_matrix(sn_rgb_cropped$r)
# sn_g_cropped <- rayshader::raster_to_matrix(sn_rgb_cropped$g)
# sn_b_cropped <- rayshader::raster_to_matrix(sn_rgb_cropped$b)

sn_elev_matrix <- rayshader::raster_to_matrix(elev_cropped)

sn_rgb_array <- array(0,dim=c(nrow(sn_r_cropped),ncol(sn_r_cropped),3))

sn_rgb_array[,,1] <- sn_r_cropped/255 #Red layer
sn_rgb_array[,,2] <- sn_g_cropped/255 #Blue layer
sn_rgb_array[,,3] <- sn_b_cropped/255 #Green layer

sn_rgb_array <- aperm(sn_rgb_array, c(2,1,3))

plot_map(sn_rgb_array)

# Improve contrast
sn_rgb_contrast <- scales::rescale(sn_rgb_array,to=c(0,1))
plot_map(sn_rgb_contrast)


plot_3d(rgb_contrast, sn_elev_matrix, 
        windowsize = c(1100,900), zscale = 15, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")





render_snapshot(title_text = "Zion National Park, Utah | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)

















## Crop 
bottom_left <- c(x=-3.719817, y=36.885560)
top_right <- c(x=-2.512720,y=37.293617)
extent_latlong <- sp::SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm <- sp::spTransform(extent_latlong, raster::crs(elev_utm))
ee <- raster::extent(extent_utm)


sn_rgb_cropped <- raster::crop(sn_rgb_corrected, ee)
names(sn_rgb_cropped) = c("r","g","b")

elev_cropped <- raster::crop(elev_utm, ee)


# create 3-layer RGB array of intensities 
sn_r_cropped <- rayshader::raster_to_matrix(sn_rgb_cropped$r)
sn_g_cropped <- rayshader::raster_to_matrix(sn_rgb_cropped$g)
sn_b_cropped <- rayshader::raster_to_matrix(sn_rgb_cropped$b)

sn_elev_matrix <- rayshader::raster_to_matrix(elev_cropped)

sn_rgb_array <- array(0,dim=c(nrow(sn_r_cropped),ncol(sn_r_cropped),3))

sn_rgb_array[,,1] <- sn_r_cropped/255 #Red layer
sn_rgb_array[,,2] <- sn_g_cropped/255 #Blue layer
sn_rgb_array[,,3] <- sn_b_cropped/255 #Green layer

sn_rgb_array <- aperm(sn_rgb_array, c(2,1,3))

plot_map(sn_rgb_array)

# Improve contrast
sn_rgb_contrast <- scales::rescale(sn_rgb_array,to=c(0,1))
plot_map(sn_rgb_contrast)



plot_3d(sn_rgb_contrast, sn_elev_matrix, windowsize = c(1100,900), zscale = 15, shadowdepth = -50,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_snapshot(title_text = "Zion National Park, Utah | Imagery: Landsat 8 | DEM: 30m SRTM",
                title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)


### POBLACIONES DE ROBLEDAL 


# Notes:
# https://www.tylermw.com/a-step-by-step-guide-to-making-3d-maps-with-satellite-imagery-in-r/ 