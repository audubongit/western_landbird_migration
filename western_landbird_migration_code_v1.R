# setup ------------------------------------------------------------------------
# libraries
library(RColorBrewer)
library(colorspace)
library(grid)
library(sf)
library(ggplot2)
library(rasterVis)
library(tidyr)
library(stringr)
library(dplyr)
library(propagate)
library(ebirdst)
library(raster)

# options
options(scipen=9999999)
options(max.print=99999)

# pointers
extract <- raster::extract
select <- dplyr::select

# plot
theme_timeseries <- function (base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = rel(0.9), color="gray10", angle = 0),
          axis.text.y = element_text(size = rel(0.9), color="gray10", angle = 0),
          strip.background = element_rect(fill = "grey80"),
          legend.key = element_rect(fill = "white", colour = NA),
          plot.title = element_text(size=14, hjust = 0.5,
                                    margin=margin(t=5, b=10)),
          legend.position="right",
          complete = TRUE)
}; theme_set(theme_timeseries())

# working directory
setwd("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\CO River Delta\\R Work")

# crs's
ebird_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
epsg102008 <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# ------------------------------------------------------------------------------




# get maps and extra data ------------------------------------------------------
# focal areas
dir("./shapes")
focal <- read_sf("./shapes/focal_cv4.shp") %>%
  mutate(FID=0) %>% dplyr::select(FID)
na <- read_sf("./shapes/na.shp") %>% st_union(focal) %>% select(-FID.1)
west <- read_sf("./shapes/west.shp") %>% st_union(focal) %>% select(-FID.1)
coast <- read_sf("./shapes/coast.shp") %>% st_union(focal) %>% select(-FID.1)
dir("Z:/Migratory_Bird_Initiative/Western Landbirds/CO River Delta/Shapes/transects")
grid <- read_sf("Z:/Migratory_Bird_Initiative/Western Landbirds/CO River Delta/Shapes/transects/cv4tran.shp") %>% 
  mutate(FID=c(0:(nrow(.)-1))) %>% select(FID) %>% st_transform(ebird_crs)
grid_cell <- grid[1,]
grid_extent <- as(st_buffer(st_combine(grid), 0.01), "Spatial")
grid_extent <- as(grid_extent, "sf")

# plot
plot(st_geometry(st_transform(na, epsg102008)), border="#619CFF", lwd=2)
plot(st_geometry(st_transform(west, epsg102008)), add=T, border="#00BA38", lwd=2)
plot(st_geometry(st_transform(coast, epsg102008)), add=T, border="#F8766D", lwd=2)
plot(st_geometry(st_transform(grid, epsg102008)), add=T, border="gray60", lwd=2)
plot(st_geometry(st_transform(grid_cell, epsg102008)), add=T, border="green", lwd=2)
plot(st_geometry(st_transform(grid_extent, epsg102008)), add=T, border="gray80", lwd=2)
plot(st_geometry(st_transform(focal, epsg102008)), add=T, border="black", lwd=2)

# pop size data
collapse_est <- read.csv("bird_decline.csv")
pif_bcr_est <- read.csv("pif_pop_estimates.csv")
# ------------------------------------------------------------------------------




# get spp data -----------------------------------------------------------------
# make species list
# View(ebirdst_runs)
spp_list <- c("Western Meadowlark")


##This is a complete list of 131 species to be used for the Western Landbird Migration Project
##Created 19 March 2020 by Bill DeLuca
##Input was received by the Western Water and Audubon CA stakeholders
##Shapefiles of the latitude transects are located here Z:/Migratory_Bird_Initiative/Western Landbirds/CO River Delta/Shapes/transects
##There are also new shapefiles with all 5 sites converted to Albers Equal Area here Z:/Migratory_Bird_Initiative/Western Landbirds/CO River Delta/Shapes
spp_list <- c("Yellow-billed Cuckoo", "Black Swift",	"Vaux's Swift",	"White-throated Swift",
              "Black-chinned Hummingbird",	"Anna's Hummingbird",	"Costa's Hummingbird",	"Rufous Hummingbird",
              "Allen's Hummingbird",	"Calliope Hummingbird",	"Ash-throated Flycatcher",	"Brown-crested Flycatcher",
              "Cassin's Kingbird",	"Western Kingbird",	"Olive-sided Flycatcher",	"Western Wood-Pewee",
              "Willow Flycatcher",	"Hammond's Flycatcher",	"Gray Flycatcher",	"Dusky Flycatcher",
              "Pacific-slope Flycatcher",	"Black Phoebe",	"Say's Phoebe",	"Vermilion Flycatcher",	"Loggerhead Shrike",
              "Bell's Vireo",	"Hutton's Vireo",	"Cassin's Vireo",	"Plumbeous Vireo",	"Warbling Vireo",	"Horned Lark",
              "Bank Swallow",	"Tree Swallow",	"Violet-green Swallow",	"Northern Rough-winged Swallow",
              "Purple Martin",	"Barn Swallow",	"Cliff Swallow",	"Juniper Titmouse",	"Red-breasted Nuthatch",
              "White-breasted Nuthatch",	"Pygmy Nuthatch",	"Brown Creeper",	"Rock Wren",	"Canyon Wren",
              "House Wren",	"Pacific Wren",	"Marsh Wren",	"Bewick's Wren",	"Cactus Wren",	"Blue-gray Gnatcatcher",
              "California Gnatcatcher",	"Black-tailed Gnatcatcher",	"American Dipper",	"Golden-crowned Kinglet",
              "Ruby-crowned Kinglet",	"Western Bluebird",	"Mountain Bluebird",	"Townsend's Solitaire",
              "Swainson's Thrush",	"Hermit Thrush",	"American Robin",	"Varied Thrush",	"California Thrasher",
              "LeConte's Thrasher",	"Crissal Thrasher",	"Sage Thrasher",	"Northern Mockingbird",	"Cedar Waxwing",
              "Phainopepla",	"American Pipit",	"Evening Grosbeak",	"House Finch",	"Purple Finch",	"Cassin's Finch",
              "Pine Siskin",	"Lesser Goldfinch",	"Lawrence's Goldfinch",	"American Goldfinch",	"Grasshopper Sparrow",
              "Black-throated Sparrow",	"Lark Sparrow",	"Lark Bunting",	"Chipping Sparrow",	"Black-chinned Sparrow",
              "Brewer's Sparrow",	"Fox Sparrow",	"Dark-eyed Junco",	"White-crowned Sparrow",
              "Golden-crowned Sparrow",	"White-throated Sparrow",	"Sagebrush Sparrow",	"Bell's Sparrow",
              "Vesper Sparrow",	"Savannah Sparrow",	"Song Sparrow",	"Lincoln's Sparrow",	"Abert's Towhee",
              "California Towhee",	"Rufous-crowned Sparrow",	"Green-tailed Towhee",	"Spotted Towhee",
              "Yellow-breasted Chat",	"Yellow-headed Blackbird",	"Western Meadowlark",	"Hooded Oriole",
              "Bullock's Oriole",	"Scott's Oriole",	"Red-winged Blackbird",	"Tricolored Blackbird",	"Bronzed Cowbird",
              "Brown-headed Cowbird",	"Brewer's Blackbird",	"Great-tailed Grackle",	"Orange-crowned Warbler",
              "Lucy's Warbler",	"Nashville Warbler",	"MacGillivray's Warbler",	"Common Yellowthroat",
              "American Redstart",	"Yellow Warbler",	"Yellow-rumped Warbler",	"Black-throated Gray Warbler",
              "Townsend's Warbler",	"Hermit Warbler",	"Wilson's Warbler",	"Summer Tanager",	"Western Tanager",
              "Black-headed Grosbeak",	"Blue Grosbeak",	"Lazuli Bunting")
spp_list %in% ebirdst_runs$common_name
# ------------------------------------------------------------------------------




# loop turned into function --------------------------------------------
run_species <- function(spp1="Yellow Warbler"){
  # b=1
  # loop through species b=1
  # for(b in 1:length(spp_list)){
  # for(b in 1:5){
  # define sp
  # spp1 <- spp_list[b]
  extract <- raster::extract
  select <- dplyr::select
  spp1 <- spp1
  print(paste("starting species", spp1))
  
  # get stem layers
  sp_path <- ebirdst_download(species = spp1, force=T)
  abd <- load_raster("abundance", path = sp_path)
  lcl <- load_raster("abundance_lower", path = sp_path)
  ucl <- load_raster("abundance_upper", path = sp_path)
  
  # # pull population totals from collapse paper
  # tot_n <- collapse_est$popest[which(collapse_est$species==spp1)]
  # tot_n_lcl <- collapse_est$popestlci[which(collapse_est$species==spp1)]
  # tot_n_ucl <- collapse_est$popestuci[which(collapse_est$species==spp1)]
  # tot_n_se <- (tot_n_ucl - tot_n_lcl) / 3.93
  # ann_trend <- collapse_est[,2][which(collapse_est$species==spp1)]
  
  # get na population totals from summed PIF estimates
  pif_bcr_est_na <- pif_bcr_est %>% filter(EnglishName==spp1)
  pif_est_na_sum <- sum(pif_bcr_est_na$PopulationEstimate, na.rm=T)
  pif_est_na_sum_se <- sqrt(sum(((pif_bcr_est_na$UCL - pif_bcr_est_na$LCL) / 
                                   3.93)^2)) # convert to se and then delta method
  
  # get west population totals from summed PIF estimates
  west_bcr <- as.character(c(4, 5, 9, 10, 15, 16, 32, 33))
  pif_bcr_est_west <- pif_bcr_est_na %>% 
    mutate(BCR=as.character(BCR)) %>% filter(BCR %in% west_bcr)
  pif_est_west_sum <- sum(pif_bcr_est_west$PopulationEstimate, na.rm=T)
  pif_est_west_sum_se <- sqrt(sum(((pif_bcr_est_west$UCL - 
                                      pif_bcr_est_west$LCL) / 3.93)^2))
  
  # get coast population totals from summed PIF estimates
  coast_bcr <- c(5, 15, 32, 33)
  pif_bcr_est_coast <- pif_bcr_est_na %>% 
    mutate(BCR=as.character(BCR)) %>% filter(BCR %in% coast_bcr)
  pif_est_coast_sum <- sum(pif_bcr_est_coast$PopulationEstimate, na.rm=T)
  pif_est_coast_sum_se <- sqrt(sum(((pif_bcr_est_coast$UCL - 
                                       pif_bcr_est_coast$LCL) / 3.93)^2))
  
  # get seasons
  sp_dates <- filter(ebirdst_runs, common_name == spp1) %>%
    select(setdiff(matches("(start)|(end)"), matches("year_round"))) %>%
    gather("label", "date") %>%
    separate(label, c("season", "start_end"), "_(?=s|e)") %>%
    spread(start_end, date) %>%
    select(season, start_dt, end_dt)
  sp_dates <- mutate(sp_dates, pass = !(is.na(start_dt) | is.na(end_dt)))
  weeks <- parse_raster_dates(abd)
  weeks_season <- rep(NA_character_, length(weeks))
  for (i in seq_len(nrow(sp_dates))) {
    s <- sp_dates[i, ]
    if (!s$pass) {
      next()
    }
    if (s$start_dt <= s$end_dt) {
      in_season <- weeks >= s$start_dt & weeks <= s$end_dt
    } else {
      in_season <- weeks >= s$start_dt | weeks <= s$end_dt
    }
    weeks_season[in_season] <- s$season
  }
  # table(weeks_season)
  
  # drop weeks not assigned to season
  week_pass1 <- !is.na(weeks_season)
  abd <- abd[[which(week_pass1)]]
  lcl <- lcl[[which(week_pass1)]]
  ucl <- ucl[[which(week_pass1)]]
  weeks1 <- weeks[week_pass1]
  weeks_season1 <- weeks_season[week_pass1]
  # table(weeks_season1)
  
  # save breeding season layers
  week_pass2 <- weeks_season1=="breeding"
  abd_breed <- abd[[which(week_pass2)]]
  lcl_breed <- lcl[[which(week_pass2)]]
  ucl_breed <- ucl[[which(week_pass2)]]
  weeks2 <- weeks1[week_pass2]
  weeks_season2 <- weeks_season1[week_pass2]
  # table(weeks_season2)
  
  # keep all remaining weeks
  week_pass3 <- weeks_season1=="prebreeding_migration" |
    weeks_season1=="postbreeding_migration" |
    weeks_season1=="breeding" |
    weeks_season1=="nonbreeding"
  abd_mig <- abd[[which(week_pass3)]]
  lcl_mig <- lcl[[which(week_pass3)]]
  ucl_mig <- ucl[[which(week_pass3)]]
  weeks3 <- weeks1[week_pass3]
  weeks_season3 <- weeks_season1[week_pass3]
  # table(weeks_season3)
  
  # get breeding season mean and produce se layer from conf lims
  var_breed <- abd_breed
  for (l in 1:nlayers(abd_breed)){
    var_breed[[l]] <- ((ucl_breed[[l]] - lcl_breed[[l]]) / 2.56)^2
  }
  var_breed <- sum(var_breed, na.rm=T)
  abd_breed <- mean(abd_breed, na.rm=T)
  # plot(var_breed)
  # plot(abd_breed)
  # plot(st_geometry(na), add=T)
  
  # get summed breeding index with standard error from quadrature for na
  na_sum <- cellStats(abd_breed, sum, na.rm=T)
  na_sum_se <- sqrt(cellStats(var_breed, sum, na.rm=T))
  
  # make empty df and loop through weeks 
  out_df <- c()
  sums_df <- c()
  # w=21
  for(w in 1:length(weeks_season3)){
    # get ids
    week1 <- weeks3[w]
    seas1 <- weeks_season3[w]
    
    # pull maps
    lay1 <- abd_mig[[w]]
    lay2 <- ((ucl_mig[[w]] - lcl_mig[[w]]) / 2.56)^2
    
    # get focal sums and se
    focal_sum <- cellStats(mask(lay1, focal), sum, na.rm=T)
    focal_sum_se <- sqrt(cellStats(mask(lay2, focal), sum, na.rm=T)) # delta method
    
    # get grid cell info
    grid_grid_n <- crop(lay1, grid_extent)
    grid_grid_se <- crop(lay2, grid_extent)
    grid_grid_se[is.na(grid_grid_se)] <- 0
    grid_grid_area <- (grid_grid_n * 0) + 1
    cell_areas <- extract(grid_grid_area, st_zm(grid), sum, na.rm=T)
    cell_sums <- extract(grid_grid_n, st_zm(grid), sum, na.rm=T)
    cell_se_sums <- extract(grid_grid_se, st_zm(grid),
                            fun=function(x, ...) sqrt(sum(x)))
    sums_dfi <- data.frame(cell_id=1:length(cell_sums),
                           cell_area=cell_areas,
                           cell_sum=cell_sums,
                           cell_sum_se=cell_se_sums)
    sums_dfi$week <- week1
    sums_dfi$season <- seas1
    sums_df <- rbind(sums_df, sums_dfi)
    
    # get proportions
    expr1 <- expression(y/x)
    x <- c(na_sum, na_sum_se)
    y <- c(focal_sum, focal_sum_se)
    df1 <- cbind(x, y)
    res1 <- propagate(expr=expr1, data = df1, second.order=F,
                      do.sim=F)
    na_prop <- res1$prop[1]
    if(is.na(na_prop)) na_prop <- 0
    na_prop_se <- res1$prop[3]
    if(is.na(na_prop_se)) na_prop_se <- 0
    
    # make row
    out <- data.frame(species=spp1, #collapse_ann_trend=ann_trend,
                      pif_est_na_sum, pif_est_na_sum_se,
                      pif_est_west_sum, pif_est_west_sum_se, 
                      pif_est_coast_sum, pif_est_coast_sum_se,
                      season=seas1, week=week1) 
    
    # add n_migs
    expr1 <- expression(y * x)
    x <- c(na_prop, na_prop_se)
    y <- c(pif_est_na_sum, pif_est_na_sum_se)
    df1 <- cbind(x, y)
    res1 <- propagate(expr=expr1, data=df1, second.order=F,
                      do.sim=F)
    out$n_migs <- res1$prop[1]
    if(is.na(out$n_migs)) out$n_migs <- 0
    out$n_migs_se <- res1$prop[3]
    if(is.na(out$n_migs_se)) out$n_migs_se <- 0
    
    # add row
    out_df <- rbind(out_df, out)
  }
  
  # clean and stack and save
  results <- out_df %>% as.data.frame()
  write.csv(results, paste0("Z:/Migratory_Bird_Initiative/Western Landbirds/CO River Delta/R Work/output/",
                            spp1, " number_migrants.csv"), na="", 
            row.names=F)
  grid_props <- sums_df %>% as.data.frame()
  gp1 <- grid_props %>% 
    group_by(week) %>%
    summarise(cell_area_sum=sum(cell_area),
              cell_sum_sum=sum(cell_sum),
              cell_sum_sum_se=sqrt(sum(cell_sum_se^2)))
  grid_props <- grid_props %>% left_join(gp1)
  write.csv(grid_props, paste0("Z:/Migratory_Bird_Initiative/Western Landbirds/CO River Delta/R Work/output/", 
                               spp1, " grid_proportions.csv"), na="", 
            row.names=F)

  # fraction data
  # head(results)
  # names(results)
  seas_sums <- results %>% 
    group_by(species, #collapse_ann_trend,
             pif_est_na_sum, pif_est_na_sum_se,
             pif_est_west_sum, pif_est_west_sum_se,
             pif_est_coast_sum, pif_est_coast_sum_se,
             season) %>%
    summarise(sum_migs=sum(n_migs),
              sum_migs_se=sqrt(sum(n_migs_se^2)))
  
  # get fractions
  # na fraction
  expr1 <- expression(x/y)
  seas <- "prebreeding_migration"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_na_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_na_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$na_prop <- NA
  seas_sums$na_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$na_prop_se <- NA
  seas_sums$na_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$na_prop_lcl <- NA
  seas_sums$na_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$na_prop_ucl <- NA
  seas_sums$na_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  seas <- "postbreeding_migration"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_na_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_na_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$na_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$na_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$na_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$na_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  seas <- "breeding"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_na_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_na_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$na_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$na_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$na_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$na_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  seas <- "nonbreeding"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_na_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_na_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$na_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$na_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$na_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$na_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  
  # west fraction
  expr1 <- expression(x/y)
  seas <- "prebreeding_migration"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_west_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_west_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$west_prop <- NA
  seas_sums$west_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$west_prop_se <- NA
  seas_sums$west_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$west_prop_lcl <- NA
  seas_sums$west_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$west_prop_ucl <- NA
  seas_sums$west_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  seas <- "postbreeding_migration"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_west_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_west_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$west_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$west_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$west_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$west_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  seas <- "breeding"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_west_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_west_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$west_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$west_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$west_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$west_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  seas <- "nonbreeding"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_west_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_west_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$west_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$west_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$west_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$west_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  
  # coast fraction
  expr1 <- expression(x/y)
  seas <- "prebreeding_migration"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_coast_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_coast_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$coast_prop <- NA
  seas_sums$coast_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$coast_prop_se <- NA
  seas_sums$coast_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$coast_prop_lcl <- NA
  seas_sums$coast_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$coast_prop_ucl <- NA
  seas_sums$coast_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  seas <- "postbreeding_migration"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_coast_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_coast_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$coast_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$coast_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$coast_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$coast_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  seas <- "breeding"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_coast_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_coast_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$coast_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$coast_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$coast_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$coast_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  seas <- "nonbreeding"
  x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
         seas_sums$sum_migs_se[which(seas_sums$season==seas)])
  y <- c(seas_sums$pif_est_coast_sum[which(seas_sums$season==seas)], 
         seas_sums$pif_est_coast_sum_se[which(seas_sums$season==seas)])
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  seas_sums$coast_prop[which(seas_sums$season==seas)]<- res1$prop[1]
  seas_sums$coast_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
  seas_sums$coast_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
  seas_sums$coast_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
  
  # write fraction data
  write.csv(seas_sums, paste0("Z:/Migratory_Bird_Initiative/Western Landbirds/CO River Delta/R Work/output/", 
                              spp1, " fraction summary.csv"), 
            na="", row.names=F)
  
}
# ------------------------------------------------------------------------------




# run function -----------------------------------------------------------------
library(doParallel)

# make cluster
cl <- makeCluster(12)
registerDoParallel(cl)

# run function
loop_out1 <- foreach(i=1:length(spp_list), 
                     .packages=c("sf", "tidyr", "stringr", "dplyr", 
                                 "propagate", "ebirdst", "raster")) %dopar% {
                                   run_species(spp_list[i])
                                 }

# stop cluster
stopCluster(cl)
# ------------------------------------------------------------------------------




# fix grid cell ids for cv polys -----------------------------------------------
library(dplyr)
library(lubridate)
library(stringr)

# get data
dir1 <- paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\Draft Output\\cv4a")
setwd(dir1)
flist <- list.files(pattern="grid_proportions.csv")

# switch function
switch_ids <- function(x){
  ifelse(x==2, 1, ifelse(x==1, 2, x))
}

# loop through species
for(i in 1:length(flist)){
  # get data
  results <- read.csv(flist[i])
  results$cell_id <- switch_ids(results$cell_id)
  out <- arrange(results, week, cell_id)
  write.csv(out, flist[i], na="", row.names=F)
}
# ------------------------------------------------------------------------------




# fix grid cell ids for crd poly -----------------------------------------------
library(dplyr)
library(lubridate)
library(stringr)

# get data
dir1 <- paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\Draft Output\\crda")
setwd(dir1)
flist <- list.files(pattern="grid_proportions.csv")

# switch function
switch_ids <- function(x){
  ifelse(x==4, 6, ifelse(x==5, 7, ifelse(x==6, 4, ifelse(x==7, 5, x))))
}

# loop through species
for(i in 1:length(flist)){
  # get data
  results <- read.csv(flist[i])
  results$cell_id <- switch_ids(results$cell_id)
  out <- arrange(results, week, cell_id)
  write.csv(out, flist[i], na="", row.names=F)
}
# ------------------------------------------------------------------------------




# time series plots ------------------------------------------------------------
# set data
dir0 <- "cv4a"

# get data
dir1 <- paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\Draft Output\\", dir0)
setwd(dir1)
flist <- list.files(pattern="number_migrants.csv")

# loop through species
# i=100
for(i in 1:length(flist)){
  results <- read.csv(flist[i])
  results$week <- as.Date(results$week, origin = "1960-10-01")
  results$n_migs[is.na(results$n_migs)] <- 0
  results$n_migs[results$n_migs==Inf] <- 0
  spp1 <- stringr::str_sub(flist[i], end = -21)
  all_same <- !any(results$n_migs!=0)
  # test if any birds went through
  if(all_same==F){
    # seasons rectangles
    smig <- data.frame (
      xmin=results$week[first(which(results$season=="prebreeding_migration"))],
      xmax=results$week[last(which(results$season=="prebreeding_migration"))],
      ymin=-Inf, ymax=Inf)
    fmig <- data.frame (
      xmin=results$week[first(which(results$season=="postbreeding_migration"))],
      xmax=results$week[last(which(results$season=="postbreeding_migration"))],
      ymin=-Inf, ymax=Inf)
    grob <- grobTree(textGrob(spp1, x=0.02,  y=0.93, hjust=0,
                    gp=gpar(col="black", fontsize=11)))
    # time series plot
    ggplot() + 
      geom_rect(data=smig, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill="green", alpha=0.1, inherit.aes = FALSE) +
      geom_rect(data=fmig, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill="blue", alpha=0.1, inherit.aes = FALSE) +
      geom_ribbon(data=results, aes(x=week, ymin=(n_migs-(2*n_migs_se)),
           ymax=(n_migs+(2*n_migs_se))), fill="gray70") +
      geom_line(data=results, aes(x=week, y=n_migs), col="#619CFF") +
      xlab("Date") + ylab("Number of individuals") +
      scale_y_continuous(labels=scales::comma) +
      annotation_custom(grob)
    # save
    ggsave(paste0(dir1, "\\timeseries\\", spp1, " time series.pdf"), width=10, height=4,
           units="in")
  }
}
# ------------------------------------------------------------------------------

  


# correct the grid data --------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(scales)
library(RColorBrewer)
library(grid)
# set data
dir0 <- "crda"
foc_cell <- 2

# set up paths
dir1 <- paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\Draft Output\\", dir0)
setwd(dir1)
flist <- list.files(pattern="grid_proportions.csv")

# loop through species to get and clean data
for(i in 1:length(flist)){
  # get data
  grid_props <- read.csv(flist[i])
  grid_props$week <- as.Date(grid_props$week, origin = "1960-10-01")
  spp1 <- stringr::str_sub(flist[i], end = -21)
  # starting df
  gp1 <- c()
  # loop through cells
  for(k in 1:length(unique(grid_props$cell_id))){
    # get data
    df <- grid_props %>% filter(cell_id==k)
    # get indices for summer and winter midpoints
    summer_mid <- ceiling(mean(which(df$season=="breeding")))
    winter0 <- which(df$season=="nonbreeding")
    winter1 <- winter0[winter0<summer_mid]
    winter2 <- winter0[winter0>summer_mid]
    winter3 <- c(winter2, winter1)
    winter_mid <- winter3[ceiling(length(winter3)/2)]
    # get baseline as max from midpoints
    base1 <- max(df$cell_sum[c(summer_mid,winter_mid)])
    base1_se <- max(df$cell_sum_se[c(summer_mid,winter_mid)])
    # get adjusted cell count
    adj_cell_sum <- df$cell_sum - base1
    adj_cell_sum[adj_cell_sum<0] <- 0
    adj_cell_sum[is.na(adj_cell_sum)] <- 0
    adj_cell_sum[adj_cell_sum==Inf] <- 0
    adj_cell_sum_se <- sqrt(df$cell_sum_se^2 + base1_se^2)
    adj_cell_sum_se[is.na(adj_cell_sum_se)] <- 0
    adj_cell_sum_se[adj_cell_sum_se==Inf] <- 0
    adj_cell_sum_se[adj_cell_sum==0] <- 0
    # clean
    df <- df %>% select(cell_id, week, season) %>%
      mutate(adj_cell_sum, adj_cell_sum_se)
    gp1 <- rbind(gp1, df)
  }
  # get totals
  week_sums <- gp1 %>% arrange(week) %>%
    group_by(week) %>%
    summarise(adj_cell_sum_week=sum(adj_cell_sum),
              adj_cell_sum_week_se=sqrt(sum(adj_cell_sum_se^2)))
  gp2 <- left_join(gp1, week_sums, "week") %>% 
    arrange(week) %>% 
    filter(season %in% c("prebreeding_migration", 
                         "postbreeding_migration")) 
  gp3 <- gp2 %>% mutate(prop_week=adj_cell_sum/adj_cell_sum_week) %>%
    mutate(prop_week_se=sqrt((adj_cell_sum_se/adj_cell_sum)^2 + 
                (adj_cell_sum_week_se/adj_cell_sum_week)^2) * prop_week) %>%
    mutate(prop_week=ifelse(is.na(prop_week), 0, prop_week)) %>%
    mutate(prop_week_se=ifelse(prop_week==0, 0, prop_week_se))
  # write corrected file
  write.csv(gp3, paste("corrected", flist[i], sep="_"), row.names=F, na="")
}
# ------------------------------------------------------------------------------




# plot the grid data -----------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(scales)
library(RColorBrewer)
library(grid)
# set data
dir0 <- "crda"
foc_cell <- 3

# set up paths
dir1 <- paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\Draft Output\\", dir0)
setwd(dir1)
flist <- list.files(pattern="grid_proportions.csv")
flist <- flist[grep("corrected", flist)]

# loop through species to get and clean data
for(i in 1:length(flist)){
  # get data
  grid_props <- read.csv(flist[i])
  grid_props$week <- as.Date(grid_props$week, origin = "1960-10-01")
  grid_props$week2 <-  as.numeric(yday(grid_props$week))
  grid_props$lab <- as.character(format(grid_props$week, "%b %d"))
  spp1 <- stringr::str_sub(flist[i], start=11, end = -21)
  if(sum(grid_props$adj_cell_sum)==0) next()
  # seasons rectangles
  smig <- data.frame (
    xmin=grid_props$week2[first(which(grid_props$season=="prebreeding_migration"))],
    xmax=grid_props$week2[last(which(grid_props$season=="prebreeding_migration"))],
    ymin=-Inf, ymax=Inf)
  fmig <- data.frame (
    xmin=grid_props$week2[first(which(grid_props$season=="postbreeding_migration"))],
    xmax=grid_props$week2[last(which(grid_props$season=="postbreeding_migration"))],
    ymin=-Inf, ymax=Inf)
  grob <- grobTree(textGrob(spp1, x=0.95,  y=0.925, hjust=1,
                  gp=gpar(col="black", fontsize=11)))
  # make plot
  grid_props %>% as.data.frame() %>%
    mutate(week2=as.numeric(yday(week))) %>%
    ggplot() +
    geom_rect(data=smig, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="green", alpha=0.1, inherit.aes = FALSE) +
    geom_rect(data=fmig, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="blue", alpha=0.1, inherit.aes = FALSE) +
    geom_point(aes(y=cell_id, x=week2, col=prop_week, size=adj_cell_sum),
                          shape=16) +
    scale_color_distiller("Passage\nproportion", palette="Spectral") +
    scale_y_continuous(breaks=1:20) +
    scale_x_reverse(breaks=unique(grid_props$week2)[c(T,F)], 
                    labels=unique(grid_props$lab)[c(T,F)]) +
    scale_size_continuous(guide="none", range=c(1,6)) +
    geom_hline(yintercept=foc_cell-0.5, lty=2, col="gray80", size=0.5) +
    geom_hline(yintercept=foc_cell+0.5, lty=2, col="gray80", size=0.5) +
    xlab("Date") + ylab("Grid cell") +
    coord_flip() +
    annotation_custom(grob)
  # save it
  ggsave(paste0(dir1, "\\concentration\\", spp1, "grid plot.pdf"), width=10, height=4,
         units="in")
}
#-------------------------------------------------------------------------------
  



# make table s1 and fig 2 ------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(scales)
library(RColorBrewer)
# get data
dir1 <- paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\Draft Output\\crda")
setwd(dir1)
flist <- list.files(pattern="number_migrants.csv")

# loop through species
i=101
big_tab_seasons <- c()
big_tab_weeks <- c()
for(i in 1:length(flist[1:length(flist)])){
  results <- read.csv(flist[i])
  results$week <- as.Date(results$week, origin = "1960-10-01")
  spp1 <- stringr::str_sub(flist[i], end = -21)
  
  # test
  all_same <- !any(results$n_migs!=0)
  if(all_same==F){
    # get indices for summer and winter midpoints
    summer_mid <- ceiling(mean(which(results$season=="breeding")))
    winter0 <- which(results$season=="nonbreeding")
    winter1 <- winter0[winter0<summer_mid]
    winter2 <- winter0[winter0>summer_mid]
    winter3 <- c(winter2, winter1)
    winter_mid <- winter3[ceiling(length(winter3)/2)]
    
    # get baseline as max from midpoints
    base1 <- max(results$n_migs[c(summer_mid,winter_mid)])
    base1_se <- max(results$n_migs_se[c(summer_mid,winter_mid)])
    
    # get adjusted weekly count
    adj_wc <- results$n_migs - base1
    adj_wc[adj_wc<0] <- 0
    adj_wc_se <- sqrt(results$n_migs_se^2 + base1_se^2)
    adj_wc_se[adj_wc==0] <- 0
    wc_out <- data.frame(species=spp1, 
                         region=stringr::str_sub(dir1, start=-4),
                         week=results$week,
                         season=results$season,
                         adj_count=adj_wc,
                         adj_count_se=adj_wc_se)
    big_tab_weeks <- rbind(big_tab_weeks, wc_out)
    
    # get spring counts
    spring_counts <- results$n_migs[which(results$season=="prebreeding_migration")]
    sc_se <- results$n_migs_se[which(results$season=="prebreeding_migration")]
    
    # get spring aoc and max
    adj_sc <- spring_counts - base1
    adj_sc[adj_sc<0] <- 0
    adj_sc_se <- sqrt(sc_se^2 + base1_se^2)
    adj_sc_se[adj_sc==0] <- 0
    aoc_spring <- sum(adj_sc)
    aoc_spring_se <- sqrt(sum(adj_sc_se^2))
    max_spring <- max(adj_sc)
    max_spring_se <- mean(adj_sc_se[adj_sc==max(adj_sc)])
    
    # get fall counts
    fall_counts <- results$n_migs[which(results$season=="postbreeding_migration")]
    fc_se <- results$n_migs_se[which(results$season=="postbreeding_migration")]
    
    # get fall aoc
    adj_fc <- fall_counts - base1
    adj_fc[adj_fc<0] <- 0
    adj_fc_se <- sqrt(fc_se^2 + base1_se^2)
    adj_fc_se[adj_fc==0] <- 0
    aoc_fall <- sum(adj_fc)
    aoc_fall_se <- sqrt(sum(adj_fc_se^2))
    max_fall <- max(adj_fc)
    max_fall_se <- mean(adj_fc_se[adj_fc==max(adj_fc)])
    
    # get spring NA props
    expr1 <- expression(aoc1 / tot1)
    aoc1 <- c(aoc_spring, aoc_spring_se)
    tot1 <- c(mean(results$pif_est_na_sum),
               mean(results$pif_est_na_sum_se))
    df1 <- cbind(aoc1, tot1)
    res1 <- unlist(propagate(expr=expr1, data=df1, second.order=F,
                        do.sim=F))
    prop_na_spring <- round(res1$prop.Mean.1, 4)
    prop_na_spring_se <- round(res1$prop.sd.1, 4)
    
    # get fall NA props
    aoc1 <- c(aoc_fall, aoc_fall_se)
    tot1 <- c(mean(results$pif_est_na_sum),
               mean(results$pif_est_na_sum_se))
    df1 <- cbind(aoc1, tot1)
    res1 <- unlist(propagate(expr=expr1, data=df1, second.order=F,
                        do.sim=F))
    prop_na_fall <- round(res1$prop.Mean.1, 8)
    prop_na_fall_se <- round(res1$prop.sd.1, 8)
    
    # make df
    dout <- data.frame(species=spp1,
               region=stringr::str_sub(dir1, start=-4),
               max_count_spring=max_spring, 
               max_count_spring_se=max_spring_se,
               max_count_fall=max_fall, 
               max_count_fall_se=max_fall_se,
               tot_count_spring=aoc_spring, 
               tot_count_spring_se=aoc_spring_se,
               tot_count_fall=aoc_fall, 
               tot_count_fall_se=aoc_fall_se,
               tot_na_pop=mean(results$pif_est_na_sum), 
               tot_na_pop_se=mean(results$pif_est_na_sum_se),
               prop_na_spring, 
               prop_na_spring_se,
               prop_na_fall, 
               prop_na_fall_se)
    big_tab_seasons <- rbind(big_tab_seasons, dout)
    } else {next}
}

# save incremental outputs
# bts_crd <- big_tab_seasons
# btw_crd <- big_tab_weeks

# bts_cv1 <- big_tab_seasons
# btw_cv1 <- big_tab_weeks

# bts_cv2 <- big_tab_seasons
# btw_cv2 <- big_tab_weeks

# bts_cv3 <- big_tab_seasons
# btw_cv3 <- big_tab_weeks

# bts_cv4 <- big_tab_seasons
# btw_cv4 <- big_tab_weeks

# combine outputs and save
seas_sum <- rbind(bts_crd, bts_cv1, bts_cv2, bts_cv3, bts_cv4) %>%
  arrange(species, region)
week_sum <- rbind(btw_crd, btw_cv1, btw_cv2, btw_cv3, btw_cv4) %>%
  arrange(species, region, week)
write.csv(seas_sum, paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\R Work\\species_site_season_summary.csv"),
          row.names=F, na="")
write.csv(week_sum, paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\R Work\\species_site_week_summary.csv"),
          row.names=F, na="")

# time series plot
week_sum <- read.csv(paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\R Work\\species_site_week_summary.csv"))
names(week_sum)
d1 <- week_sum %>% 
  mutate(week=ymd(week)) %>%
  filter(week > "2018-02-01" &
         week < "2018-12-01") %>%
  group_by(region, week) %>% 
  summarise(sum_counts=sum(adj_count, na.rm=T),
            sum_counts_se=sqrt(sum(adj_count_se^2)))
d2 <- week_sum %>% 
  mutate(week=ymd(week)) %>%
  filter(week > "2018-02-01" &
         week < "2018-12-01") %>%
  mutate(sum_counts=adj_count, sum_count_se=adj_count_se)
ggplot() + 
  geom_ribbon(data=d1, aes(x=week, 
                  ymin=sum_counts-2*sum_counts_se,
                  ymax=sum_counts+2*sum_counts_se
                  ), alpha=0.25) +
  geom_line(data=d1, aes(x=week, y=sum_counts)) +
  facet_wrap(~region, scales="free") +
  scale_y_continuous(labels=scales::comma) +
  xlab("Week") + ylab("Number of migrants") +
  geom_line(data=d2, aes(x=week, y=sum_counts, col=species), alpha=0.6)  +
  guides(color="none") +
  scale_color_hue(l = 25, c = 50, h = c(200, 360))

# season summaries plot
seas_sum <- read.csv(paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\R Work\\species_site_season_summary.csv"))
names(seas_sum)
wide <- seas_sum %>% group_by(region) %>%
  summarise(spring_total=sum(tot_count_spring, na.rm=T),
            spring_tot_se=sqrt(sum(tot_count_spring_se^2, na.rm=T)),
            fall_total=sum(tot_count_fall, na.rm=T),
            fall_tot_se=sqrt(sum(tot_count_fall_se^2, na.rm=T)))
rbind(as.matrix(wide[1:5, c(2,3)]), as.matrix(wide[1:5, c(4,5)])) %>%
  as.data.frame() %>%
  mutate(region=factor(rep(wide$region, 2)),
         season=factor(rep(c("spring", "fall"), each=5))) %>%
  select(region, season, total_migrants=spring_total, 
         total_migrants_se=spring_tot_se) %>%
  ggplot(aes(x=region, fill=season)) + 
  geom_bar(aes(y=total_migrants), 
           stat="identity", position=position_dodge()) +
  geom_linerange(aes(ymin=total_migrants-2*total_migrants_se, 
                    ymax=total_migrants+2*total_migrants_se),
                position=position_dodge(width=0.9), lwd=1) +
  scale_y_continuous(labels=scales::comma) +
  scale_fill_brewer("Season", palette="Paired") +
  xlab("Region") + ylab("Total migrants")
# ------------------------------------------------------------------------------

    
  

# make table s2 and fig 5 ------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(scales)
library(RColorBrewer)
# get data
dir1 <- paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\Draft Output\\crda")
focal_cell <- 3 # 2 for CA and 3 for MX
cut_prop <- c(0.07, 0.08, 0.10, 0.13, 0.13)[5] # goes from N to S
setwd(dir1)
flist <- list.files(pattern="grid_proportions.csv")
flist <- flist[grep("corrected", flist)]

# loop through species
# i=23
big_prop_dat <- c()
for(i in 1:length(flist[1:length(flist)])){
  # get data
  results <- read.csv(flist[i])
  results$week <- as.Date(results$week, origin = "1960-10-01")
  spp1 <- stringr::str_sub(flist[i], start=11, end = -21)
  results <- results %>%
    filter(cell_id==focal_cell, season=="prebreeding_migration" | 
               season=="postbreeding_migration")
  # test
  all_same <- !any(results$adj_cell_sum!=0)
  if(all_same==F){
    dat1 <- data.frame(species=spp1, 
                       region=stringr::str_sub(dir1, start=-4),
                       week=results$week, season=results$season,
                       cell=results$cell_id,
               week_prop=results$prop_week, week_prop_se=results$prop_week_se) %>%
      mutate(times1=ifelse(week_prop>=cut_prop & week_prop<cut_prop*2, 1, 0)) %>%
      mutate(times2=ifelse(week_prop>=cut_prop*2 & week_prop<cut_prop*3, 1, 0)) %>%
      mutate(times3=ifelse(week_prop>=cut_prop*3, 1, 0)) %>%
      group_by(species, region, season) %>% 
      summarise(weeks_in_season=n(),
        times1=sum(times1, na.rm=T), 
                times2=sum(times2, na.rm=T),
                times3=sum(times3, na.rm=T)) 
    # stack data
    big_prop_dat <- rbind(big_prop_dat, dat1)
    } else {next}
}

# save incremental outputs
# bpd_cv1 <- big_prop_dat
# bpd_cv2 <- big_prop_dat
# bpd_cv3 <- big_prop_dat
# bpd_cv4 <- big_prop_dat
# bpd_crd <- big_prop_dat

# combine outputs and save
seas_sum <- rbind(bpd_crd, bpd_cv1, bpd_cv2, bpd_cv3, bpd_cv4) %>%
  arrange(species, region)
write.csv(seas_sum, paste0("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\",
               "CO River Delta\\R Work\\species_weeks_grid_props.csv"),
          row.names=F, na="")
# ------------------------------------------------------------------------------



  
# rerun the function -----------------------------------------------------------
setwd("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\CO River Delta\\Draft Output\\cv4a")
flist <- list.files(pattern="number_migrants.csv")
slist <- stringr::str_sub(flist, end = -21)
crashed <- ebirdst_runs[ebirdst_runs$common_name %in% spp_list[!spp_list %in% slist],] %>%
  filter(!is.na(breeding_start_dt)) %>% pull(common_name)
setwd("Z:\\Migratory_Bird_Initiative\\Western Landbirds\\CO River Delta\\R Work")

# make cluster
library(doParallel)
cl <- makeCluster(12)
registerDoParallel(cl)

# run function
loop_out1 <- foreach(i=1:length(crashed), 
                     .packages=c("sf", "tidyr", "stringr", "dplyr", 
                                 "propagate", "ebirdst", "raster")) %dopar% {
                                   run_species(crashed[i])
                                 }

# stop cluster
stopCluster(cl)
# ------------------------------------------------------------------------------




# walk through loop  -----------------------------------------------------------
spp1 <- "Yellow Warbler"
print(paste("starting species", spp1))

# get stem layers
sp_path <- ebirdst_download(species = spp1, force=T)
abd <- load_raster("abundance", path = sp_path)
lcl <- load_raster("abundance_lower", path = sp_path)
ucl <- load_raster("abundance_upper", path = sp_path)

# # pull population totals from collapse paper
# tot_n <- collapse_est$popest[which(collapse_est$species==spp1)]
# tot_n_lcl <- collapse_est$popestlci[which(collapse_est$species==spp1)]
# tot_n_ucl <- collapse_est$popestuci[which(collapse_est$species==spp1)]
# tot_n_se <- (tot_n_ucl - tot_n_lcl) / 3.93
# ann_trend <- collapse_est[,2][which(collapse_est$species==spp1)]

# get na population totals from summed PIF estimates
pif_bcr_est_na <- pif_bcr_est %>% filter(EnglishName==spp1)
pif_est_na_sum <- sum(pif_bcr_est_na$PopulationEstimate, na.rm=T)
pif_est_na_sum_se <- sqrt(sum(((pif_bcr_est_na$UCL - pif_bcr_est_na$LCL) / 
                                 3.93)^2)) # convert to se and then delta method

# get west population totals from summed PIF estimates
west_bcr <- as.character(c(4, 5, 9, 10, 15, 16, 32, 33))
pif_bcr_est_west <- pif_bcr_est_na %>% 
  mutate(BCR=as.character(BCR)) %>% filter(BCR %in% west_bcr)
pif_est_west_sum <- sum(pif_bcr_est_west$PopulationEstimate, na.rm=T)
pif_est_west_sum_se <- sqrt(sum(((pif_bcr_est_west$UCL - 
                                    pif_bcr_est_west$LCL) / 3.93)^2))

# get coast population totals from summed PIF estimates
coast_bcr <- c(5, 15, 32, 33)
pif_bcr_est_coast <- pif_bcr_est_na %>% 
  mutate(BCR=as.character(BCR)) %>% filter(BCR %in% coast_bcr)
pif_est_coast_sum <- sum(pif_bcr_est_coast$PopulationEstimate, na.rm=T)
pif_est_coast_sum_se <- sqrt(sum(((pif_bcr_est_coast$UCL - 
                                     pif_bcr_est_coast$LCL) / 3.93)^2))

# get seasons
sp_dates <- filter(ebirdst_runs, common_name == spp1) %>%
  select(setdiff(matches("(start)|(end)"), matches("year_round"))) %>%
  gather("label", "date") %>%
  separate(label, c("season", "start_end"), "_(?=s|e)") %>%
  spread(start_end, date) %>%
  select(season, start_dt, end_dt)
sp_dates <- mutate(sp_dates, pass = !(is.na(start_dt) | is.na(end_dt)))
weeks <- parse_raster_dates(abd)
weeks_season <- rep(NA_character_, length(weeks))
for (i in seq_len(nrow(sp_dates))) {
  s <- sp_dates[i, ]
  if (!s$pass) {
    next()
  }
  if (s$start_dt <= s$end_dt) {
    in_season <- weeks >= s$start_dt & weeks <= s$end_dt
  } else {
    in_season <- weeks >= s$start_dt | weeks <= s$end_dt
  }
  weeks_season[in_season] <- s$season
}
# table(weeks_season)

# drop weeks not assigned to season
week_pass1 <- !is.na(weeks_season)
abd <- abd[[which(week_pass1)]]
lcl <- lcl[[which(week_pass1)]]
ucl <- ucl[[which(week_pass1)]]
weeks1 <- weeks[week_pass1]
weeks_season1 <- weeks_season[week_pass1]
# table(weeks_season1)

# save breeding season layers
week_pass2 <- weeks_season1=="breeding"
abd_breed <- abd[[which(week_pass2)]]
lcl_breed <- lcl[[which(week_pass2)]]
ucl_breed <- ucl[[which(week_pass2)]]
weeks2 <- weeks1[week_pass2]
weeks_season2 <- weeks_season1[week_pass2]
# table(weeks_season2)

# keep all remaining weeks
week_pass3 <- weeks_season1=="prebreeding_migration" |
  weeks_season1=="postbreeding_migration" |
  weeks_season1=="breeding" |
  weeks_season1=="nonbreeding"
abd_mig <- abd[[which(week_pass3)]]
lcl_mig <- lcl[[which(week_pass3)]]
ucl_mig <- ucl[[which(week_pass3)]]
weeks3 <- weeks1[week_pass3]
weeks_season3 <- weeks_season1[week_pass3]
# table(weeks_season3)

# get breeding season mean and produce se layer from conf lims
var_breed <- abd_breed
for (l in 1:nlayers(abd_breed)){
  var_breed[[l]] <- ((ucl_breed[[l]] - lcl_breed[[l]]) / 2.56)^2
}
var_breed <- sum(var_breed, na.rm=T)
abd_breed <- mean(abd_breed, na.rm=T)
# plot(var_breed)
# plot(abd_breed)
# plot(st_geometry(na), add=T)

# get summed breeding index with standard error from quadrature for na
na_sum <- cellStats(abd_breed, sum, na.rm=T)
na_sum_se <- sqrt(cellStats(var_breed, sum, na.rm=T))

# make empty df and loop through weeks 
out_df <- c()
sums_df <- c()
# w=21
for(w in 1:length(weeks_season3)){
  # get ids
  week1 <- weeks3[w]
  seas1 <- weeks_season3[w]
  
  # pull maps
  lay1 <- abd_mig[[w]]
  lay2 <- ((ucl_mig[[w]] - lcl_mig[[w]]) / 2.56)^2
  
  # get focal sums and se
  focal_sum <- cellStats(mask(lay1, focal), sum, na.rm=T)
  focal_sum_se <- sqrt(cellStats(mask(lay2, focal), sum, na.rm=T)) # delta method
  
  # get grid cell info
  grid_grid_n <- crop(lay1, grid_extent)
  grid_grid_se <- crop(lay2, grid_extent)
  grid_grid_se[is.na(grid_grid_se)] <- 0
  grid_grid_area <- (grid_grid_n * 0) + 1
  cell_areas <- extract(grid_grid_area, st_zm(grid), sum, na.rm=T)
  cell_sums <- extract(grid_grid_n, st_zm(grid), sum, na.rm=T)
  cell_se_sums <- extract(grid_grid_se, st_zm(grid),
                          fun=function(x, ...) sqrt(sum(x)))
  sums_dfi <- data.frame(cell_id=1:length(cell_sums),
                         cell_area=cell_areas,
                         cell_sum=cell_sums,
                         cell_sum_se=cell_se_sums)
  sums_dfi$week <- week1
  sums_dfi$season <- seas1
  sums_df <- rbind(sums_df, sums_dfi)
  
  # get proportions
  expr1 <- expression(y/x)
  x <- c(na_sum, na_sum_se)
  y <- c(focal_sum, focal_sum_se)
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data = df1, second.order=F,
                    do.sim=F)
  na_prop <- res1$prop[1]
  if(is.na(na_prop)) na_prop <- 0
  na_prop_se <- res1$prop[3]
  if(is.na(na_prop_se)) na_prop_se <- 0
  
  # make row
  out <- data.frame(species=spp1, #collapse_ann_trend=ann_trend,
                    pif_est_na_sum, pif_est_na_sum_se,
                    pif_est_west_sum, pif_est_west_sum_se, 
                    pif_est_coast_sum, pif_est_coast_sum_se,
                    season=seas1, week=week1) 
  
  # add n_migs
  expr1 <- expression(y * x)
  x <- c(na_prop, na_prop_se)
  y <- c(pif_est_na_sum, pif_est_na_sum_se)
  df1 <- cbind(x, y)
  res1 <- propagate(expr=expr1, data=df1, second.order=F,
                    do.sim=F)
  out$n_migs <- res1$prop[1]
  if(is.na(out$n_migs)) out$n_migs <- 0
  out$n_migs_se <- res1$prop[3]
  if(is.na(out$n_migs_se)) out$n_migs_se <- 0
  
  # add row
  out_df <- rbind(out_df, out)
}

# clean and stack and save
results <- out_df %>% as.data.frame()
write.csv(results, paste0("Z:/Migratory_Bird_Initiative/Western Landbirds/CO River Delta/R Work/output/", 
                          spp1, " number_migrants.csv"), na="", 
          row.names=F)
grid_props <- sums_df %>% as.data.frame()
gp1 <- grid_props %>% 
  group_by(week) %>%
  summarise(cell_area_sum=sum(cell_area),
            cell_sum_sum=sum(cell_sum),
            cell_sum_sum_se=sqrt(sum(cell_sum_se^2)))
grid_props <- grid_props %>% left_join(gp1)
write.csv(grid_props, paste0("Z:/Migratory_Bird_Initiative/Western Landbirds/CO River Delta/R Work/output/",  
                             spp1, " grid_proportions.csv"), na="", 
          row.names=F)

# fraction data
# head(results)
# names(results)
seas_sums <- results %>% 
  group_by(species, #collapse_ann_trend,
           pif_est_na_sum, pif_est_na_sum_se,
           pif_est_west_sum, pif_est_west_sum_se,
           pif_est_coast_sum, pif_est_coast_sum_se,
           season) %>%
  summarise(sum_migs=sum(n_migs),
            sum_migs_se=sqrt(sum(n_migs_se^2)))

# get fractions
# na fraction
expr1 <- expression(x/y)
seas <- "prebreeding_migration"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_na_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_na_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$na_prop <- NA
seas_sums$na_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$na_prop_se <- NA
seas_sums$na_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$na_prop_lcl <- NA
seas_sums$na_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$na_prop_ucl <- NA
seas_sums$na_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
seas <- "postbreeding_migration"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_na_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_na_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$na_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$na_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$na_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$na_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
seas <- "breeding"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_na_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_na_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$na_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$na_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$na_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$na_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
seas <- "nonbreeding"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_na_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_na_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$na_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$na_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$na_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$na_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]

# west fraction
expr1 <- expression(x/y)
seas <- "prebreeding_migration"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_west_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_west_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$west_prop <- NA
seas_sums$west_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$west_prop_se <- NA
seas_sums$west_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$west_prop_lcl <- NA
seas_sums$west_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$west_prop_ucl <- NA
seas_sums$west_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
seas <- "postbreeding_migration"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_west_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_west_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$west_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$west_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$west_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$west_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
seas <- "breeding"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_west_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_west_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$west_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$west_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$west_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$west_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
seas <- "nonbreeding"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_west_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_west_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$west_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$west_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$west_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$west_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]

# coast fraction
expr1 <- expression(x/y)
seas <- "prebreeding_migration"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_coast_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_coast_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$coast_prop <- NA
seas_sums$coast_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$coast_prop_se <- NA
seas_sums$coast_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$coast_prop_lcl <- NA
seas_sums$coast_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$coast_prop_ucl <- NA
seas_sums$coast_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
seas <- "postbreeding_migration"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_coast_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_coast_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$coast_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$coast_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$coast_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$coast_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
seas <- "breeding"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_coast_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_coast_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$coast_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$coast_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$coast_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$coast_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]
seas <- "nonbreeding"
x <- c(seas_sums$sum_migs[which(seas_sums$season==seas)], 
       seas_sums$sum_migs_se[which(seas_sums$season==seas)])
y <- c(seas_sums$pif_est_coast_sum[which(seas_sums$season==seas)], 
       seas_sums$pif_est_coast_sum_se[which(seas_sums$season==seas)])
df1 <- cbind(x, y)
res1 <- propagate(expr=expr1, data = df1, second.order=F,
                  do.sim=F)
seas_sums$coast_prop[which(seas_sums$season==seas)]<- res1$prop[1]
seas_sums$coast_prop_se[which(seas_sums$season==seas)] <- res1$prop[3]
seas_sums$coast_prop_lcl[which(seas_sums$season==seas)] <- res1$prop[5]
seas_sums$coast_prop_ucl[which(seas_sums$season==seas)] <- res1$prop[6]

# write fraction data
write.csv(seas_sums, paste0("Z:/Migratory_Bird_Initiative/Western Landbirds/CO River Delta/R Work/output/", 
                            spp1, " fraction summary.csv"), 
          na="", row.names=F)

# ------------------------------------------------------------------------------




    