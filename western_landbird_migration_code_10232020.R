# this is R code put together by Tim Meehan and Bill DeLuca to do analyses
# related to William V. DeLuca, Tim Meehan, Nat Seavy, Andrea Jones, Jennifer Pitt, J
# Jill L. Deppe, and Chad B. Wilsey. 2020. The Colorado River Delta and California’s 
# Central Valley are critical regions for many migrating North American landbirds.
# Ornithological Applications.

# setup ------------------------------------------------------------------------
# set up a bunch of stuff. load libraries, set options, define spatial 
# reference systems, etc.

# libraries
library(RColorBrewer)
library(colorspace)
library(grid)
library(doParallel)
library(sf)
library(ggplot2)
library(rasterVis)
library(tidyr)
library(lubridate)
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

# time series plot function 
theme_timeseries <- function (base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(panel.background = element_rect(fill = "white", colour = NA),
          panel.border = element_rect(fill = NA, colour = "grey20"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = rel(0.9), color="gray10", 
                                     angle = 0),
          axis.text.y = element_text(size = rel(0.9), color="gray10", 
                                     angle = 0),
          strip.background = element_rect(fill = "grey80"),
          legend.key = element_rect(fill = "white", colour = NA),
          plot.title = element_text(size=14, hjust = 0.5,
                                    margin=margin(t=5, b=10)),
          legend.position="right",
          complete = TRUE)
}; theme_set(theme_timeseries())

# working directory
setwd("~/GitHub/western_landbird_migration")

# crs's
ebird_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
epsg102008 <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# ------------------------------------------------------------------------------




# get maps and extra data ------------------------------------------------------
# get shapefiles of continent and study focal area and latitudinal grid for
# grid proportions. and load continental population size estimates.

# focal areas
focal <- read_sf("./data/focal_cv4.shp") %>%
  mutate(FID=0) %>% dplyr::select(FID) %>% st_transform(ebird_crs)
na <- read_sf("./data/na.shp") %>% st_transform(ebird_crs)
grid <- read_sf("./data/cv4tran.shp") %>% 
  mutate(FID=c(1,0,2,3,4,5,6,7)) %>% 
  arrange(FID) %>% select(FID) %>% 
  st_transform(ebird_crs)
grid_cell <- grid[2,]
grid_extent <- as(st_buffer(st_combine(grid), 0.01), "Spatial")
grid_extent <- as(grid_extent, "sf")

# plot it all for QAQC
plot(st_geometry(st_transform(na, epsg102008)), border="#619CFF", 
     lwd=2)
plot(st_geometry(st_transform(grid, epsg102008)), add=T, border="gray60", 
     lwd=2)
plot(st_geometry(st_transform(grid_cell, epsg102008)), add=T, border="green", 
     lwd=2)
plot(st_geometry(st_transform(grid_extent, epsg102008)), add=T, border="gray80", 
     lwd=2)
plot(st_geometry(st_transform(focal, epsg102008)), add=T, border="black", 
     lwd=2)

# get continental pop size data
collapse_est <- read.csv("./data/bird_decline.csv")
# ------------------------------------------------------------------------------

                               


# get spp data -----------------------------------------------------------------
# make species list, here we select two species as examples
# View(ebirdst_runs)
spp_list <- c("Yellow Warbler", "Western Tanager")

# make sure that there are ebird layers for these species
spp_list %in% ebirdst_runs$common_name
# ------------------------------------------------------------------------------




# this function does most of the work ------------------------------------------
# it is the basis for figs 2 - 5 and tables s1, s2, quantifying total individuals and proportions
# source this function and then run it in a parallel loop below for multiple
# species.
run_species <- function(spp1="Western Tanager"){
  print(paste("starting species", spp1))
  
  # get ebird status and trends abundance layers using the ebirdst package
  sp_path <- ebirdst_download(species = spp1, force=F)
  abd <- load_raster("abundance", path = sp_path)
  lcl <- load_raster("abundance_lower", path = sp_path)
  ucl <- load_raster("abundance_upper", path = sp_path)

  # get continental population totals from PIF estimates
  pif_bcr_est_na <- collapse_est %>% filter(species==spp1)
  pif_est_na_sum <- pif_bcr_est_na$popest
  pif_est_na_sum_se <- sqrt(sum(((pif_bcr_est_na$popestuci - 
                                    pif_bcr_est_na$popestlci) / 3.93)^2))

  # define weeks and seasons for ebird layers
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
  
  # drop weeks not assigned to a season
  week_pass1 <- !is.na(weeks_season)
  abd <- abd[[which(week_pass1)]]
  lcl <- lcl[[which(week_pass1)]]
  ucl <- ucl[[which(week_pass1)]]
  weeks1 <- weeks[week_pass1]
  weeks_season1 <- weeks_season[week_pass1]
  
  # keep breeding season layers that will go into the breeding season average 
  # relative abundance layer
  week_pass2 <- weeks_season1=="breeding"
  abd_breed <- abd[[which(week_pass2)]]
  lcl_breed <- lcl[[which(week_pass2)]]
  ucl_breed <- ucl[[which(week_pass2)]]
  weeks2 <- weeks1[week_pass2]
  weeks_season2 <- weeks_season1[week_pass2]
  
  # keep all weeks for weekly abundance estimates
  week_pass3 <- weeks_season1=="prebreeding_migration" |
    weeks_season1=="postbreeding_migration" |
    weeks_season1=="breeding" |
    weeks_season1=="nonbreeding"
  abd_mig <- abd[[which(week_pass3)]]
  lcl_mig <- lcl[[which(week_pass3)]]
  ucl_mig <- ucl[[which(week_pass3)]]
  weeks3 <- weeks1[week_pass3]
  weeks_season3 <- weeks_season1[week_pass3]
  
  # preprocess the breeding abundance stacks to save memory and comp time
  # crop abd breed
  ra <- stack(crop(abd_breed[[1]], na))
  for(i in 2:nlayers(abd_breed)){
    ra[[i]] <- crop(abd_breed[[i]], na)
    print(paste("cropped layer abd_breed", i))
  }
  abd_breed <- ra; rm(ra); gc()
  
  # crop lcl breed
  rl <- stack(crop(lcl_breed[[1]], na))
  for(i in 2:nlayers(lcl_breed)){
    rl[[i]] <- crop(lcl_breed[[i]], na)
    print(paste("cropped layer lcl_breed", i))
  }
  lcl_breed <- rl; rm(rl); gc()
  
  # crop ucl breed
  ru <- stack(crop(ucl_breed[[1]], na))
  for(i in 2:nlayers(ucl_breed)){
    ru[[i]] <- crop(ucl_breed[[i]], na)
    print(paste("cropped layer ucl_breed", i))
  }
  ucl_breed <- ru; rm(ru); gc()
  
  # get breeding season mean and produce se layer from 80% confidence limits
  var_breed <- abd_breed
  for (l in 1:nlayers(abd_breed)){
    var_breed[[l]] <- ((ucl_breed[[l]] - lcl_breed[[l]]) / 2.56)^2
  }
  var_breed <- sum(var_breed, na.rm=T)
  abd_breed <- mean(abd_breed, na.rm=T)
  
  # get summed breeding index with standard error for na
  na_sum <- cellStats(mask(abd_breed, na), sum, na.rm=T)
  na_sum_se <- sqrt(cellStats(mask(var_breed, na), sum, na.rm=T))
  rm(abd_breed, var_breed, lcl_breed, ucl_breed); gc()
  
  # preprocess the weekly abundance stacks to save memory and comp time
  # crop abd mig
  ra <- stack(crop(abd_mig[[1]], na))
  for(i in 2:nlayers(abd_mig)){
    ra[[i]] <- crop(abd_mig[[i]], na)
    print(paste("cropped layer abd_mig", i))
  }
  abd_mig <- ra; rm(ra); gc()
  
  # crop lcl mig
  rl <- stack(crop(lcl_mig[[1]], na))
  for(i in 2:nlayers(lcl_mig)){
    rl[[i]] <- crop(lcl_mig[[i]], na)
    print(paste("cropped layer lcl_mig", i))
    rm(li); gc()
  }
  lcl_mig <- rl; rm(rl); gc()
  
  # crop ucl mig
  ru <- stack(crop(ucl_mig[[1]], na))
  for(i in 2:nlayers(ucl_mig)){
    ru[[i]] <- crop(ucl_mig[[i]], na)
    print(paste("cropped layer ucl_mig", i))
  }
  ucl_mig <- ru; rm(ru); gc()
  
  # now that we have a breeding season summed relative abundance, we loop
  # through each weekly abundance layer, extract the relative abundance in the 
  # focal area, divide that focal summed relative abundance by breeding season 
  # summed relative abundance, and then multiply the result by the total 
  # population size estimate to get number of birds in the focal area in a week.
  
  # make empty df and loop through weeks 
  out_df <- c()
  sums_df <- c()
  for(w in 1:length(weeks_season3)){
    # get ids
    week1 <- weeks3[w]
    seas1 <- weeks_season3[w]
    
    # pull appropriate weekly maps
    lay1 <- abd_mig[[w]]
    lay2 <- ((ucl_mig[[w]] - lcl_mig[[w]]) / 2.56)^2
    
    # get focal sums and se
    focal_sum <- cellStats(mask(lay1, focal), sum, na.rm=T)
    focal_sum_se <- sqrt(cellStats(mask(lay2, focal), sum, na.rm=T)) 
    
    # get focal versus continental proportions
    expr1 <- expression(y/x)
    x <- c(na_sum, na_sum_se)
    y <- c(focal_sum, focal_sum_se)
    df1 <- cbind(x, y)
    res1 <- propagate(expr=expr1, data = df1, second.order=F,
                      do.sim=F)
    na_prop <- res1$prop[1]; if(is.na(na_prop)) na_prop <- 0
    na_prop_se <- res1$prop[3]; if(is.na(na_prop_se)) na_prop_se <- 0
    
    # make row for output
    out <- data.frame(species=spp1,
                      pif_est_na_sum, pif_est_na_sum_se,
                      season=seas1, week=week1) 
    
    # calculate and add the number of birds in the focal area
    expr1 <- expression(y * x)
    x <- c(na_prop, na_prop_se)
    y <- c(pif_est_na_sum, pif_est_na_sum_se)
    df1 <- cbind(x, y)
    res1 <- propagate(expr=expr1, data=df1, second.order=F,
                      do.sim=F)
    out$n_migs <- res1$prop[1]; if(is.na(out$n_migs)) out$n_migs <- 0
    out$n_migs_se <- res1$prop[3]; if(is.na(out$n_migs_se)) out$n_migs_se <- 0
    
    # add row to the output data
    out_df <- rbind(out_df, out)
    
    # get grid cell info. here we get the relative abundance in grid cells
    # along a llatitudinal transect. these numbers are for all birds and are 
    # corrected for residents at a later stage.
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
    
    # add row to output data
    sums_df <- rbind(sums_df, sums_dfi)
    print(paste("finished week", w, "of 52"))
  }
  
  # clean and stack and save the two resulting intermediate data sets
  results <- out_df %>% as.data.frame()
  write.csv(results, paste0("./output/", spp1, " number migrants.csv"), na="", 
            row.names=F)
  grid_props <- sums_df %>% as.data.frame()
  gp1 <- grid_props %>% 
    group_by(week) %>%
    summarise(cell_area_sum=sum(cell_area),
              cell_sum_sum=sum(cell_sum),
              cell_sum_sum_se=sqrt(sum(cell_sum_se^2)))
  grid_props <- grid_props %>% left_join(gp1)
  write.csv(grid_props, paste0("./output/", spp1, " grid proportions.csv"), 
            na="", 
            row.names=F)
}
# ------------------------------------------------------------------------------




# run the work horse function in parallel --------------------------------------
# make cluster
cl <- makeCluster(6)
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




# time series plots ------------------------------------------------------------
# make time series plots per species (figure S3) to show how many birds are at the 
# focal area at any given time. note that these are all birds, migrants and 
# residents.
setwd("./output")
flist <- list.files(pattern="number migrants.csv")

# loop through species
for(i in 1:length(flist)){
  results <- read.csv(flist[i])
  results$week <- as.Date(results$week, origin = "1960-10-01")
  results$n_migs[is.na(results$n_migs)] <- 0
  results$n_migs[results$n_migs==Inf] <- 0
  spp1 <- stringr::str_sub(flist[i], end = -21)
  all_same <- !any(results$n_migs!=0)
  # test that any birds went through
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
    ggsave(paste0(spp1, " time series.pdf"), width=10, height=4,
           units="in")
  }
}
# ------------------------------------------------------------------------------

  


# correct the grid data --------------------------------------------------------
# In order to account for breeders and winter residents, and ensure
# relative abundances represented migrants, we came up with a 
# way to subtract off potential residents so that the grid cell proportions
# represented migrating birds and proportions. conceptually represented in fig S1.
# .csv output is the basis for fig 4.

#for an example focal cell
foc_cell <- 2
flist <- list.files(pattern="grid proportions.csv")

# loop through species to get and clean data
for(i in 1:length(flist)){
  # get data
  grid_props <- read.csv(flist[i])
  grid_props$week <- as.Date(grid_props$week, origin = "1960-10-01")
  spp1 <- stringr::str_sub(flist[i], end = -22)
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
  write.csv(gp3, paste("resident corrected", flist[i], sep=" "), row.names=F, 
            na="")
}
# ------------------------------------------------------------------------------




# plot the grid data -----------------------------------------------------------
# create the grid plots shown in fig. 4 fig S4 of the manuscript.
# identify the cell with the focal area
foc_cell <- 2

# get file paths
flist <- list.files(pattern="grid proportions.csv")
flist <- flist[grep("corrected", flist)]

# loop through species to get and clean data and make plots
for(i in 1:length(flist)){
  # get data
  grid_props <- read.csv(flist[i])
  grid_props$week <- as.Date(grid_props$week, origin = "1960-10-01")
  grid_props$week2 <-  as.numeric(yday(grid_props$week))
  grid_props$lab <- as.character(format(grid_props$week, "%b %d"))
  spp1 <- stringr::str_sub(flist[i], start=20, end = -22)
  if(sum(grid_props$adj_cell_sum)==0) next()
  # seasons rectangles
  smig <- data.frame (
    xmin=grid_props$week2[first(which(
      grid_props$season=="prebreeding_migration"))],
    xmax=grid_props$week2[last(which(
      grid_props$season=="prebreeding_migration"))],
    ymin=-Inf, ymax=Inf)
  fmig <- data.frame (
    xmin=grid_props$week2[first(which(
      grid_props$season=="postbreeding_migration"))],
    xmax=grid_props$week2[last(which(
      grid_props$season=="postbreeding_migration"))],
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
  ggsave(paste0(spp1, " grid plot.pdf"), width=10, height=4,
         units="in")
}
#-------------------------------------------------------------------------------
  



# make more summary figures and tables -----------------------------------------
# this pile of code grabs the weekly abundance estimates, subtracts away the 
# possible residents, estimates the total number of migrants going through
# during migration (area under histogram), and estimates the proportion of the 
# total continental populations that use the region during migration used in fig 3.

# get file paths
flist <- list.files(pattern="number migrants.csv")

# loop through species
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
                         region="focal_area",
                         week=results$week,
                         season=results$season,
                         adj_count=adj_wc,
                         adj_count_se=adj_wc_se)
    big_tab_weeks <- rbind(big_tab_weeks, wc_out)
    # get spring counts
    spring_counts <- results$n_migs[which(
      results$season=="prebreeding_migration")]
    sc_se <- results$n_migs_se[which(
      results$season=="prebreeding_migration")]
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
    fall_counts <- results$n_migs[which(
      results$season=="postbreeding_migration")]
    fc_se <- results$n_migs_se[which(
      results$season=="postbreeding_migration")]
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
               region="focal_region",
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

# get outputs and save
seas_sum <- big_tab_seasons %>% arrange(species, region)
week_sum <- big_tab_weeks %>% arrange(species, region, week)
write.csv(seas_sum, "species season summary.csv",
          row.names=F, na="")
write.csv(week_sum, "species week summary.csv",
          row.names=F, na="")

# time series plot with all species and the total summed migrants
# similar to fig 2, but here we show total migrants by week with se
# and a line for each individual species without se, uniquely colored.
names(week_sum)
d1 <- week_sum %>% 
  mutate(week=ymd(week)) %>%
  filter(week > "2018-02-01" &
         week < "2018-12-01") %>%
  group_by(week) %>% 
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
  scale_y_continuous(labels=scales::comma) +
  xlab("Week") + ylab("Number of migrants") +
  geom_line(data=d2, aes(x=week, y=sum_counts, col=species), alpha=0.6)  +
  guides(color="none") +
  scale_color_hue(l = 25, c = 50, h = c(200, 360))

# season summaries plot
names(seas_sum)
wide <- seas_sum %>% group_by(region) %>%
  summarise(spring_total=sum(tot_count_spring, na.rm=T),
            spring_tot_se=sqrt(sum(tot_count_spring_se^2, na.rm=T)),
            fall_total=sum(tot_count_fall, na.rm=T),
            fall_tot_se=sqrt(sum(tot_count_fall_se^2, na.rm=T)))
rbind(as.matrix(wide[1, c(2,3)]), as.matrix(wide[1, c(4,5)])) %>%
  as.data.frame() %>%
  mutate(region=factor(rep(wide$region, 2)),
         season=factor(rep(c("spring", "fall"), each=1))) %>%
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

    
  

# make more summary tables and figures -----------------------------------------
# this code calculates the number of weeks where the focal grid cell proportion
# exceeds a null proportion (0.13) that represents even use of all cells used in fig 5.
focal_cell <- 2 
cut_prop <- 0.13
flist <- list.files(pattern="grid proportions.csv")
flist <- flist[grep("corrected", flist)]

# loop through species
big_prop_dat <- c()
for(i in 1:length(flist[1:length(flist)])){
  # get data
  results <- read.csv(flist[i])
  results$week <- as.Date(results$week, origin = "1960-10-01")
  spp1 <- stringr::str_sub(flist[i], start=20, end = -22)
  results <- results %>%
    filter(cell_id==focal_cell, season=="prebreeding_migration" | 
               season=="postbreeding_migration")
  # test
  all_same <- !any(results$adj_cell_sum!=0)
  if(all_same==F){
    dat1 <- data.frame(species=spp1, 
                       region="focal_region",
                       week=results$week, season=results$season,
                       cell=results$cell_id,
               week_prop=results$prop_week, 
               week_prop_se=results$prop_week_se) %>%
      mutate(times1=ifelse(week_prop>=cut_prop & 
                             week_prop<cut_prop*2, 1, 0)) %>%
      mutate(times2=ifelse(week_prop>=cut_prop*2 & 
                             week_prop<cut_prop*3, 1, 0)) %>%
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

# get outputs and save
seas_sum <- big_prop_dat %>%
  arrange(species, region)
write.csv(seas_sum, "species seasons weeks prop factors.csv",
          row.names=F, na="")
# ------------------------------------------------------------------------------

 

   
