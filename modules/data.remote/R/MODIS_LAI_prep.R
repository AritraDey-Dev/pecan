#' Prepare MODIS LAI data for the SDA workflow.
#'
#' @param site_info list: Bety list of site info including site_id, lon, and lat.
#' @param time_points character: a vector contains each time point within the start and end date.
#' @param outdir character: where the final CSV file will be stored.
#' @param search_window numeric: search window for locate available LAI values.
#' @param export_csv boolean: decide if we want to export the CSV file.
#' @param sd_threshold numeric: for filtering out any estimations with unrealistic high standard error, default is 20. The QC check will be skipped if it's set as NULL.
#' @param skip.download boolean: determine if we want to use existing LAI.csv file and skip the MODIS LAI download part.
#'
#' @return A data frame containing LAI and sd for each site and each time step.
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
MODIS_LAI_prep <- function(site_info, time_points, outdir = NULL, search_window = 30, export_csv = FALSE, sd_threshold = 20, skip.download = TRUE){
  #initialize future parallel computation.
  if (future::supportsMulticore()) {
    future::plan(future::multicore, workers = 10)
  } else {
    future::plan(future::multisession, workers = 10) #10 is the maximum number of requests permitted for the MODIS server.
  }
  #if we export CSV but didn't provide any path
  if(as.logical(export_csv) && is.null(outdir)){
    PEcAn.logger::logger.info("If you want to export CSV file, please ensure input the outdir!")
    return(0)
  }
  #convert time points into paired start and end dates.
  start.end.dates <- data.frame()
  for (i in seq_along(time_points)) {
    start.end.dates <- rbind(start.end.dates,
                             list(start_date = as.character(time_points[i] - lubridate::days(search_window)),
                                  end_date = as.character(time_points[i] + lubridate::days(search_window))))
    
  }
  #grab previous data to see which site has incomplete observations, if so, download the site for the whole time period.
  #if we have previous downloaded CSV file
  if(file.exists(file.path(outdir, "LAI.csv"))){
    PEcAn.logger::logger.info("Extracting previous LAI file!")
    Previous_CSV <- utils::read.csv(file.path(outdir, "LAI.csv"), 
                                    colClasses = c(rep("character", 2), rep("numeric", 4), "character"))
    if (!is.null(sd_threshold)) {
      PEcAn.logger::logger.info("filtering out records with high standard errors!")
      ind.rm <- which(Previous_CSV$sd >= sd_threshold)
      if (length(ind.rm) > 0) {
        Previous_CSV <- Previous_CSV[-ind.rm,]
      }
    }
    LAI_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_LAI"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, LAI, std, target time point.
    LAI_Output$site_id <- site_info$site_id
    #Calculate LAI for each time step and site.
    #loop over time and site
    LAI.list <- time_points %>% furrr::future_map(function(t){
      out.t <- data.frame()
      for (id in site_info$site_id) {
        site_LAI <- Previous_CSV[which(Previous_CSV$site_id == id),]
        site_LAI$sd[which(site_LAI$sd<=0.66)] <- 0.66
        diff_days <- abs(lubridate::days(lubridate::date(site_LAI$date)-lubridate::date(t))@day)
        if(any(diff_days <= search_window)){#data found
          out.t <- rbind(out.t, list(mean = site_LAI$lai[which.min(diff_days)], sd = site_LAI$sd[which.min(diff_days)]))
        } else {
          out.t <- rbind(out.t, list(mean = NA, sd = NA))
        }
      }
      out.t %>% purrr::set_names(c(paste0(t, "_LAI"), paste0(t, "_SD")))
    }, .progress = T)
    for (i in seq_along(time_points)) {
      t <- time_points[i]#otherwise the t will be number instead of date.
      LAI_Output[, paste0(t, "_LAI")] <- LAI.list[[i]][,paste0(t, "_LAI")]
      LAI_Output[, paste0(t, "_SD")] <- LAI.list[[i]][,paste0(t, "_SD")]
    }
  }else{#we don't have any previous downloaded CSV file.
    LAI_Output <- matrix(NA, length(site_info$site_id), 2*length(time_points)+1) %>% 
      `colnames<-`(c("site_id", paste0(time_points, "_LAI"), paste0(time_points, "_SD"))) %>% as.data.frame()#we need: site_id, LAI, std, target time point.
    LAI_Output$site_id <- site_info$site_id
  }
  #only Site that has NA for any time points need to be downloaded.
  new_site_info <- site_info %>% purrr::map(function(x)x[!stats::complete.cases(LAI_Output)])
  #TODO: only download data for specific date when we have missing data.
  if(length(new_site_info$site_id) != 0 && !skip.download){
    product <- "MCD15A3H"
    PEcAn.logger::logger.info("Extracting LAI mean products!")
    lai_mean <- split(as.data.frame(new_site_info), seq(nrow(as.data.frame(new_site_info)))) %>% 
      furrr::future_map(function(s){
        split(as.data.frame(start.end.dates), seq(nrow(as.data.frame(start.end.dates)))) %>% 
          purrr::map(function(dates){
            if (! "try-error" %in% class(try(mean <- MODISTools::mt_subset(product = product,
                                                                           lat = s$lat,
                                                                           lon = s$lon,
                                                                           band = "Lai_500m",
                                                                           start = dates$start_date,
                                                                           end = dates$end_date,
                                                                           progress = FALSE)))) {
              return(list(mean = mean$value, date = mean$calendar_date))
            } else {
              return(NA)
            }
          }) %>% dplyr::bind_rows()
      }, .progress = T)
    PEcAn.logger::logger.info("Extracting LAI std products!")
    lai_std <- split(as.data.frame(new_site_info), seq(nrow(as.data.frame(new_site_info)))) %>% 
      furrr::future_map(function(s){
        split(as.data.frame(start.end.dates), seq(nrow(as.data.frame(start.end.dates)))) %>% 
          purrr::map(function(dates){
            if (! "try-error" %in% class(try(std <- MODISTools::mt_subset(product = product,
                                                                          lat = s$lat,
                                                                          lon = s$lon,
                                                                          band = "LaiStdDev_500m",
                                                                          start = dates$start_date,
                                                                          end = dates$end_date,
                                                                          progress = FALSE)))) {
              return(std$value)
            } else {
              return(NA)
            }
          }) %>% unlist %>% purrr::set_names(NULL)
      }, .progress = T)
    PEcAn.logger::logger.info("Extracting LAI qc products!")
    lai_qc <- split(as.data.frame(new_site_info), seq(nrow(as.data.frame(new_site_info)))) %>% 
      furrr::future_map(function(s){
        split(as.data.frame(start.end.dates), seq(nrow(as.data.frame(start.end.dates)))) %>% 
          purrr::map(function(dates){
            if (! "try-error" %in% class(try(qc <- MODISTools::mt_subset(product = product,
                                                                         lat = s$lat,
                                                                         lon = s$lon,
                                                                         band = "FparLai_QC",
                                                                         start = dates$start_date,
                                                                         end = dates$end_date,
                                                                         progress = FALSE)))) {
              qc$value %>% purrr::map(function(v){
                qc_flag <- intToBits(as.integer(v)) # NB big-endian (ones place first)
                qc_flag <- as.integer(rev(utils::head(qc_flag, 3))) # now ones place last
                paste(qc_flag, collapse = "")
              })
            } else {
              return(NA)
            }
          }) %>% unlist %>% purrr::set_names(NULL)
      }, .progress = T)
    # LAI <- data.frame(matrix(NA, 0, 6)) %>% `colnames<-`(c("date", "site_id", "lat", "lon", "lai", "sd"))
    LAI <- data.frame()
    for (i in seq_along(lai_std)) {
      for (j in seq_along(lai_std[[i]])) {
        # skip pixels with NA observation.
        if (is.na(lai_std[[i]][j])) {
          next
        }
        # skip bad pixels based on qc band.
        if (! lai_qc[[i]][j] %in% c("000", "001")) {
          next
        }
        if (!is.null(sd_threshold)) {
          if (lai_std[[i]][j] >= sd_threshold) {
            next
          }
        }
        LAI <- rbind(LAI, list(date = lai_mean[[i]]$date[j],
                               site_id = new_site_info$site_id[i],
                               lat = new_site_info$lat[i],
                               lon = new_site_info$lon[i],
                               lai = lai_mean[[i]]$mean[j]*0.1,
                               sd = lai_std[[i]][j]*0.1,
                               qc = lai_qc[[i]][j]))
      }
    }
    #Compare with existing CSV file. (We name the CSV file as LAI.csv)
    if(as.logical(export_csv)){
      if(exists("Previous_CSV")){#we already read the csv file previously.
        Current_CSV <- rbind(Previous_CSV, LAI)
        Current_CSV <- Current_CSV[!duplicated(paste0(Current_CSV$site_id, Current_CSV$date)),]#using site_id and date to remove duplicated records.
        utils::write.csv(Current_CSV, file = file.path(outdir, "LAI.csv"), row.names = FALSE)
      }else{
        Current_CSV <- LAI
        utils::write.csv(Current_CSV, file = file.path(outdir, "LAI.csv"), row.names = FALSE)
      }
    } else {
      Current_CSV <- LAI
    }
    #Calculate LAI for each time step and site.
    #loop over time and site
    for (i in seq_along(time_points)) {
      t <- time_points[i]#otherwise the t will be number instead of date.
      for (id in new_site_info$site_id) {
        site_LAI <- Current_CSV[which(Current_CSV$site_id == id),]
        site_LAI$sd[which(site_LAI$sd<=0.66)] <- 0.66
        diff_days <- abs(lubridate::days(lubridate::date(site_LAI$date)-lubridate::date(t))@day)
        if(any(diff_days <= as.numeric(search_window))){#data found
          LAI_Output[which(LAI_Output$site_id==id), paste0(t, "_LAI")] <- site_LAI$lai[which.min(diff_days)]
          LAI_Output[which(LAI_Output$site_id==id), paste0(t, "_SD")] <- site_LAI$sd[which.min(diff_days)]
        }
      }
    }
  }
  PEcAn.logger::logger.info("MODIS LAI Prep Completed!")
  list(LAI_Output = LAI_Output, time_points = time_points, var = "LAI")
}

#' Prepare MODIS LAI data from the NASA DAAC server for the SDA workflow.
#'
#' @param site_info list: Bety list of site info including site_id, lon, and lat.
#' @param extent numeric: A vector contains the bounding box that covers all sites (West longitude, East longitude, South latitude ,North latitude).
#' @param from character: the start time for searching the MODIS products.
#' @param to character: the end time for searching the MODIS products.
#' @param download.outdir character: Where the MODIS tiles will be stored.
#' @param csv.outdir character: Where the final CSV file will be stored.
#' 
#' @return A data frame containing LAI and sd for each site and each time step.
#' @export
#' 
#' @author Dongchen Zhang
#' @importFrom magrittr %>%
Prep.MODIS.CSV.from.DAAC <- function(site_info, extent, from, to, download.outdir, csv.outdir) {
  # load previous CSV file.
  if (file.exists(file.path(csv.outdir, "LAI.csv"))) {
    previous.csv <- read.csv(file.path(csv.outdir, "LAI.csv"), 
                             colClasses = c("character", rep("numeric", 5), "character"))
  } else {
    previous.csv <- NULL
  }
  # setup the foreach parallel computation.
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  # reproject site locations to MODIS projection.
  site.ids <- site_info$site_id
  site.locs <- cbind(site_info$lon, site_info$lon) %>%
    `colnames<-`(c("lon","lat")) %>%
    `rownames<-`(site.ids)
  # create vector based on coordinates and MODIS projection.
  pts <- data.frame(lon = site.locs[,1], lat = site.locs[,2])
  sp::coordinates(pts) <- ~lon+lat
  sp::proj4string(pts) <- sp::CRS("+proj=longlat +datum=WGS84")
  pts.reproj <- sp::spTransform(pts, "+proj=sinu +a=6371007.181 +b=6371007.181 +units=m")
  coords.reproj <- sp::coordinates(pts.reproj) %>% `colnames<-`(c("x", "y"))
  # download data.
  metadata <- NASA_DAAC_download(ul_lat = extent[4], 
                                 ul_lon = extent[1], 
                                 lr_lat = extent[3], 
                                 lr_lon = extent[2], 
                                 from = from,
                                 to = to, 
                                 just_path = F,
                                 outdir = download.outdir,
                                 doi = "10.5067/MODIS/MCD15A3H.061",
                                 ncore = parallel::detectCores()-1)
  # grab file paths for downloaded hdf files.
  modis.out <- list.files(download.outdir, full.names = T, pattern = "*.hdf")
  # grab id for each file.
  ids <- basename(modis.out)
  # split string.
  splits <- strsplit(x = ids, split = ".", fixed = T)
  # collect attributes table (date, ih, and iv).
  dates <- ivs <- ihs <- c()
  for (i in seq_along(ids)) {
    temp <- splits[[i]]
    # calculate dates based on year and day of year.
    y.doy <- substr(temp[[2]], 2, 8)
    year <- substr(y.doy, 1, 4)
    doy <- substr(y.doy, 5, 7)
    dates <- c(dates, as.character(as.Date(paste(year, doy), format="%Y %j")))
    # extract tile id.
    ihs <- c(ihs, substr(temp[[3]], 2, 3))
    ivs <- c(ivs, substr(temp[[3]], 5, 6))
  }
  # create data frame of extents for MODIS tiles.
  # record progress.
  message("\nCreating information table for downloaded MODIS tiles.")
  doSNOW::registerDoSNOW(cl)
  pb <- utils::txtProgressBar(min=1, max=length(modis.out), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  MODIS.tiles.extents <- foreach::foreach(
    tile = modis.out,
    .packages=c("terra", "Kendall"),
    .options.snow=opts
  ) %dopar% {
    return(as.vector(terra::ext(terra::rast(tile))))
  } %>% dplyr::bind_rows()
  MODIS.tiles.info <- cbind(dates, ihs, ivs, MODIS.tiles.extents)
  # filter dates.
  unique.dates <- unique(MODIS.tiles.info$dates)
  unique.dates <- unique.dates[which(as.Date(unique.dates) >= from &
                                       as.Date(unique.dates) <= to)]
  # find inds of file paths that match each point.
  message("\nFinding indexes of file paths that match each point.")
  pb <- utils::txtProgressBar(min=1, max=length(pts), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  tile.ids <- foreach::foreach(
    i = as.numeric(site.ids),
    .packages="Kendall",
    .options.snow=opts
  ) %dopar% {
    # filter tile position ids based on lat/lon range.
    inds <- which(as.Date(MODIS.tiles.info$dates) >= from & 
                    as.Date(MODIS.tiles.info$dates) <= to & 
                    coords.reproj[i, "x"] > MODIS.tiles.info$xmin & 
                    coords.reproj[i, "x"] < MODIS.tiles.info$xmax & 
                    coords.reproj[i, "y"] > MODIS.tiles.info$ymin &
                    coords.reproj[i, "y"] < MODIS.tiles.info$ymax)
    names(inds) <- rep(i,length(inds))
    return(inds)
  } %>% unlist
  # extract MODIS products.
  # loop over tiles.
  message("\nExtracting MODIS datasets by points.")
  unique.tile.ids <- sort(unique(tile.ids)) 
  pb <- utils::txtProgressBar(min=1, max=length(unique.tile.ids), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  outputs <- foreach::foreach(
    i = unique.tile.ids,
    .packages=c("purrr", "Kendall"),
    .options.snow=opts
  ) %dopar% {
    temp.tile <- terra::rast(modis.out[i])
    points.inds <- as.numeric(names(tile.ids)[which(tile.ids == i)])
    temp.res <- terra::extract(temp.tile, data.frame(x = coords.reproj[points.inds, "x"],
                                                     y = coords.reproj[points.inds, "y"]))
    # convert QC flag.
    qc <- temp.res[,4] %>% purrr::map(function(v){
      qc_flag <- intToBits(as.integer(v)) # NB big-endian (ones place first)
      qc_flag <- as.integer(rev(utils::head(qc_flag, 3))) # now ones place last
      paste(qc_flag, collapse = "")
    }) %>% unlist
    # write into table.
    temp.res <- data.frame(date = MODIS.tiles.info$dates[i],
                           site_id = points.inds, 
                           lat = site.locs[points.inds, "lat"],
                           lon = site.locs[points.inds, "lon"],
                           lai = temp.res[,3],
                           sd = temp.res[,7],
                           qc = qc)
    return(temp.res)
  } %>% dplyr::bind_rows()
  # stop parallel.
  parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  # combine with previous CSV file.
  if (!is.null(previous.csv)) {
    outputs <- rbind(previous.csv, outputs)
    outputs <- outputs[!duplicated(outputs),]
  }
  # filter by QC band.
  outputs <- outputs[which(outputs$qc %in% c("000", "001")),]
  # write into CSV file.
  write.csv(outputs, file = file.path(csv.outdir, "LAI.csv"), row.names = F)
  # delete downloaded files.
  unlink(list.files(download.outdir, full.names = T), recursive = T)
  return(outputs)
}