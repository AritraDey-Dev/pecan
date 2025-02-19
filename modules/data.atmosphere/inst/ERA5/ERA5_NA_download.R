library(reticulate)
library(future)
library(purrr)
library(furrr)
outdir <- "/projectnb/dietzelab/dongchen/anchorSites/ERA5/"
years <- 2012:2024
months <- list('01','02','03',
               '04','05','06',
               '07','08','09',
               '10','11','12')
days <- list('01','02','03',
             '04','05','06',
             '07','08','09',
             '10','11','12',
             '13','14','15',
             '16','17','18',
             '19','20','21',
             '22','23','24',
             '25','26','27',
             '28','29','30',
             '31')
times <- list('00:00','03:00','06:00',
              '09:00','12:00','15:00',
              '18:00','21:00')
area <- "85/-179/7/-20"
variables <- list( "2m_temperature","surface_pressure",
                   "2m_dewpoint_temperature","total_precipitation",                
                   "10m_u_component_of_wind","10m_v_component_of_wind",            
                   "surface_solar_radiation_downwards","surface_thermal_radiation_downwards")
paths <- PEcAn.data.atmosphere::ERA5_cds_annual_download(years = year, 
                                                         months = months, 
                                                         days = days, 
                                                         times = times, 
                                                         area = area, 
                                                         variables = variables, 
                                                         outdir = outdir, 
                                                         auto.create.key = T)
