library(RPostgreSQL)
library(DT)
library(plotly)
library(rjson)
library(pool)

pool <- dbPool(
  drv = dbDriver("PostgreSQL", max.con = 100),
  dbname= "dbhitsa2019",
  host = "dev.vk.edu.ee",
  port = 5432,
  user = "ruuvi_sel",
  password = "ruuvisel",
  idleTimeout = 3600000
)

controller <- c(dbGetQuery(pool, "SELECT controllername FROM controller"))
tables <- c(dbGetQuery(pool, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' AND table_type = 'BASE TABLE'" ))
sensor <- c(dbGetQuery(pool, "SELECT sensorname FROM sensor"))
date <- c(dbGetQuery(pool,"SELECT MIN(date_time) FROM vw_sensorsdata"))
controller2 <- c(dbGetQuery(pool, "SELECT controller FROM vw_sensorsdata GROUP BY controller"))
