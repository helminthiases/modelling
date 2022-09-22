# Title     : import.R
# Objective : Imports data
# Created by: greyhypotheses
# Created on: 05/09/2022


# storage
storage <- file.path(getwd(), 'data')


# directories
if (dir.exists(paths = storage)) {
  base::unlink(storage, recursive = TRUE)
}
dir.create(path = storage, showWarnings = TRUE, recursive = TRUE)


# the list of data files
T <- read.csv('https://raw.githubusercontent.com/helminthiases/spatial/master/warehouse/features/data.csv')
URL <- T$path
names <- lapply(X = URL, FUN = function (x) basename(path = x)) %>% unlist()


# Unload function
temporary <- function (url, name, storage) {
  httr::GET(url = url,
            httr::write_disk(path = file.path(storage, name), overwrite = TRUE),
            overwrite = TRUE)
}


# Unload in-parallel
cores <- parallel::detectCores() / 2
doParallel::registerDoParallel(cores = cores)
clusters <- parallel::makeCluster(cores)
parallel::clusterMap(clusters, fun = temporary, URL, names,
                     MoreArgs = list(storage = storage))
parallel::stopCluster(clusters)
rm(clusters, cores)
