# Title     : export.R
# Objective : Exports calculations, graphs, etc.
# Created by: greyhypotheses
# Created on: 14/08/2022


hub <- dirname(getwd())


# the list of this project's warehouse directories
internal <- file.path(getwd(), 'warehouse')
directories <- list.dirs(path = internal)


# the names of the mirror directories in an external project's warehouse
external <- file.path(hub, 'manuscript', 'warehouse')
mirror <- lapply(X = directories, FUN = function (x) str_replace(x, pattern = internal, replacement = external)) %>%
  unlist()
mirror <- mirror[!(mirror %in% external)]


# hence, create the external storage directories
.create<- function (path) {
  if (dir.exists(path)) {
    base::unlink(x = path, recursive = TRUE)
  }
  dir.create(path = path, recursive = TRUE)
}
lapply(X = mirror, FUN = .create)


# the files to be delivered to the <manuscript> repository
src <- list.files(path = file.path(getwd(), 'warehouse'),
                  full.names = TRUE, recursive = TRUE)


# delivery names
dst <- lapply(
X = src,
FUN = function (x) paste0(dirname(external), str_replace(x, pattern = getwd(), replacement = ''))
) %>%
  unlist()


# finally
mapply(FUN = file.copy, from = src, to = dst)

