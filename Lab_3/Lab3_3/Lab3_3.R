#1 задание
getMean <- function(folder, type, id = 1:332) {

  if (!type %in% c("sulfate", "nitrate")) {
    stop("type must be 'sulfate' or 'nitrate'")
  }
  
  paths <- file.path(folder, sprintf("%03d.csv", id))
  
  # читаем нужный столбец из каждого файла и склеиваем в один вектор
  values <- unlist(lapply(paths, function(p) read.csv(p)[[type]]))
  
  # среднее с игнорированием NA
  mean(values, na.rm = TRUE)
}

#2 задание
getCompleteObservation <- function(folder, id = 1:332) {
  paths <- file.path(folder, sprintf("%03d.csv", id))
  
  counts <- vapply(paths, function(p) {
    sum(complete.cases(read.csv(p)))
  }, integer(1))
  
  data.frame(id = id, count = counts)
}

#3 задание
getCorrelation <- function(folder, limen = 0) {
  
  paths <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  
  cors <- numeric(0)
  for (p in paths) {
    dat <- read.csv(p)
    if (sum(complete.cases(dat)) > limen) {
      cors <- c(cors, cor(dat$sulfate, dat$nitrate, use = "complete.obs"))
    }
  }
  cors
}
