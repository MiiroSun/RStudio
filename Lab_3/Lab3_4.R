generateMatrix <- function(x = matrix()) {
  inv <- NULL                       # кэш обратной матрицы, изначально пуст
  
  set <- function(y) {
    x   <<- y                       # присваиваем в среду generateMatrix, а не в set
    inv <<- NULL                    # сброс кэша: матрица изменилась
  }
  # Получить текущую матрицу
  get        <- function() x
  # Кладём готовую обратную матрицу в кэш
  setInverse <- function(inverse) inv <<- inverse
  # 
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheInverseMatrix <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("получение закешированных данных")
    return(inv)
  }
  
  # Кэш пуст-берём матрицу и считаем обратну
  mat <- x$get()
  inv <- solve(mat, ...)
  
  # Запоминаем результат
  x$setInverse(inv)
  inv
}
