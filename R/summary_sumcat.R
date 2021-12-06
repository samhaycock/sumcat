
summary.sumcat <- function(sumcat) {
  for (i in seq_len(length(sumcat))) {
    print(rep("=", 80))
    print(sumcat[[i]])
  }
}
