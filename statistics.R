IQR_outliers <- function(data){
  iqr <- IQR(data, type = 2)
  quantile <- quantile(data, type = 2)
  weight <- iqr * 1.5
  left_boundary <- as.numeric(quantile[2] - weight)
  right_boundary <- as.numeric(quantile[3] + weight)
  return(data[data < left_boundary | data > right_boundary])
}