load_all_packages <- function(vector){
  for (i in vector) {
    if (!requireNamespace(i, quietly = TRUE)){
      install.packages(i)
    }
    library(i,character.only=T)
  }

}

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

add_lag <- function(df, cols) {
  for(col in cols) {
    df$temp = df[[col]]
    df = df %>%
      mutate(
        temp_lag = lag(temp,1)
      ) %>%
      select(-temp)
    var_name = paste0(col,"_lag")
    df[[var_name]] = df$temp_lag
    df = df %>%
      select(-temp_lag)

  }
  df = df[ , -which(names(df) %in% cols)]
  return(df)
}
