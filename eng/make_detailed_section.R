# make_detailed_section <- function(..., order = c(3, 2, 1, 4), detailVector = "Details", bulletVector = "Bullets") { 
#   args <- list(...)
#   if (length(args) == 0) stop("Function requires arguments")
#   
#   for (i in length(args)) {
#     if (!is.list(args[[i]])) stop(paste("Argument", i, "is not a list."))
#   }
#   
#   df <- vector("list")
#   
#   for(i in 1:length(args)) {
#     df[[i]] <- tibble(
#       What = args[[i]][[detailVector]][[order[[1]]]], 
#       When = args[[i]][[detailVector]][[order[[2]]]], 
#       With = args[[i]][[detailVector]][[order[[3]]]], 
#       Where = args[[i]][[detailVector]][[order[[4]]]], 
#       Why = args[[i]][[bulletVector]][seq_along(args[[i]][[bulletVector]])]
#     )
#   }
#   
#   do.call(rbind.data.frame, df) %>%
#     detailed_entries(What, When, With, Where, Why)
#   
# }


make_detailed_section <- function(..., order = c(3, 2, 1, 4), detailVector = "Details", bulletVector = "Bullets") { 
  args <- list(...)
  if (length(args) == 0) stop("Function requires arguments")
  
  df <- vector("list")
  
  for(i in 1:length(args)) {
    # Filtrar los valores NA
    details <- args[[i]][[detailVector]]
    bullets <- args[[i]][[bulletVector]]
    
    # Crear un tibble solo con los detalles no NA
    non_na_details <- details[!is.na(details)]
    
    # Asegurarnos de que hay suficientes detalles antes de intentar acceder a ellos
    df[[i]] <- tibble(
      What  = if (length(non_na_details) >= 1) non_na_details[[1]] else NA,
      When  = if (length(non_na_details) >= 2) non_na_details[[2]] else NA,
      With  = if (length(non_na_details) >= 3) non_na_details[[3]] else NA,
      Where = if (length(non_na_details) >= 4) non_na_details[[4]] else NA,
      Why   = bullets[seq_along(bullets)]
    ) %>%
      filter(!is.na(What)) # Filtrar filas vacías o incompletas
  }
  
  do.call(rbind.data.frame, df) %>%
    detailed_entries(What, When, With, Where, Why)
}
