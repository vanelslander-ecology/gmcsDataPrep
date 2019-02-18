myClimatebcAPI <- function (x, period = "Normal_1961_1990.nrm", ysm = "Y")
{
  url = "http://api.climatebc.ca/api/clmApi"
  require(jsonlite)
  require(httr)
  library("foreach")
  library("doSNOW")
  require(tictoc)
  body <- x
  body$prd <- period
  body$varYSM <- ysm
  head(body)
  set.seed(123321)
  #nCore <- parallel::detectCores()
  #nCore = nCore - 2
  #nCore
  #cl <- makeCluster(nCore, type = "SOCK")
  #registerDoSNOW(cl)
  NperGrp <- 1000
  grp <- seq(0, nrow(body), by = NperGrp)
  grp
  tic()
  results <-
    lapply(grp,
            # .combine = rbind,
            # .packages = c("httr",
            #               "jsonlite"))
  # %dopar%
  function(k) {
    out <- sapply(1:NperGrp, function(i) {
      num <- i + k
      if (num <= nrow(body)) {
        paste(c(paste0(sprintf("[%d][ID1]", i - 1),
                       "=", body$ID1[num]), paste0(sprintf("[%d][ID2]",
                                                           i - 1), "=", body$ID2[num]), paste0(sprintf("[%d][lat]",
                                                                                                       i - 1), "=", body$lat[num]), paste0(sprintf("[%d][lon]",
                                                                                                                                                   i - 1), "=", body$lon[num]), paste0(sprintf("[%d][el]",
                                                                                                                                                                                               i - 1), "=", body$el[num]), paste0(sprintf("[%d][prd]",
                                                                                                                                                                                                                                          i - 1), "=", body$prd[num]), paste0(sprintf("[%d][varYSM]",
                                                                                                                                                                                                                                                                                      i - 1), "=", body$varYSM[num])), collapse = "&")
      }
    })
    browser()
    out <- paste(out, collapse = "&")
    head(out)
    result <- POST(url = url, body = out, add_headers(`Content-Type` = "application/x-www-form-urlencoded"),
                   timeout(400000))
    output <- fromJSON(rawToChar(result$content))
    output <- subset(output, select = -c(prd, varYSM))
    output
  })

  browser()
  toc()
  stopCluster(cl)
  head(results)
  ver = results$Version[1]
  ver
  alertText = paste0("The climate data were genrated by ",
                     ver, " for the period ", period)
  print(alertText)
  id <- results[, 1:2]
  head(id)
  clmv <- data.frame(lapply(results[, -c(1:2, ncol(results))],
                            as.numeric))
  head(clmv)
  cmb <- data.frame(id, clmv)
  head(cmb)
  return(cmb)
}