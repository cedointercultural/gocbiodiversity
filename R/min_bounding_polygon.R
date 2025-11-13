#' @title Get minimum bounding polygon
#' @description https://johngodlee.xyz/posts/2021-11-14-minimum_rectangle/
#' @param polygon
#'
#' @returns
#' @export
#'
#' @examples
min_bounding_polygon <- function(polygon) {

    stopifnot(all(sf::st_is(polygon, c("POLYGON", "MULTIPOLYGON"))))

  min_box_list <-   lapply(sf::st_geometry(polygon), function(y) {
      x_mat <- sf::st_coordinates(y)[,1:2]

      # Extract convex hull of polygon
      H <- chull(x_mat)
      n <- length(H)
      hull <- x_mat[H, ]

      # Get direction vector
      hDir <- diff(rbind(hull, hull[1,]))
      hLens <- sqrt(rowSums(hDir^2))
      huDir <- diag(1/hLens) %*% hDir
      ouDir <- cbind(-huDir[,2], huDir[,1])

      # Project hull vertices
      projMat <- rbind(huDir, ouDir) %*% t(hull)

      # Get width and length of bounding rectangle
      rangeH <- matrix(numeric(n*2), ncol = 2)
      rangeO <- matrix(numeric(n*2), ncol = 2)
      widths <- numeric(n)
      lengths <- numeric(n)

      for(i in seq(along=numeric(n))) {
        rangeH[i,] <- range(projMat[i,])
        rangeO[i,] <- range(projMat[n+i,])
        widths[i] <- abs(diff(rangeH[i,]))
        lengths[i] <- abs(diff(rangeO[i,]))
      }

      eMin <- which.min(widths*lengths)
      hProj <- rbind(rangeH[eMin,], 0)
      oProj <- rbind(0, rangeO[eMin,])

      # Move projections to rectangle corners
      hPts <- sweep(hProj, 1, oProj[,1], "+")
      oPts <- sweep(hProj, 1, oProj[,2], "+")

      # Get corner coordinates
      basis <- cbind(huDir[eMin,], ouDir[eMin,])
      hCorn <- basis %*% hPts
      oCorn <- basis %*% oPts
      pts <- t(cbind(hCorn, oCorn[,c(2,1)]))

      # Angle
      dPts <- diff(pts)
      e <- dPts[which.max(rowSums(dPts^2)), ]
      eUp <- e * sign(e[2])
      deg <- atan2(eUp[2], eUp[1])*180/pi

      return(list(pts = pts, length = lengths[eMin],
                  width = widths[eMin], angle = deg))
    })


  min_box_sf <- do.call(rbind, lapply(min_box_list, function(x) {
     pts_sf <- sf::st_as_sf(as.data.frame(x$pts), coords = c("X", "Y"))
     sf::st_sf(geometry = sf::st_convex_hull(sf::st_union(pts_sf)), crs = sf::st_crs(polygon))
     }))

  return(min_box_sf)
  }

