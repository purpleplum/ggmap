#' Format URI for tileservers
#'
#' Formats a URI for a tileserver. The URI is specified as
#' `http://example.org/{z}/{x}/{y}.png` or similar.
#' @export
#' @param uri URI pattern
#' @param x x coordinate
#' @param y y coordinate
#' @param z z coordinate
format_uri <- function(uri, x, y, z) {
  uri <- sub('\\$?\\{x\\}', x, uri)
  uri <- sub('\\$?\\{y\\}', y, uri)
  uri <- sub('\\$?\\{z\\}', z, uri)
  return(uri)
}

#' Gets a tile
#'
#' Tries to get tile `x`, `y`, `z` from tile server(s) defined in
#' `uri_patterns`. On failure recurses with a decreased max_tries until
#' max_tries reaches zero.
#' @param x X coordinate of tile
#' @param y Y coordinate of tile
#' @param z Z coordinate of tile
#' @param uri_patterns Patterns describing one or several tile servers where
#' the tiles can be found. Should be formatted as described in `format_uri`.
#' @param max_tries Maximum tries until failure
#' @param start_index Index of uri_patterns to start getting data from. On
#' failure is incremented to try the next server: i=i+1%%length(uri_patterns)+1
#' @param messaging print messages in console
#' @param force ignore cached requests
#' @return a ggmap object with the tile that was requested
#' @export
get_tile <- function(x, y, z, uri_patterns, max_tries=length(uri_patterns)*3,
                     start_index=1, messaging=F, force=F) {
    # things
    tile_get_request <- function(x, y, z, uri_patterns, tries=9, index=1) {
      # Function for recursive tile getting until tries is exceeded.
      if (tries==0)
        stop('Tries exceeded')
      uri <- format_uri(uri_patterns[index], x, y, z)
      response <- file_drawer_get(uri)
      if (!is.null(response) & !force) 
        return(httr::content(response))
      response <- httr::GET(uri)
      if (response$status_code != 200L) {
        if (messaging) message(sprintf('Failed GET `%s`', uri))
        return(tile_get_request(x, y, z, uri_patterns, tries=tries-1, index=((index+1)%%length(uri_patterns))+1))
      } else {
        if (messaging) message(sprintf('Successful GET `%s`', uri))
        file_drawer_set(uri, response)
        return(httr::content(response))
      }
    }
    tile <- tile_get_request(x, y, z, uri_patterns, tries=max_tries, index=start_index)
    tile <- apply(tile, 1:2, function(x) rgb(x[1], x[2], x[3]))
    tile <- t(tile)
    upleft <- XY2LonLat(x, y, z)
    downright <- XY2LonLat(x, y, z, 255L, 255L)
    bb <- tibble(
      "ll.lat" = unname(downright$lat),
      "ll.lon" = unname(upleft$lon),
      "ur.lat" = unname(upleft$lat),
      "ur.lon" = unname(downright$lon)
    )
    class(tile) <- c('ggmap', 'raster')
    attr(tile, 'bb') <- bb
    return(tile)
}

#' Get a map from a tileserver
#'
#' Gets a map with boundaries defined by `bbox` from servers defined by
#' `uri_patterns` with zoom `zoom`.
#'
#' @export
#' @param bbox bounding box for map.
#' @param uri_patterns URI patterns to use for getting the data. See
#' `format_uri` on pattern definition.
#' @param crop Crop image to bbox
#' @param force Ignore cached requests
#' @param messaging Verbose mode
#' @return ggmap object containing the map requested.
get_tilemap <- function(bbox, uri_patterns, zoom, crop=T, force=F, messaging=F, ...) {
  corners <- expand.grid(
    lon=c(bbox['left'], bbox['right']),
    lat=c(bbox['bottom'], bbox['top'])
  )
  corners$zoom <- zoom
  row.names(corners) <- c("lowerleft","lowerright","upperleft","upperright")
  corner_tiles <- apply(corners, 1, function(v) LonLat2XY(v[1],v[2],v[3]))
  tile_coords <- expand.grid(
    x=Reduce(":", sort(unique(as.numeric(sapply(corner_tiles, function(df) df$X))))),
    y=Reduce(":", sort(unique(as.numeric(sapply(corner_tiles, function(df) df$Y)))))
  )
  tile_coords$z <- zoom
  # Get tiles. start_index makes sure we evently use all the different tile
  # servers listed in uri_patterns.
  tiles <- mapply(get_tile, tile_coords$x, tile_coords$y, tile_coords$z,
                  start_index=(0:(nrow(tile_coords)-1)%%length(uri_patterns))+1,
                  MoreArgs=list(uri_patterns=uri_patterns, messaging=messaging, force=force), 
                  SIMPLIFY=F)
  map <- tile_stitch(tiles)
  attr(map, 'zoom') <- zoom
  attr(map, 'source') <- 'tiles'
  if (crop) {
    map <- crop_map(map, bbox, tile_coords)
  }
  return(map)
}

#' Crops the map
#' 
#' Crop `map` to bbox.
#'
#' This will at some point be a function usable outside of get_tilemap. For
#' the time being, don't use this.
#' @param map A ggmap
#' @param bbox A bbox
#' @param bbox list of tiles in map
crop_map <- function(map, bbox, tiles) {
  # ToDo: make a general purpose cropping function
  mbbox <- attr(map, 'bb')
  zoom <- attr(map, 'zoom')
  size <- dim(map)
  size <- c(x=size[2], y=size[1])
  slon <- seq(mbbox$ll.lon, mbbox$ur.lon, length.out=size['x'])
  ys <- unique(tiles$y)
  xs <- unique(tiles$x)
  slat <- vector("double", length = 256L*length(ys))
  for(k in seq_along(ys)){
    slat[(k-1)*256 + 1:256] <- sapply(
      as.list(0:255), 
      function(y){
        XY2LonLat(X=xs[1], Y=ys[k], zoom, x=0, y=y)$lat
      }
    )
  }
  slat <- rev(slat)
  tbox <- c(
    left=min(bbox['left'], bbox['right']),
    right=max(bbox['left'], bbox['right']),
    bottom=min(bbox['bottom'], bbox['top']),
    top=max(bbox['bottom'], bbox['top'])
  )
  x_keep <- which(tbox["left"] <= slon & slon <= tbox["right"])
  y_keep <- sort(size['y'] - which(tbox["bottom"] <= slat & slat <= tbox["top"]))
  crop <- map[y_keep, x_keep]
  crop <- as.raster(crop)
  class(crop) <- c('ggmap', 'raster')
  attr(crop, 'bb') <- data.frame(
    ll.lat=tbox['bottom'], ll.lon=tbox['left'],
    ur.lat=tbox['top'], ur.lon=tbox['right']
  )
  attr(crop, 'source') <- attr(map, 'source')
  attr(crop, 'zoom') <- attr(map, 'zoom')
  return(crop)
}

#' Stitch list of ggmaps
#' 
#' Stitches a list of ggmap objects, as they are for example provided by
#' get_tilemap or get_tile.
#' @export
#' @param tiles A list of ggmap objects to stitch
#' @return A single ggmap object stitched from the other ggmap objects.
tile_stitch <- function(tiles){
  # determine bounding box
  bbs <- ldply(tiles, function(x) attr(x, "bb"))
  bigbb <- data.frame(
    ll.lat=min(bbs$ll.lat),
    ll.lon=min(bbs$ll.lon),
    ur.lat=max(bbs$ur.lat),
    ur.lon=max(bbs$ur.lon)
  )
  # determine positions of tile in slate (aggregate)
  ord <- order(-(bbs$ll.lat), bbs$ll.lon)
  tiles <- tiles[ord]
  tiles <- lapply(tiles, as.matrix) # essential for cbind/rbind to work properly!
  # split tiles, then squeeze together from top and bottom
  # and then squeeze together from left and right
  nrows <- length(unique(bbs$ll.lat))
  ncols <- length(unique(bbs$ll.lon))
  tiles <- split(tiles, rep(1:nrows, each = ncols))
  tiles <- lapply(tiles, function(x) Reduce(cbind, x))
  tiles <- Reduce(rbind, tiles)
  tiles <- as.raster(tiles)
  class(tiles) <- c("ggmap", "raster")
  attr(tiles, "bb") <- bigbb
  return(tiles)
}

#' Get openstreetmap maps.
#' 
#' Gets a map from openstreetmaps defined by bbox and zoom.
#' Essentially a wrapper around `get_tilemap`.
#' @param bbox Bounding Box for map
#' @param zoom Zoom level of map
#' @param ... Further arguments passed to get_tilemap
#' @export
get_openstreetmap <- function(bbox=c(left=-95.80204, bottom=29.38048,
                                     right=-94.92313, top=30.14344), 
                              zoom=10, ...) {
  uri_patterns <- c('https://a.tile.openstreetmap.org/{z}/{x}/{y}.png',
                    'https://b.tile.openstreetmap.org/{z}/{x}/{y}.png',
                    'https://c.tile.openstreetmap.org/{z}/{x}/{y}.png')
  map <- get_tilemap(bbox, uri_patterns, zoom=zoom, ...)
  attr(map, 'source') <- 'openstreetmaps'
  return(map)
}

#' Get opentopomap maps.
#' 
#' Gets a map from opentopomaps defined by bbox and zoom.
#' Essentially a wrapper around `get_tilemap`.
#' @param bbox Bounding Box for map
#' @param zoom Zoom level of map
#' @param ... Further arguments passed to get_tilemap
#' @export
get_opentopomap <- function(bbox=c(left=-95.80204, bottom=29.38048,
                                     right=-94.92313, top=30.14344), 
                              zoom=10, ...) {
  uri_patterns <- c('https://a.tile.opentopomap.org/{z}/{x}/{y}.png',
                    'https://b.tile.opentopomap.org/{z}/{x}/{y}.png',
                    'https://c.tile.opentopomap.org/{z}/{x}/{y}.png')
  map <- get_tilemap(bbox, uri_patterns, zoom=zoom, ...)
  attr(map, 'source') <- 'opentopomaps'
  return(map)
}

#' Get wikimedia maps.
#' 
#' Gets a map from wikimedia defined by bbox and zoom.
#' Essentially a wrapper around `get_tilemap`.
#' @param bbox Bounding Box for map
#' @param zoom Zoom level of map
#' @param ... Further arguments passed to get_tilemap
#' @export
get_wikimedia <- function(bbox=c(left=-95.80204, bottom=29.38048,
                                     right=-94.92313, top=30.14344), 
                              zoom=10, ...) {
  uri_patterns <- c('https://maps.wikimedia.org/osm-intl/{z}/{x}/{y}.png')
  map <- get_tilemap(bbox, uri_patterns, zoom=zoom, ...)
  attr(map, 'source') <- 'wikimedia'
  return(map)
}

#' Get stamentoner maps.
#' 
#' Gets a map from stamentoner defined by bbox and zoom.
#' Essentially a wrapper around `get_tilemap`.
#' @param bbox Bounding Box for map
#' @param zoom Zoom level of map
#' @param ... Further arguments passed to get_tilemap
#' @export
get_stamentoner <- function(bbox=c(left=-95.80204, bottom=29.38048,
                                     right=-94.92313, top=30.14344), 
                              zoom=10, ...) {
  uri_patterns <- c('https://a.tile.stamen.com/toner/{z}/{x}/{y}.png')
  map <- get_tilemap(bbox, uri_patterns, zoom=zoom, ...)
  attr(map, 'source') <- 'stamen toner'
  return(map)
}

#' Get mapnik grayscale maps.
#' 
#' Gets a map from mapnik (grayscale) defined by bbox and zoom.
#' Essentially a wrapper around `get_tilemap`.
#' @param bbox Bounding Box for map
#' @param zoom Zoom level of map
#' @param ... Further arguments passed to get_tilemap
#' @export
get_mapnikgrayscale <- function(bbox=c(left=-95.80204, bottom=29.38048,
                                     right=-94.92313, top=30.14344), 
                              zoom=10, ...) {
  uri_patterns <- c(
    "https://a.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",
    "https://b.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png",
    "https://c.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png"
  )
  map <- get_tilemap(bbox, uri_patterns, zoom=zoom, ...)
  attr(map, 'source') <- 'mapnik grayscale'
  return(map)
}

#' Get mapnik no labels maps.
#' 
#' Gets a map from mapnik (labelless) defined by bbox and zoom.
#' Essentially a wrapper around `get_tilemap`.
#' @param bbox Bounding Box for map
#' @param zoom Zoom level of map
#' @param ... Further arguments passed to get_tilemap
#' @export
get_mapniknolabels <- function(bbox=c(left=-95.80204, bottom=29.38048,
                                     right=-94.92313, top=30.14344), 
                              zoom=10, ...) {
  uri_patterns <- c(
    "https://a.tiles.wmflabs.org/osm-no-labels/{z}/{x}/{y}.png",
    "https://b.tiles.wmflabs.org/osm-no-labels/{z}/{x}/{y}.png",
    "https://c.tiles.wmflabs.org/osm-no-labels/{z}/{x}/{y}.png"
  )
  map <- get_tilemap(bbox, uri_patterns, zoom=zoom, ...)
  attr(map, 'source') <- 'mapnik no labels'
  return(map)
}
