## code to prepare `DATASET` dataset goes here
cellmarker_database <- list(
    human = data.table::fread(
        "http://xteam.xbio.top/CellMarker/download/Human_cell_markers.txt"
    ),
    mouse = data.table::fread(
        "http://xteam.xbio.top/CellMarker/download/Mouse_cell_markers.txt"
    )
)
cellmarker_database <- lapply(cellmarker_database, cellmarker_prepare)

usethis::use_data(
    cellmarker_database,
    internal = TRUE, overwrite = TRUE,
    compress = "gzip"
)
