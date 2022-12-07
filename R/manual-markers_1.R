# https://www.sciencedirect.com/science/article/pii/S153561082100115X?via%3Dihub#app2
# https://ars.els-cdn.com/content/image/1-s2.0-S153561082100115X-mmc1.pdf
new_marker_set(
    name = "Braun_2021",
    main = list(immune = "PTPRC", epithelial = "EPCAM"),
    immune = list(
        `T cell` = c(
            "CD3D", "CD3E", "CD3G", "CD4", "CD8A", "CD8B", "FOXP3", "IL2RA"
        ),
        `NK cell` = c("NCAM1", "FCGR3A", "NCR1", "KLRB1"),
        `B cell` = c("CD19", "MS4A1"),
        `Plasma` = c("CD38", "SDC1", "TNFRSF17"),
        `Myeloid` = c(
            "ITGAM", "ITGAX", "CSF1R", "CD68", "CD163", "THBD", "CLEC9A",
            "CLEC4C", "TPSAB1", "KIT"
        )
    ),
    epithelial = list(tumor = "CA9", normal = c("ALDOB", "UMOD")),
    reference = "https://www.sciencedirect.com/science/article/pii/S153561082100115X"
)
