# Since this file will run code from other files, it's better to keep this file
# in the end of "R/".

# https://www.sciencedirect.com/science/article/pii/S153561082100115X
# https://ars.els-cdn.com/content/image/1-s2.0-S153561082100115X-mmc1.pdf
new_marker_set(
    name = "Braun_2021",
    main = list(immune = "PTPRC", Epithelium = "EPCAM"),
    Immune = list(
        `T cell` = c(
            "CD3D", "CD3E", "CD3G", "CD4", "CD8A", "CD8B", "FOXP3", "IL2RA"
        ),
        `NK cell` = c("NCAM1", "FCGR3A", "NCR1", "KLRB1"),
        `B cell` = c("CD19", "MS4A1"),
        `plasma` = c("CD38", "SDC1", "TNFRSF17"),
        `Myeloid` = c(
            "ITGAM", "ITGAX", "CSF1R", "CD68", "CD163", "THBD", "CLEC9A",
            "CLEC4C", "TPSAB1", "KIT"
        )
    ),
    Epithelium = list(tumor = "CA9", normal = c("ALDOB", "UMOD")),
    Myeloid = list(
        Monocyte = c("CD14", "FCGR3A", "S100A8", "S100A9", "SELL"),
        Macrophage = c("CD68", "CD163", "CSF1R", "CD69"),
        Maturation = c("CD80", "CD86"),
        `Antigen presentation` = c(
            "HLA-DRA", "HLA-DRB1", "HLA-DQA1", "HLA-DQA2",
            "HLA-DQB1", "HLA-DQB2"
        ),
        Inflammation = c("TNF", "IL1B", "IFNG", "IL6", "IL8"),
        Apolipoprotein = c("APOC1", "APOE"),
        Complement = c("C1QA", "C1QB", "C1QC"),
        `Dendritic cell` = c(
            "FLT3", "THBD", "BATF", "CLEC9A", "CD1C", "CLEC4C"
        ),
        Mast = c("TPSAB1", "KIT"),
        Proliferation = c("MK167", "TOP2A")
    ),
    reference = "https://www.sciencedirect.com/science/article/pii/S153561082100115X"
)

new_marker_set(
    name = "Luo_2022",
    main = list(
        Fibroblast = c("DCN", "COL1A1"),
        Lymphocyte = c("CD3D", "CD3E"),
        Myeloid = c("CD68", "CD14"),
        Endothelium = c("VWF", "PECAM1"),
        Plasma = c("IGHG1", "JCHAIN"),
        Epithelium = c("EPCAM", "KRT19")
    ),
    reference = "https://www.nature.com/articles/s41467-022-34395-2"
)


# tumor microenvironments (TMEs) consist of several distinct major lineages
# including mast cells, plasmacytoid dendritic cells (pDCs), conventional
# dendritic cells (cDCs), monocytes, and macrophages.

# Monocytes are usually classified based on the expression of surface markers
# CD14 and CD16

# Macrophages are critical mediators in TME and participate in multiple aspects
# of tumor immunity (DeNardo and Ruffell, 2019). The "classically activated" M1
# and "alternatively activated" M2 macrophage polarization system has been used
# to describe the in vitro activation state of macrophages (Vogel et al., 2014).
# However, macrophages in vivo exhibit more complex phenotypes, which argue
# against such a simple categorization in vitro.

# Dendritic cells (DCs) are key players in antigen-specific immune responses
# (Mellman, 2013).  Two distinct cDC subsets, XCR1+CADM1+ cDC1s and CD1A+
# CD172A+ cDC2s, have been identified and shown to interact with CD8+ and CD4+ T
# cells, respectively (Binnewies et al., 2019; Salmon et al., 2016).

# Furthermore, the heterogeneity of cDC2s only starts to be revealed recently,
# as distinct cDC2 subsets in human blood and spleen have been identified (Brown
# et al., 2019; Dutertre et al., 2019; Villani et al., 2017). However, the
# complexity of cDC2s across tumors is still not fully characterized.



# Three DC subsets (hM02-hM04), plasmacytoid DC (pDC), cDC2, and cDC1 cells,
# characterized by high expression of HLA-DRs and low expression of CD14
# (Figures S4B and S4C), were also identified and were further distinguished by
# specific expression of LILRA4/LILRB4, CD1C/FCER1A, and XCR1/BATF3,
# respectively (Figure 2B).
# The remaining clusters were identified as macrophages based on their high
# expression of CD68, CD163, and MRC1 (encoding CD206).
