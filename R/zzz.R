if (getRversion() >= "2.15.1") {
    utils::globalVariables(
        c(
            ".pipeline", ".step", # used as data pronoun
            # `cellmarker_prepare`
            "gene_list",
            # Variable used in function `cellmarker_search`
            "targeted", "gene_list", "targeted_size", "targeted_prop",
            # variable used in function `step_sce_normalize`
             "sizefactor"
        )
    )
}
