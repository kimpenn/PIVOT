


callModule(pivot_help, "meanvar_help", title = "About mean variability plot", content = list(
    tags$li("This plot is generated using the Seurat package, for details please go to http://www.satijalab.org/clustertutorial1.html."),
    tags$li("Description: Identifies genes that are outliers on a 'mean variability plot'. First, uses a function to calculate average expression (fxn.x) and dispersion (fxn.y) for each gene. Next, divides genes into num.bin (deafult 20) bins based on their average expression, and calculates z-scores for dispersion within each bin. "),
    tags$li("The purpose of this is to identify variable genes while controlling for the strong relationship between variability and average expression.")
))
