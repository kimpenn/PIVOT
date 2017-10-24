

# The help button in PIVOT, fully modulized code

callModule(pivot_help, "meanvar_help", title = "About mean variability plot", content = list(
    tags$li("This plot is generated using the Seurat package, for details please go to http://www.satijalab.org/clustertutorial1.html."),
    tags$li("Description: Identifies genes that are outliers on a 'mean variability plot'. First, uses a function to calculate average expression (fxn.x) and dispersion (fxn.y) for each gene. Next, divides genes into num.bin (deafult 20) bins based on their average expression, and calculates z-scores for dispersion within each bin. "),
    tags$li("The purpose of this is to identify variable genes while controlling for the strong relationship between variability and average expression.")
))

callModule(pivot_help, "directory", title = "How to input a count folder", content = list(
    tags$li("Accepts folder containing HTSeq/featureCounts/VERSE output."),
    tags$li("Counts files must contain the same feature set (Same number of rows, same rownames, multiple species not allowed)."),
    tags$li("You can use file filter to select files containing certain keywords"),
    tags$li("Example count file:"),
    br(),
    img(src = "exp_count_file.png", width = 150)
))

callModule(pivot_help, "tenx_input", title = "How to input a 10x Cell Ranger output folder", content = list(
    tags$li("PIVOT expect the output directory produced by Cell Ranger (the folder containing `outs`).")
))


callModule(pivot_help, "single", title = "How to input a count table", content = list(
    tags$li("PIVOT only accepts .csv or .txt input."),
    tags$li("Feature names or sample names may be slightly modified to be valid R row/column names."),
    tags$li("Rows must be features and columns must be samples."),
    tags$li("Negative values are not allowed."),
    tags$li("Example:"),
    img(src = "input_exp.png", width = 500)
))

callModule(pivot_help, "design", title = "What is a design table", content =  list(
    tags$li("Specify group information (conditions used for differential expression analysis), or batch information (different experiments), or other sample metadata."),
    tags$li("Adding this information will not affect normalization results, or any analysis that does not depend on group information."),
    tags$li("You can upload entire sample metadata sheet. Each column will be treated as a category, and the values in each column will be treated as groups in that category.")
))

callModule(pivot_help, "design_input", title = "File format requirements", content = list(
    tags$li("The first column must be sample column, and rest be metadata (categories). Values in each non-sample column will be treated as groups in that category"),
    tags$li("All samples must be present in the table (case sensitive). It is not recommended to have empty cells in the sheet, as those may affect some analysis/plots."),
    img(src = "sample_design_tbl.png", width = 500)
))


callModule(pivot_help, "feature_convert", title = "Feature id/name conversion mechanism", content = list(
    tags$li("Feature id/name conversion is performed using the bioMart database."),
    tags$li("PIVOT will try to convert all your features to the desired gene name/id type."),
    tags$li("For features that has no match in the database, PIVOT will keep its original ID."),
    tags$li("For features that map to multiple entries in the database, PIVOT will use the first matched entry."),
    tags$li("For multiple features that map to one entry in the database, PIVOT will name the duplicates as 'name.1', 'name.2',etc.")
))

callModule(pivot_help, "filter_type", title = "What is a feature filter", content = list(
    tags$li("You can select or delete features from the input set to create subset for analysis."),
    tags$li("You can use two types of filters, expression filter or feature list filter (below option)."),
    tags$li("Expression filter allow you to choose features within a certain range of average/total expression."),
    tags$li("Gene list filter will only keep genes you are interested for further analysis.")
))

callModule(pivot_help, "marker_filter", title ="File format requirements", content = list(
    tags$p("The first column of the uploaded file will be used as markers (case insensitive). Example:"),
    img(src = "marker_tbl_exp.png", width = 250)
))

callModule(pivot_help, "subset_upload", title ="File format requirements", content = list(
    tags$p("The samples in the first column of the uploaded file will be selected. The rest columns doesn't matter. Example:"),
    img(src = "sample_subset.png", width = 500)
))

callModule(pivot_help, "data_map", title ="What is a 'Data Map'", content = list(
    tags$p("A data map is PIVOT's unique way of managing your analysis dataset."),
    tags$p("As you filter or subset the input data based on various criteria, you are creating multiple data nodes."),
    tags$p("The data nodes and their relationships are presented as a tree graph ('data map')."),
    tags$p("Mouse over each node/edge in the map will show you the attributes of that node/edge."),
    tags$p("By selecting a particular node, you will be able to add notes, rename node, switch to that dataset or delete it."),
    tags$p("In analysis modules, if you click the magnet button on the top-right, PIVOT will create and link an analysis report to the map."),
    tags$p("If you need further information, please check the PIVOT manual.")
))

callModule(pivot_help, "edgeR_compare", title ="How to compare your groups?", content = list(
    tags$b("If use exact test:"),
    tags$p("Simply choose which pair to compare."),
    tags$b("If use GLM model:"),
    tags$p("If model contains intercept (formula ~0+XYZ), use contrast, e.g., contrast: (1)A (-1)B) for pairwise group comparison"),
    tags$p("If model does not contain intercept (default), the first group will be treated as the baseline. The coefficient B will be B vs A and coefficient C will be C vs A. To compare B vs C use contrast: (1)B (-1)C."),
    tags$p("Please see edgeR manual for more detailed explanation.")
))

