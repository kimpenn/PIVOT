

# The help button in PIVOT, fully modulized code

callModule(pivot_help, "directory", title = "How to input a count folder", content = list(
    tags$li("Accepts folder containing HTSeq/featureCounts/VERSE output."),
    tags$li("Counts files must contain the same feature set (Same number of rows, same rownames, multiple species not allowed)."),
    tags$li("You can use file filter to select files containing certain keywords"),
    tags$li("Example count file:"),
    br(),
    img(src = "exp_count_file.png", width = 150)
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



