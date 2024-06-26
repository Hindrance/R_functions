function_list = c(
"add_alpha.r",
#"aggregate_clusters.r",
#"aggregate_duplicate_genes.r",
#"aggregate_peaks.r",
#"apw.r",
"arrow_overlay.r",
"barplot_matrix.r",
"bars.r",
"bin_data.r",
"cell_types.r",
"cmy.r",
"collect_points.r",
"colourLibrary.r",
"colour_scale_map.r",
#"content_parser.r",
#"convert.r",
#"DESeq_normalise.r",
"dot_plot.r",
"downsample_cells.r",
"expression.so.r",
"extract_top_Seurat_markers.r",
#"FQnorm.r",
#"gene_colour.r",
#"genomic_features.r",
"geom_mean.r",
"g.r",
"gradient_legend.r",
"gradient_line.r",
#"hist_boundaries.r",
#"kde_height.r",
"legend_3.r",
"legend_4.r",
"line_error_plot.r",
#"load_functions.r",
"MA.r",
"matrix_overlay.r",
"matrix_plot.r",
"mkdir.r",
#"nbc_model_tools.r",
"normalise.r",
"object_ram.r",
"orthogonal_residuals.r",
"partition_samples.r",
"plotcols.r",
"plot_density.r",
#"plotHL.r",
#"plot_model_kdes.r",
"plot_plus.r",
"plotSmooth_custom.r",
"plot_so.r",
#"plot_volcano.r",
"plot_volcano_v2.r",
"quickQC.r",
"remove_low_expression.r",
"remove_low_expression_var.r",
"remove_outlier_expression.r",
#"returnGO.r",
#"rf_optim.r",
"sample_equal.r",
#"searchGOTerm.r",
"seq_log.r",
"shuffle.r",
"similar.r",
"spectral_clustering.r",
"stratified_sample.r",
"stratified_subsampling",
"subplot_coords.r",
"sub_sample_sequential.r",
"subset_list.r",
"text_overlay.r",
"update_genes.r",
"update_labels.r",
"version_source.r",
"volcano_curve_optim.r",
"volcano_curve_plot.r",
"volcano_curve.r"
)

func.dir = "https://raw.githubusercontent.com/Hindrance/R_functions/main/functions/"

func.list <- list()
for(i in function_list){
  func.list[[i]] <- tryCatch(source(file.path(func.dir,i)), warning = function(w) {paste(i)}, error = function(e) {paste(i, "was not loaded properly, please check the code")})
} 

