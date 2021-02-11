## figs        : Generates all figures
figs: figures/fig1.tiff figures/fig2.tiff figures/fig3.tiff figures/fig4.tiff figures/figS1.tiff figures/figS2.tiff

## report.html : Generates html version of the data analysis report
report.html: report.Rmd
	Rscript -e 'rmarkdown::render("$<")'

figures/fig1.tiff: output/emm_pc_FN_BMD_time.csv output/emm_pc_LS_BMD_time.csv output/emm_pc_TH_BMD_time.csv output/emm_pc_TR_BMD_time.csv figures/fig1.R
	cd figures;R CMD BATCH fig1.R

figures/fig2.tiff: output/emm_whole_body_total_mass_time.csv output/emm_steps_time.csv output/emm_gravitational_loading_time.csv output/emm_sclerostin_time.csv figures/fig2.R
	cd figures;R CMD BATCH fig2.R

figures/fig3.tiff: output/emm_MVPA_min_time.csv output/emm_high_impacts_time.csv figures/fig3.R
	cd figures;R CMD BATCH fig3.R

figures/fig4.tiff: output/emm_whole_body_fat_mass_time.csv output/emm_whole_body_lean_mass_time.csv figures/fig4.R
	cd figures;R CMD BATCH fig4.R

figures/figS1.tiff: output/emm_SB_h_time.csv output/emm_LPA_h_time.csv output/emm_MVPA_min_time.csv figures/figS1.R
	cd figures;R CMD BATCH figS1.R

figures/figS2.tiff: data/df.csv figures/figS2.R
	cd figures;R CMD BATCH figS2.R

output/%.csv: data/df.csv code/functions/utils.R code/analysis.R
	cd code;R CMD BATCH analysis.R

## clean       : Removes auto-generated files
clean: 
	\rm -f figures/*.Rout figures/.Rdata code/analysis.Rout code/.RData *.html

## cleanall    : Removes auto-generated files, including data from output/
cleanall:
	\rm -f figures/*.Rout figures/.Rdata figures/*.tiff output/*.csv code/analysis.Rout code/.RData *.html

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
