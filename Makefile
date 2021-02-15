## figs        : Generates all figures
figs: figures/fig1.tiff figures/fig2.tiff figures/fig3.tiff figures/fig4.tiff figures/figS1.tiff figures/figS2.tiff

## install     : Install all necessary packages
install:
	Rscript -e 'renv::restore()'

## report.html : Generates html version of the data analysis report
report.html: report.Rmd
	Rscript -e 'rmarkdown::render("$<")'

figures/fig1.tiff: output/emm_pc_FN_BMD_time.csv output/emm_pc_LS_BMD_time.csv output/emm_pc_TH_BMD_time.csv output/emm_pc_TR_BMD_time.csv figures/fig1.R
	R CMD BATCH figures/fig1.R

figures/fig2.tiff: output/emm_whole_body_total_mass_time.csv output/emm_steps_time.csv output/emm_gravitational_loading_time.csv output/emm_sclerostin_time.csv figures/fig2.R
	R CMD BATCH figures/fig2.R

figures/fig3.tiff: output/emm_MVPA_min_time.csv output/emm_high_impacts_time.csv figures/fig3.R
	R CMD BATCH figures/fig3.R

figures/fig4.tiff: output/emm_whole_body_fat_mass_time.csv output/emm_whole_body_lean_mass_time.csv figures/fig4.R
	R CMD BATCH figures/fig4.R

figures/figS1.tiff: output/emm_SB_h_time.csv output/emm_LPA_h_time.csv output/emm_MVPA_min_time.csv figures/figS1.R
	R CMD BATCH figures/figS1.R

figures/figS2.tiff: data/df.csv figures/figS2.R
	R CMD BATCH figures/figS2.R

output/%.csv: data/df.csv code/functions/utils.R code/analysis.R
	R CMD BATCH code/analysis.R

## clean       : Removes auto-generated files
clean: 
	\rm -f *.Rout .Rdata

## cleanall    : Removes auto-generated files, including data from output/
cleanall:
	\rm -f *.Rout .Rdata figures/*.tiff output/*.csv *.html

.PHONY : help
help : Makefile
	@sed -n 's/^##//p' $<
