all: thesis_draft.pdf

thesis_draft.pdf: thesis_draft.tex summary_stats.tex summary_stats_stations.tex shiny_app.tex summary_stats_others.tex summary_trip_distribution.tex citibike_script.tex master_stations.tex model_regression.tex regression_summary.tex netflow_tables.tex regression1.tex regression2.tex regression3.tex regression4.tex random_forest.tex regression_cluster.tex conclusion.tex references.bib
	pdflatex thesis_draft
	bibtex thesis_draft
	pdflatex thesis_draft
	bibtex thesis_draft
	pdflatex thesis_draft

clean: 
	rm *.idx *.ilg *.ind
	
summary_stats.tex: summary_stats.Rnw
	Rscript -e "knitr::knit('summary_stats.Rnw')"
summary_stats_others.tex: summary_stats_others.Rnw
	Rscript -e "knitr::knit('summary_stats_others.Rnw')"
summary_stats_stations.tex: summary_stats_stations.Rnw
	Rscript -e "knitr::knit('summary_stats_stations.Rnw')"
citibike_script.tex: citibike_script.Rnw
	Rscript -e "knitr::knit('citibike_script.Rnw')"
master_stations.tex: master_stations.Rnw
	Rscript -e "knitr::knit('master_stations.Rnw')"
summary_trip_distribution.tex: summary_trip_distribution.Rnw
	Rscript -e "knitr::knit('summary_trip_distribution.Rnw')"
shiny_app.tex: shiny_app.Rnw
	Rscript -e "knitr::knit('shiny_app.Rnw')"
model_regression.tex: model_regression.Rnw
	Rscript -e "knitr::knit('model_regression.Rnw')"
regression_summary.tex: regression_summary.Rnw
	Rscript -e "knitr::knit('regression_summary.Rnw')"
netflow_tables.tex: netflow_tables.Rnw
	Rscript -e "knitr::knit('netflow_tables.Rnw')"
regression1.tex: regression1.Rnw
	Rscript -e "knitr::knit('regression1.Rnw')"	
regression2.tex: regression2.Rnw
	Rscript -e "knitr::knit('regression2.Rnw')"	
regression3.tex: regression3.Rnw
	Rscript -e "knitr::knit('regression3.Rnw')"	
regression4.tex: regression4.Rnw
	Rscript -e "knitr::knit('regression4.Rnw')"	
regression_cluster.tex: regression_cluster.Rnw
	Rscript -e "knitr::knit('regression_cluster.Rnw')"
random_forest.tex: random_forest.Rnw
	Rscript -e "knitr::knit('random_forest.Rnw')"	
conclusion.tex: conclusion.Rnw
	Rscript -e "knitr::knit('conclusion.Rnw')"	
thesis_draft.tex: thesis_draft.Rnw
	Rscript -e "knitr::knit('thesis_draft.Rnw')"