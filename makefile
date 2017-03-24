all: thesis_draft.pdf

thesis_draft.pdf: thesis_draft.tex summary_stats.tex summary_stats_stations.tex summary_stats_others.tex citibike_script.tex references.bib
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
thesis_draft.tex: thesis_draft.Rnw
	Rscript -e "knitr::knit('thesis_draft.Rnw')"