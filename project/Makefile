paperfilename=paper
presfilename=pres

all: paper presentation

clean:
	rm -rf *.aux *.bbl *.blg *-blx.bib *.gz *.log *.nav *.out *.snm *.toc *.tex *.xml .Rhistory *~ $(paperfilename)_figure $(paperfilename)_cache $(presfilename)_figure $(presfilename)_cache

paper: $(paperfilename).pdf

$(paperfilename).pdf: $(paperfilename).tex references.bib
	pdflatex $(paperfilename).tex
	bibtex $(paperfilename)
	pdflatex $(paperfilename).tex
	pdflatex $(paperfilename).tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('$(paperfilename).pdf')"

$(paperfilename).tex: $(paperfilename).rnw
	Rscript --vanilla -e "require(knitr);knit('$(paperfilename).rnw')"

$(paperfilename).rnw:

presentation: $(presfilename).pdf

$(presfilename).pdf: $(presfilename).tex references.bib
	pdflatex $(presfilename).tex
	bibtex $(presfilename)
	pdflatex $(presfilename).tex
	pdflatex $(presfilename).tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('$(presfilename).pdf')"

$(presfilename).tex: $(presfilename).rnw
	Rscript --vanilla -e "require(knitr);knit('$(presfilename).rnw')"

$(presfilename).rnw:

references.bib:
