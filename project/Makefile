paperfilename=paper
presfilename=pres

all: $(paperfilename).pdf

clean:
	rm -rf *.aux *.bbl *.blg *-blx.bib *.gz *.log *.out *.tex *.xml .Rhistory *~ $(paperfilename)_figure $(paperfilename)_cache

$(paperfilename).pdf: $(paperfilename).tex references.bib
	pdflatex $(paperfilename).tex
	bibtex $(paperfilename)
	pdflatex $(paperfilename).tex
	pdflatex $(paperfilename).tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('$(paperfilename).pdf')"

$(paperfilename).tex: $(paperfilename).rnw
	Rscript --vanilla -e "require(knitr);knit('$(paperfilename).rnw')"

$(paperfilename).rnw:

references.bib:
