all: report.docx

report.docx: report.md template.docx
	pandoc +RTS -K512m -RTS report.md --to docx --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures --output report.docx --filter /usr/local/bin/pandoc-citeproc --highlight-style tango --bibliography references.bib --reference-docx template.docx

report.md: report.Rmd
	Rscript -e "knitr::knit('report.Rmd')"
