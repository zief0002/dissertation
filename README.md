# My PhD Thesis

This is my PhD thesis. I am adding it here to show how to do certain things using RMarkdown and the `{gopherdown}` package. Note that the original data was deleted (as per IRB) and I simulated some data to show how to create figures/tables in Chapter 4, but did not do this for every aspect of the thesis. This also allowed me to show some LaTeX code for creating tables. I also took the liberty of producing some new tables and used `{ggplot2}` (rather than base R) for plotting, so this is not a perfect re-creation of my thesis. (A PDF version of the original thesis can be found [here](https://iase-web.org/Publications.php?p=Dissertations).)


The PDF version of the thesis is in the `_book/` directory.

## Files and Directories (Edit These)

The following files and directories are the ones I updated, edited, and added content to in order to customize my thesis:

### _bookdown.yml

This is the main configuration file for your thesis. Arrange the order of your
chapters in this file and ensure that the names match the names in your folders.

### index.Rmd

This file contains all the meta information (in the YAML) that helps create the thesis. I also included an R code chunk that loads all the packages and sets different global (e.g., scientific notation penalty) and knitr options (e.g., supress messages nd warnings in code chunks).

There are several YAML fields here to edit:

- `title:` Your thesis title goes here
- `author:` "Andrew Stefan Zieffler"  # Student's full legal name
- `month:` and `year:` Include the month and year that degree requirements were met 
-  `advisor:` Add your advisor here
- `coadvisor:` I did not have a coadvisor, so I left this line commented out.
- `phd: true` True since this is a PhD thesis
- `plan_b: false` False since this is a PhD thesis
- `knit:` The string `"bookdown::render_book"` builds the entire thesis when you click `knit` in the `index.Rmd` file.
- `output:` These lines use the `thesis_pdf()` function from `{gopherdown}` to render the thesis. The `latex-engine: xelatex` line uses the XeLaTeX engine which allows you to use a wider range of fonts.
- `mainfont:`, `sansfont:`, and `monofont:` Set the fonts to use in the thesis; the main font, sans-serif font, and mon-spaced (for code) fonts.
- `link-citations: true` Makes the citations in the thesis clickable and links them to the appropriate refere3nce in the *References* section 
- `colored-not-bordered-links: true` Uses colored links.
- `urlcolor-hex:`, `citecolor-hex:`, and `linkcolor-hex:` set the colors for URLs, in document citations, and links to sections in your thesis. Here I set these to `"2C6DAC"`, the HEX code for a nice blue color.
- `bibliography:` This gives the pathnames for the BIB files where you have embedded reference information. Here I set this to `["bib/lit-references.bib", "bib/methods-references.bib"]` which calls two BIB files, both located in the `bib/` directory.
- `csl:` This gives the pathname for your CSL file. Here I set this to `"csl/apa.csl"` which calls the APA CSL file located in the `csl/` directory.
- `citation_package:`, `biblatexoptions:`, and `biblio-style:` These are commented out because I am using Pandoc to create the citations and references. If you want to use BibLaTeX instead, you can uncomment these and set any options and style. You will also need to comment out the `csl:` YAML field since the CSL file is only used by Pandoc.
- `nocite:` Thes are citation keys for references I wanted included in the thesis' *References* section, but didn't actually call in the RMD files. This is mainly some references cited in Table 2.1 where I had to use some LaTeX syntax in the `longtable` environment and couldn't use typical RMD citations (e.g., using `[@cite-key]`).
- `lot: true` This adds the *List of Tables* to the front matter of your thesis.
- `lof: true` This adds the *List of Figures* to the front matter of your thesis.
- `header-includes:` This includes LaTeX syntax in the Preamble to the TEX document used to compile your thesis. I didn't need any additional LaTeX packages, so this is all commented out.


### pre/

This folder contains all of the RMD files to be included in the front matter of your dissertation (e.g., abstract, acknowledgements, dedication). Since I didn't have a dedication I could have deleted the file `00-dedication.Rmd`. However, having it there is not a big deal. I needed to comment out the lines in the YAML of `index.Rmd` where the dedication was being called.

### chapters/

This folder contains the RMD files for each chapter in the dissertation, as well as the appendices. These files contain the main content for the thesis.

### bib/

Store your bibliography as bibtex (.BIB) files here. I have the references for Chapters 1, 2, and 5 in the `lit-references.bib` file and those from Chapters 3 and 4 in `methods-references.bib`. If you have other .BIB files they can be included in this folder. You would also need to add them to the `bibliography:` YAML field in `index.Rmd`.

### csl/

I added the `apa.csl` style file here. (The APA style file is added by default when you create a new document using `{gopherdown}`. If you want to use a different style file grab it from [https://www.zotero.org/styles](https://www.zotero.org/styles) or [https://github.com/citation-style-language/styles](https://github.com/citation-style-language/styles) and add the CSL file to this folder. Then change the `csl:` YAML field in `index.Rmd`.

### figure/

Here I have stored figures that I don't create within the RMD files (e.g., figures I created in Keynote). I call them in the chapter RMD files using `knitr::include_graphics()`.

### data/

Here I stored data sets (e.g., CSV files) that I call in the chapter RMD files.


--

## Files and Directories (Do Not Edit)

The `{gopherdown}` package calls the `thesis_pdf()` function to create your thesis. It draws on the `template.tex` and `umnthesis.cls` files in the main directory. (Do not change these unless you know what you are doing!) When the thesis is rendered (clicking the `knit` button in the `index.Rmd` file), the `_book` and `_bookdown_files` directories are populated. 

### _book/

This directory includes your compiled thesis and all the relevant TEX files.

### _bookdown_files

This directory includes things that the `{bookdown}` package creates when the PDF is compiled (e.g., PDF versions of all figures).


**You should not edit these. To edit, change the RMD files or other files in the main directory.**
