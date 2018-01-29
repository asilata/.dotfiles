$pdf_mode = 1;
$pdflatex = "pdflatex -interaction=nonstopmode -synctex=1 --shell-escape  %O  %S";
$bibtex_use = 2;
$pdf_previewer = 'open -a Skim';
@generated_exts = (@generated_exts, 'synctex.gz');