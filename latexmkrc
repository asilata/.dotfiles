$pdf_mode = 1;
$pdflatex = "pdflatex  --shell-escape --synctex=1  %O  %S";
$bibtex_use = 2;

$pdf_update_method = 4;
$pdf_previewer="emacsclient -e '(ab/pdf-preview %S)'";
$pdf_update_command="emacsclient -e '(ab/pdf-preview %S)'";

# Time out the pvc compiler after a certain number of minutes of inactivity.
$pvc_timeout = 1;
$pvc_timeout_mins = 30;
