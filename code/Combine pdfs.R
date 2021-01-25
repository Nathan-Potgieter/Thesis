# Combine title page with thesis
pacman::p_load("pdftools")
pdftools::pdf_combine(c("Template front page Masters[3187].pdf", "Thesis.pdf"),
                      output = "Complete Thesis.pdf")