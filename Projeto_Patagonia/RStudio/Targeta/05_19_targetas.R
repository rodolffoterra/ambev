##############
# Date: 05 19 2021
# Project: Targetas
##############


# 1.0 Packate----


library(pdftools)


# 2.0 Load pdf

download.file("Targeta/Dados/resumen_mensual_202104.pdf", "1403.2805.pdf")
txt <- pdf_text("Targeta/Dados/resumen_mensual_202104.pdf")

# first page text
cat(txt[1])


df <- as.data.frame(txt)


toc <- pdf_toc("1403.2805.pdf")



# Show as JSON
jsonlite::toJSON(toc, auto_unbox = TRUE, pretty = TRUE)


# Author, version, etc
info <- pdf_info("1403.2805.pdf")

# Table with fonts
fonts <- pdf_fonts("1403.2805.pdf")

# renders pdf to bitmap array
bitmap <- pdf_render_page("1403.2805.pdf", page = 1)

# save bitmap image
png::writePNG(bitmap, "page.png")
jpeg::writeJPEG(bitmap, "page.jpeg")
webp::write_webp(bitmap, "page.webp")
