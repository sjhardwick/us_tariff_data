
library(pdftools)
library(stringr)
library(readr)

# --- config
pdf_url <- "https://ustr.gov/sites/default/files/enforcement/301Investigations/Notice_of_Modification_%28List_4A_and_List_4B%29.pdf"
out_txt <- "data/temp/section301_list4.txt"  # output file with the "hs8: ..." string

# --- 1) download pdf to a temp file
tmp_pdf <- tempfile(fileext = ".pdf")
download.file(pdf_url, tmp_pdf, mode = "wb", quiet = TRUE)

# --- 2) extract text from all pages
txt_pages <- pdf_text(tmp_pdf)
txt_full  <- paste(txt_pages, collapse = "\n")

# --- 3) pull all HTSUS subheadings in dotted 4.2.2 format (e.g., 2710.19.30)
# - strict 8-digit pattern with dots
pat_hs8 <- "(?<!\\d)(\\d{4}\\.\\d{2}\\.\\d{2})(?!\\d)"
codes_dotted <- str_extract_all(txt_full, pat_hs8, simplify = FALSE)[[1]]

# --- 4) normalize to HS8 without dots and deduplicate
codes_hs8 <- gsub("\\.", "", codes_dotted)
codes_hs8 <- unique(codes_hs8)
codes_hs8 <- sort(codes_hs8)

# drop obvious Chapter 99 references if they ever appear
codes_hs8 <- codes_hs8[!startsWith(codes_hs8, "99")]

# --- 5) build the single string and write to file
hs8_string <- paste0("hs8: ", paste(codes_hs8, collapse = ", "))
write_lines(hs8_string, out_txt)

# --- 6) print a quick preview
cat("Found", length(codes_hs8), "HS8 codes.\n")
cat(substr(hs8_string, 1, 200), "...\n")
cat("Saved to:", normalizePath(out_txt), "\n")

###

list1 <- codes_hs8
list2 <- codes_hs8
list3 <- codes_hs8
list4 <- codes_hs8
codes_hs8 <- setdiff(list3, c(list1, list2))
hs8_string <- paste0("hs8: ", paste(new, collapse = ", "))

###

# install.packages("tesseract")
library(tesseract)
library(pdftools)

# 1. Convert PDF pages to images
img_files <- pdf_convert(
  pdf = "data/raw/301_exclusions_id24.pdf",
  dpi = 300,
  filenames = "page_%d.png"   # format string for output files
)

# 2. OCR each page
eng <- tesseract("eng")
texts <- vapply(img_files, ocr, character(1), engine = eng)

# 3. Combine into one text string
full_text <- paste(texts, collapse = "\n")

# 4. Extract 8-digit HTS codes in dotted format
codes_dotted <- str_extract_all(full_text, "\\d{4}\\.\\d{2}\\.\\d{2}")[[1]]
codes_dotted <- unique(codes_dotted)
