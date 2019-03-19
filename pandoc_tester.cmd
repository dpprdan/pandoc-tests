"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" --version
:: HTML4
:: 1. @znmeb's example
"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" process_raw.md --from markdown --to html4 --output process_raw.html --email-obfuscation none --self-contained
:: 2. remove `--self-contained`
"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" process_raw.md --from markdown --to html4 --output process_raw.html --email-obfuscation none
:: 3. remove `--email-obfuscation none`
"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" process_raw.md --from markdown --to html4 --output process_raw.html --self-contained
:: 4. add `--standalone`
"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" process_raw.md --from markdown --to html4 --output process_raw.html --email-obfuscation none --self-contained --standalone
:: 5. remove `--self-contained`, add `--standalone`
"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" process_raw.md --from markdown --to html4 --output process_raw.html --email-obfuscation none --standalone

:: HTML5
:: 1. @znmeb's example
"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" process_raw.md --from markdown --to html5 --output process_raw.html --email-obfuscation none --self-contained
:: 2. remove `--self-contained`
"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" process_raw.md --from markdown --to html5 --output process_raw.html --email-obfuscation none
:: 3. remove `--email-obfuscation none`
"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" process_raw.md --from markdown --to html5 --output process_raw.html --self-contained
:: 4. add `--standalone`
"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" process_raw.md --from markdown --to html5 --output process_raw.html --email-obfuscation none --self-contained --standalone
:: 5. remove `--self-contained`, add `--standalone`
"C:/Users/daniel/Documents/rstudio-pandoc/pandoc" process_raw.md --from markdown --to html5 --output process_raw.html --email-obfuscation none --standalone

pause
