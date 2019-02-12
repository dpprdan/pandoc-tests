"C:/Program Files/Pandoc/pandoc" --version
:: HTML4
:: 1. @znmeb's example
"C:/Program Files/Pandoc/pandoc" process_raw.md --from markdown --to html4 --output process_raw.html --email-obfuscation none --self-contained
:: 2. remove `--self-contained`
"C:/Program Files/Pandoc/pandoc" process_raw.md --from markdown --to html4 --output process_raw.html --email-obfuscation none
:: 3. remove `--email-obfuscation none`
"C:/Program Files/Pandoc/pandoc" process_raw.md --from markdown --to html4 --output process_raw.html --self-contained
:: 4. add `--standalone`
"C:/Program Files/Pandoc/pandoc" process_raw.md --from markdown --to html4 --output process_raw.html --email-obfuscation none --self-contained --standalone
:: 5. remove `--self-contained`, add `--standalone`
"C:/Program Files/Pandoc/pandoc" process_raw.md --from markdown --to html4 --output process_raw.html --email-obfuscation none --standalone

:: HTML5
:: 1. @znmeb's example
"C:/Program Files/Pandoc/pandoc" process_raw.md --from markdown --to html5 --output process_raw.html --email-obfuscation none --self-contained
:: 2. remove `--self-contained`
"C:/Program Files/Pandoc/pandoc" process_raw.md --from markdown --to html5 --output process_raw.html --email-obfuscation none
:: 3. remove `--email-obfuscation none`
"C:/Program Files/Pandoc/pandoc" process_raw.md --from markdown --to html5 --output process_raw.html --self-contained
:: 4. add `--standalone`
"C:/Program Files/Pandoc/pandoc" process_raw.md --from markdown --to html5 --output process_raw.html --email-obfuscation none --self-contained --standalone
:: 5. remove `--self-contained`, add `--standalone`
"C:/Program Files/Pandoc/pandoc" process_raw.md --from markdown --to html5 --output process_raw.html --email-obfuscation none --standalone

pause
