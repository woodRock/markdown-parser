# markdown-parser

![Haskell CI](https://github.com/woodRock/markdown-parser/workflows/Haskell%20CI/badge.svg)

A markdown parser written using the Haskell language and the Stack framework.

It uses an abstract syntax tree with functors, applicatives and alternatives from the standard haskell libraries.

## Running 

Open the interactive Haskell intepreter with the `MarkdownParser.hs` module.

```bash
ghci MarkdownParser.hs
```

Run this line within the interactive terminal.

```hs
parseFile "../test/markdown/blockquotes.md" file
```

## Features 

- [x] Headers (e.g., '# H1', '## H2')
- [x] Paragraphs 
- [x] Basic Lists 
- [x] Italics 
- [x] Bold
- [x] Horozontal Rule
- [x] Blockquotes
- [x] Codeblocks
- [x] File IO
- [ ] Nested List 
- [ ] Inline font styling
- [ ] Strikethrough
- [ ] Ordered List without ordered numbers
- [ ] Convert to another file type (html, latex)
