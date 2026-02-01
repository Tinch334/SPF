
# SPF (Simple PDF)

## What is it?
Simple PDF, or SPF for short, is a simple document preparation system based on LaTeX, designed to make creating documents simple and quick.

## Installing & Running
SPF uses [**Stack**](https://docs.haskellstack.org/). Read the [installation guide](https://docs.haskellstack.org/en/stable/README/#how-to-install) for details. 

To set up the project, ensure you are in the project's directory `SPF` and execute:
```bash
stack setup

```

This will install the correct version of `GHC`. To build the project, execute:

```bash
stack build

```

This will install all necessary packages and compile the project. Keep in mind that both steps might take some time to finish.

Finally, to run the project:

```bash
stack exec SPF-exe -- <options>
```

## Project Structure

The project has the following structure:

```
.
|-- app
|    |-- Main.hs
|---src
|   |-- Common.hs
|   |-- Parser.hs
|   |-- Resources.hs
|       |-- Datatypes
|           |-- ....
|       |-- Typesetting
|           |-- ....
|       |-- Validation
|           |-- ....
|-- Examples
|     |-- ....
|-- README.md
|-- Setup.hs
|-- SPF.cabal
|-- package.yaml
|-- stack.yaml
|-- stack.yaml.lock

```

## Command Reference
This reference outlines the usage of SPF commands. An SPF document is split into three sections, which must appear in this order:
1. **Configuration**
2. **Metadata**
3. **Document Content**

**Comments:**
* Single line: `// Comment`
* Multi-line: `/* Comment */`

### 1. Configuration
The first section of the document allows you to set variables that control the default behavoiur during typesetting. **Syntax:** `\config{<variable>}[<key>:<value>, ...]`. Note that all specified values for the given option must be included. For example the command `\config{figurespacing}[after: 10]` is invalid, since the `before` value is missing.

| Variable | Description | Options & Values |
| --- | --- | --- |
| `size` | Page size | `{a4, a3, legal}` or `{width: <number>, height: <number>}` |
| `pagenumbering` | Page numbering style | `{arabic, roman, none}` |
| `sectionspacing` | Space around sections | `{before: <number>, after: <number>}` |
| `paragraphspacing` | Space around paragraphs | `{before: <number>, after: <number>}` |
| `listspacing` | Space around lists | `{before: <number>, after: <number>}` |
| `tablespacing` | Space around tables | `{before: <number>, after: <number>}` |
| `figurespacing` | Space around figures | `{before: <number>, after: <number>}` |
| `verbatimspacing` | Space around verbatim blocks | `{before: <number>, after: <number>}` |
| `parindent` | Paragraph indentation | `{indent: <number>}` |
| `font` | Document font | `{helvetica, times, courier}` |
| `parsize` | Paragraph font size | `{size: <number>}` |
| `titlesize` | Title font size | `{size: <number>}` |
| `sectionsize` | Section header size | `{size: <number>}` |
| `subsectionsize` | Subsection header size | `{size: <number>}` |
| `verbatimsize` | Verbatim font size | `{size: <number>}` |
| `justification` | Paragraph justification | `{left, right, center, full}` |
| `liststyle` | List item marker style | `{bullet, square, arrow, number}` |
| `vertmargin` | Vertical page margins | `{margin: <number>}` |
| `hozmargin` | Horizontal page margins | `{margin: <number>}` |
| `sectionnumbering` | Set section numbering | `{numbering: <true/false>}` |
| `figurenumbering` | Set figure numbering | `{numbering: <true/false>}` |
| `verbatimnumbering` | Set line numbering in verbatim environment | `{numbering: <true/false>}` |

### 2. Metadata
These commands set the properties of the documents title page and metadata. They do not accept options. 
* `\title{<text>}`: Sets the document title.
* `\author{<text>}`: Sets the document author.
* `\date{<text>}`: Sets the document date.

### 3. Document Content
These commands are used to structure and format the main body of the text, they may be used in any order.

#### Text & Formatting
For _regular_ text, that is to say text without any modifiers the text must be entered into the file without any commands. The following modifiers are available for text:
* `\bold{text}`: **Bold** text.
* `\italic{text}`: *Italic* text.
* `\emph{text}`: ***Emphasized*** text.

Some limitations on text commands:
- Text commands cannot be concatenated, the following is invalid: `\bold{\italic{Lorem ipsum}}`.
- Other commands cannot be used inside of a text command, the following is invalid: `\emph{Lorem ipsum \section{Vitae semper}}`
- An empty line between two blocks of text will cause them to be treated as separate paragraphs.

An example of valid text:
```
Lorem ipsum dolor sit amet, \italic{consectetur adipiscing elit.}
Mauris consectetur augue vitae neque aliquam fringilla.
Etiam eget sollicitudin sem.                                <--- One paragraph

Etiam eget sollicitudin sem. Nulla euismod \emph{consectetur consectetur.
Suspendisse ullamcorper} fringilla leo, vitae semper metus pretium quis.  <--- Separate paragraph
```

#### Sections & subsections
There are two commands that create sections and subsections respectively:
-   `\section{Title}[options]`: Creates a section header.
-   `\subsection{Title}[options]`: Creates a subsection header.

For both commands the options are: `font: <font>`, `size: <number>`.

#### Format
There are two commands that handle the :
* `\newpage`: Forces a page break.
* `\hline`: Draws a horizontal line.
    * **Options:** `width: <number>`(mandatory), `thickness: <number>`

#### Figures
In order to include images into the document the command used is: `\figure{<path>}[options]`, which inserts an image. The supported formats are: `jpg`, `png`, `bmp` and `svg`. The options are: `width: <number>`(mandatory), `caption: <text>`.

#### Lists
Creating lists is done by the use of the list environment:
```latex
\begin{list}
  \item Item one
  \item Item two
\end{list}
```
The option is: `style: {bullet, square, arrow, number}`.

#### Tables
Creating a table is done by the use of the table environment. Rows are separated by `\break` and columns by `|`:
```latex
\begin{table}[columns: 3]
  Header 1 | Header 2 | Header 3 \break
  Cell 1   | Cell 2   | Cell 3
\end{table}
```
The option is: `columns: <number>`(mandatory).

#### Verbatim (Code Blocks)
Creates a block of text rendered in a monospaced font, preserving whitespace and line breaks.

```latex
\begin{verbatim}
  Code goes here
  No formatting is applied
\end{verbatim}
```
**Options**: `size: <number>`, `numbering: {true, false}`.

#### Paragraphs
Paragraphs are normally created automatically by leaving a blank line between text. However, you can explicitly define them:
```latex
\begin{paragraph}
  This is a specific paragraph block.
\end{paragraph}
```
**Options**: `size: <number>`, `font: {helvetica, times, courier}` , `justification: {left, right, center, full}`.