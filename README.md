# Fregex

"**F**riendly **Reg**ular **Ex**pressions", or "**F**ilter **Reg**ular **Ex**pressions".

The Fregex laguage extends regular expressions with user-friendly combinators and is transpiled back into regular expressions.


## Usage


1. **From a file**
   Write a fregex expression in `input.txt` and run the following to convert it into a regular expression:

```bash
cat input.txt | xargs -0 ai-fregex
```

2. **Passing fregex as an argument**

```bash
ai-fregex $'<within sentence>\n  <year 2020s>\n  plan|goal'
```
## Fregex language

All regular expressions are admitted by Fregex.
In addition, it has the following combinators:

### `<within sentence>`

Is used to match such sentences, in which all arguments of `<within sentence>` occur.

### `<choice>`

At least one of the choices should be true.

### `<year future>`

Year in the future.

### `<year 21cent>`

Year in the 21st century.

### `<year 2020s>`

Year in the 20s.

### `<year>`

Any year.

**Example**
```
<within sentence>
  <choice>
    word1
    <year future>
  word3
```
Matches sentence with occurrence of "word1" and "word3", or any year in the future and "word3".
