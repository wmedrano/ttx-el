# ttx.el

`ttx.el` is an Emacs major mode for viewing TrueType and OpenType font files as XML. It leverages the `ttx` utility from [fonttools](https://github.com/fonttools/fonttools) to decompile font files into a readable XML format directly within Emacs.

## Features

- **Asynchronous Decompilation**: Uses `ttx` in the background to avoid freezing Emacs while decompiling large font files.
- **XML Integration**: Built on top of `nxml-mode`, providing syntax highlighting and XML navigation out of the box.
- **Automatic Activation**: Automatically associates with `.ttf` and `.otf` files.
- **Read-only by Default**: Protects the buffer while allowing easy viewing of the font structure.

## Requirements

- [fonttools](https://github.com/fonttools/fonttools): The `ttx` command must be installed and available in your system's `PATH`.

## Installation

Add `ttx.el` to your load path and require it:

```elisp
(add-to-list 'load-path "/path/to/ttx-el")
(require 'ttx)
```

## Usage

Simply open any `.ttf` or `.otf` file in Emacs. `ttx.el` will automatically trigger `ttx-mode`, decompile the font, and display the XML content.

To manually enable the mode:
`M-x ttx-mode`

To refresh the XML representation (e.g., if the font file changed):
`M-x revert-buffer`

## Customization

You can customize the path to the `ttx` executable if it's not in your `PATH`:

```elisp
(setq ttx-command "/path/to/ttx")
```

## License



[GPL-3.0](LICENSE)
