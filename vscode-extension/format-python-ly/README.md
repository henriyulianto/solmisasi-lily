Format with python-ly
=====================

This small VS Code extension adds a Command Palette entry "Format with python-ly" which runs the workspace task
`Format current LilyPond file (python-ly)`.

Installation
------------

1. Install `vsce` if you don't have it: `npm install -g vsce`
2. Package the extension:

```bash
cd vscode-extension/format-python-ly
vsce package
# creates format-python-ly-0.0.1.vsix
```

3. Install the generated VSIX in VS Code: Command Palette → "Extensions: Install from VSIX..." → pick the `.vsix` file.

Usage
-----

Open Command Palette and run: `Format with python-ly`.
