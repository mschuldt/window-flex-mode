
window-flex-mode grows the horizontal width of the selected window to
accommodate the width of the longest visable line.

This is intended to make it easy to work with many vertically
split windows when the average window width is less then the
possible length of text lines.

The width of the selected window is only expanded, ever reduced.

By default it will try to prevent large differences in window width
by balancing all windows before attempting to increase the width.
