# truetype-clx
clx-truetype minus the clx, just outputs 2d arrays of alpha values with metrics.

TEXT-PIXARRAY works for single character strings, outputting just the glyph with no padding.

TEXT-LINE-PIXARRAY works for text strings and adds line height padding.

Font is a string designating the path to a ttf font.

Adjust *inch/pts* to your needs, currently set at 72.

e.g.
This is the output difference between text-line-pixarray and text-pixarray as output by the following command.
(visualize:this (truetype-clx:text-line-pixarray (font-match:find-match "deja") "J" 28 144 144) :surface *surface* :zoom 5 :gridp t :spacing 5 :second-spacing 10  :palette (palette:create-alpha-palette 256))

https://imgur.com/a/mYPkGp4

https://imgur.com/a/NBDlaVW
