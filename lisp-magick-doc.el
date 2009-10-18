;;; lisp-magick-doc.el --- 

;; Copyright (C) 2007  Niels Giesen

;; Author: Niels Giesen <com dot gmail at niels dot giesen, in reversed order>
;; Keywords: lisp, multimedia, hypermedia, docs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; lisp-magick-doc is to be used with the common lisp library lisp-magick (available
;; http://www.nil.at/software/lisp-magick) which is an interface to ImageMagick
;; (http://www.imagemagic.org)

;; Requirements:

(eval-when (compile) (require 'cl))

;;; Code:
(defun lisp-magick-doc ()
  "Look up the C API reference for lisp-magick symbol at point."
  (interactive)
  (let* (case-fold-search
	 (magickal-name 
	  (if (string-match "^[A-Z]" (symbol-name-nearest-point))
	      (symbol-name-nearest-point)
	    (let ((symbol-name (capitalize (replace-regexp-in-string ".*:" "" (symbol-name-nearest-point)))))
	      (while
		  (string-match "-\\(.\\)" symbol-name)
		(setq symbol-name (replace-match (upcase (match-string 1 symbol-name)) nil t symbol-name)))
	      symbol-name)))
	 (subpage (car (rassoc* magickal-name 
				'(("pixel-iterator" . "ClearPixelIterator\\|DestroyPixelIterator\\|IsPixelIterator\\|NewPixel\\(?:\\(?:Region\\)?Iterator\\)\\|Pixel\\(?:ClearIteratorException\\|Get\\(?:CurrentIteratorRow\\|Iterator\\(?:Exception\\|Row\\)\\|\\(?:Next\\|Previous\\)IteratorRow\\)\\|ResetIterator\\|S\\(?:et\\(?:\\(?:\\(?:Fir\\|La\\)st\\)?IteratorRow\\)\\|yncIterator\\)\\)")
				  ("magick-wand" . "Cl\\(?:\\(?:ear\\|one\\)MagickWand\\)\\|DestroyMagickWand\\|IsMagickWand\\|Magick\\(?:ClearException\\|GetIteratorIndex\\|Query\\(?:ConfigureOptions?\\|\\(?:Font\\(?:Metric\\)?\\|MultilineFontMetric\\)s\\)\\|Re\\(?:linquishMemory\\|setIterator\\)\\|Set\\(?:FirstIterator\\|IteratorIndex\\|LastIterator\\)\\|Wand\\(?:\\(?:Genesi\\|Terminu\\)s\\)\\)\\|NewMagickWand\\(?:WithImage\\)?")
				  ("magick-deprecate" . "Draw\\(?:Get\\(?:\\(?:Fill\\|Stroke\\)Alpha\\)\\|P\\(?:eekGraphicWand\\|\\(?:op\\|ush\\)GraphicContext\\)\\|Set\\(?:\\(?:Fill\\|Stroke\\)Alpha\\)\\)\\|Magick\\(?:ColorFloodfillImage\\|DescribeImage\\|GetImage\\(?:\\(?:Attribut\\|Siz\\)e\\)\\|MatteFloodfillImage\\|OpaqueImage\\|RegionOfInterestImage\\|SetImage\\(?:Attribute\\|Index\\|VirtualPixelMethod\\)\\|TransparentImage\\|WriteImageBlob\\)\\|Pixel\\(?:GetNextRow\\|IteratorGetException\\)")
				  ("pixel-wand" . "ClearPixelWand\\|DestroyPixelWands?\\|IsPixelWand\\(?:Similar\\)?\\|NewPixelWands?\\|Pixel\\(?:ClearException\\|Get\\(?:Alpha\\(?:Quantum\\)?\\|Bl\\(?:ack\\(?:Quantum\\)?\\|ue\\(?:Quantum\\)?\\)\\|C\\(?:olor\\(?:As\\(?:\\(?:Normalized\\)?String\\)\\|Count\\)\\|yan\\(?:Quantum\\)?\\)\\|Exception\\|Fuzz\\|Green\\(?:Quantum\\)?\\|HSL\\|Index\\|Magenta\\(?:Quantum\\)?\\|Opacity\\(?:Quantum\\)?\\|QuantumColor\\|Red\\(?:Quantum\\)?\\|Yellow\\(?:Quantum\\)?\\)\\|Set\\(?:Alpha\\(?:Quantum\\)?\\|Bl\\(?:ack\\(?:Quantum\\)?\\|ue\\(?:Quantum\\)?\\)\\|C\\(?:olor\\(?:Count\\)?\\|yan\\(?:Quantum\\)?\\)\\|Fuzz\\|Green\\(?:Quantum\\)?\\|HSL\\|Index\\|Magenta\\(?:Quantum\\)?\\|Opacity\\(?:Quantum\\)?\\|QuantumColor\\|Red\\(?:Quantum\\)?\\|Yellow\\(?:Quantum\\)?\\)\\)")
				  ("magick-property" . "Magick\\(?:Get\\(?:Antialias\\|Co\\(?:mpression\\(?:Quality\\)?\\|pyright\\)\\|Exception\\|F\\(?:ilename\\|ormat\\)\\|HomeURL\\|Inter\\(?:laceScheme\\|polateMethod\\)\\|O\\(?:\\(?:p\\|rienta\\)tion\\)\\|Pa\\(?:\\(?:ckageNam\\|g\\)e\\)\\|Quantum\\(?:Depth\\|Range\\)\\|Re\\(?:leaseDate\\|source\\(?:Limit\\)?\\)\\|S\\(?:amplingFactors\\|ize\\(?:Offset\\)?\\)\\|Version\\)\\|Set\\(?:Antialias\\|BackgroundColor\\|Compression\\(?:Quality\\)?\\|Depth\\|F\\(?:ilename\\|ormat\\)\\|Inter\\(?:laceScheme\\|polateMethod\\)\\|O\\(?:\\(?:p\\|rienta\\)tion\\)\\|P\\(?:a\\(?:\\(?:g\\|ssphras\\)e\\)\\|rogressMonitor\\)\\|Reso\\(?:lution\\|urceLimit\\)\\|S\\(?:amplingFactors\\|ize\\(?:Offset\\)?\\)\\|Type\\)\\)")
				  ("drawing-wand" . "Cl\\(?:\\(?:ear\\|one\\)DrawingWand\\)\\|D\\(?:estroyDrawingWand\\|raw\\(?:A\\(?:ffine\\|nnotation\\|rc\\)\\|Bezier\\|C\\(?:ircle\\|learException\\|o\\(?:lor\\|m\\(?:ment\\|posite\\)\\)\\)\\|Ellipse\\|Get\\(?:Clip\\(?:Path\\|Rule\\|Units\\)\\|Exception\\|F\\(?:ill\\(?:Color\\|Opacity\\|Rule\\)\\|ont\\(?:Family\\|S\\(?:ize\\|t\\(?:retch\\|yle\\)\\)\\|Weight\\)?\\)\\|Gravity\\|Stroke\\(?:Antialias\\|Color\\|Dash\\(?:Array\\|Offset\\)\\|Line\\(?:Cap\\|Join\\)\\|MiterLimit\\|Opacity\\|Width\\)\\|Text\\(?:A\\(?:lignment\\|ntialias\\)\\|Decoration\\|Encoding\\|UnderColor\\)\\|VectorGraphics\\)\\|Line\\|Matte\\|P\\(?:ath\\(?:C\\(?:\\(?:los\\|urveTo\\(?:Absolut\\|QuadraticBezier\\(?:Absolut\\|Relativ\\|SmoothAbsolut\\)\\|Relativ\\|Smooth\\(?:Absolut\\|Relativ\\)\\)\\)e\\)\\|EllipticArc\\(?:\\(?:Absolut\\|Relativ\\)e\\)\\|Finish\\|LineTo\\(?:\\(?:Absolut\\|Horizontal\\(?:Absolut\\|Relativ\\)\\|Relativ\\|Vertical\\(?:Absolut\\|Relativ\\)\\)e\\)\\|MoveTo\\(?:\\(?:Absolut\\|Relativ\\)e\\)\\|Start\\)\\|o\\(?:int\\|ly\\(?:gon\\|line\\)\\|p\\(?:ClipPath\\|Defs\\|Pattern\\)\\)\\|ush\\(?:ClipPath\\|Defs\\|Pattern\\)\\)\\|R\\(?:e\\(?:ctangle\\|nder\\|setVectorGraphics\\)\\|o\\(?:\\(?:tat\\|undRectangl\\)e\\)\\)\\|S\\(?:cale\\|et\\(?:Clip\\(?:Path\\|Rule\\|Units\\)\\|F\\(?:ill\\(?:Color\\|Opacity\\|PatternURL\\|Rule\\)\\|ont\\(?:Family\\|S\\(?:ize\\|t\\(?:retch\\|yle\\)\\)\\|Weight\\)?\\)\\|Gravity\\|Stroke\\(?:Antialias\\|Color\\|Dash\\(?:Array\\|Offset\\)\\|Line\\(?:Cap\\|Join\\)\\|MiterLimit\\|Opacity\\|PatternURL\\|Width\\)\\|Text\\(?:A\\(?:lignment\\|ntialias\\)\\|Decoration\\|Encoding\\|UnderColor\\)\\|V\\(?:ectorGraphics\\|iewbox\\)\\)\\|kew[XY]\\)\\|Translate\\)\\)\\|\\(?:Is\\|New\\|P\\(?:eek\\|op\\|ush\\)\\)DrawingWand")
				  ("magick-image" . "GetImageFromMagickWand\\|Magick\\(?:A\\(?:d\\(?:\\(?:aptive\\(?:Blur\\|Resize\\|Sharpen\\|Threshold\\)\\|d\\(?:Noise\\)?\\)Image\\)\\|ffineTransformImage\\|n\\(?:imateImages\\|notateImage\\)\\|\\(?:ppend\\|verage\\)Images\\)\\|B\\(?:\\(?:l\\(?:ackThreshold\\|ur\\)\\|order\\)Image\\)\\|C\\(?:h\\(?:\\(?:arcoal\\|op\\)Image\\)\\|lip\\(?:\\(?:Path\\)?Image\\)\\|o\\(?:alesceImages\\|lorizeImage\\|m\\(?:bineImages\\|mentImage\\|p\\(?:areImage\\(?:\\(?:Channel\\|Layer\\)s\\)?\\|ositeImage\\)\\)\\|n\\(?:\\(?:stitute\\|trast\\(?:Stretch\\)?\\|volve\\)Image\\)\\)\\|\\(?:ro\\|ycleColorma\\)pImage\\)\\|D\\(?:e\\(?:constructImages\\|s\\(?:\\(?:peckle\\|troy\\)Image\\)\\)\\|isplayImages?\\|rawImage\\)\\|E\\(?:\\(?:dge\\|mboss\\|nhance\\|qualize\\|valuate\\|xtent\\)Image\\)\\|F\\(?:l\\(?:attenImages\\|[io]pImage\\)\\|\\(?:rame\\|x\\)Image\\)\\|G\\(?:a\\(?:\\(?:mma\\|ussianBlur\\)Image\\)\\|et\\(?:Image\\(?:B\\(?:ackgroundColor\\|l\\(?:ob\\|uePrimary\\)\\|orderColor\\)\\|C\\(?:hannel\\(?:D\\(?:epth\\|istortion\\)\\|Mean\\|Range\\|Statistics\\)\\|lipMask\\|o\\(?:lor\\(?:mapColor\\|s\\(?:pace\\)?\\)\\|mp\\(?:ose\\|ression\\)\\)\\)\\|D\\(?:e\\(?:lay\\|pth\\)\\|is\\(?:pose\\|tortion\\)\\)\\|F\\(?:ilename\\|ormat\\)\\|G\\(?:amma\\|reenPrimary\\)\\|H\\(?:eight\\|istogram\\)\\|I\\(?:nter\\(?:laceScheme\\|polateMethod\\)\\|terations\\)\\|Length\\|Matte\\(?:Color\\)?\\|Orientation\\|P\\(?:age\\|ixel\\(?:Color\\|s\\)\\|ro\\(?:file\\|perty\\)\\)\\|Re\\(?:dPrimary\\|gion\\|nderingIntent\\|solution\\)\\|S\\(?:\\(?:cen\\|ignatur\\)e\\)\\|T\\(?:icksPerSecond\\|otalInkDensity\\|ype\\)\\|Units\\|VirtualPixelMethod\\|W\\(?:hitePoint\\|idth\\)\\)?\\|NumberImages\\)\\)\\|Has\\(?:\\(?:Next\\|Previous\\)Image\\)\\|I\\(?:\\(?:dentify\\|mplode\\)Image\\)\\|L\\(?:\\(?:abel\\|evel\\|inearStretch\\)Image\\)\\|M\\(?:a\\(?:\\(?:gnify\\|p\\)Image\\)\\|edianFilterImage\\|inifyImage\\|o\\(?:dulateImage\\|ntageImage\\|rphImages\\|saicImages\\|tionBlurImage\\)\\)\\|N\\(?:\\(?:e\\(?:gate\\|w\\|xt\\)\\|ormalize\\)Image\\)\\|O\\(?:ilPaintImage\\|ptimizeImageLayers\\|rderedPosterizeImage\\)\\|P\\(?:aint\\(?:\\(?:Floodfill\\|Opaque\\|Transparent\\)Image\\)\\|ingImage\\(?:Blob\\|File\\)?\\|o\\(?:\\(?:laroid\\|sterize\\)Image\\)\\|r\\(?:evi\\(?:ewImages\\|ousImage\\)\\|ofileImage\\)\\)\\|QuantizeImages?\\|R\\(?:a\\(?:\\(?:dialBlur\\|ise\\|ndomThreshold\\)Image\\)\\|e\\(?:adImage\\(?:Blob\\|File\\)?\\|\\(?:colorImag\\|duceNoiseImag\\|moveImag\\(?:eProfil\\)?\\|s\\(?:\\(?:ampleIm\\|etImageP\\|izeIm\\)ag\\)\\)e\\)\\|o\\(?:\\(?:ll\\|tate\\)Image\\)\\)\\|S\\(?:ampleImage\\|caleImage\\|e\\(?:gmentImage\\|p\\(?:arateImageChannel\\|iaToneImage\\)\\|tImage\\(?:B\\(?:ackgroundColor\\|ias\\|luePrimary\\|orderColor\\)\\|C\\(?:hannelDepth\\|lipMask\\|o\\(?:lor\\(?:mapColor\\|space\\)\\|mp\\(?:ose\\|ression\\(?:Quality\\)?\\)\\)\\)\\|D\\(?:e\\(?:lay\\|pth\\)\\|ispose\\)\\|Extent\\|F\\(?:ilename\\|ormat\\)\\|G\\(?:amma\\|reenPrimary\\)\\|I\\(?:nter\\(?:laceScheme\\|polateMethod\\)\\|terations\\)\\|Matte\\(?:Color\\)?\\|O\\(?:pacity\\|rientation\\)\\|P\\(?:age\\|ixels\\|ro\\(?:file\\|gressMonitor\\|perty\\)\\)\\|Re\\(?:dPrimary\\|nderingIntent\\|solution\\)\\|Scene\\|T\\(?:icksPerSecond\\|ype\\)\\|Units\\|WhitePoint\\)?\\)\\|\\(?:h\\(?:a\\(?:d\\(?:e\\|ow\\)\\|rpen\\|ve\\)\\|ear\\)\\|igmoidalContrast\\|ketch\\|olarize\\|p\\(?:lice\\|read\\)\\|t\\(?:e\\(?:\\(?:gan\\|re\\)o\\)\\|rip\\)\\|wirl\\)Image\\)\\|T\\(?:\\(?:exture\\|h\\(?:reshold\\|umbnail\\)\\|int\\|r\\(?:ans\\(?:form\\|\\(?:po\\|ver\\)se\\)\\|im\\)\\)Image\\)\\|Un\\(?:iqueImageColors\\|sharpMaskImage\\)\\|VignetteImage\\|W\\(?:aveImage\\|hiteThresholdImage\\|riteImage\\(?:File\\|s\\(?:File\\)?\\)?\\)\\)"))
				:test (lambda (key val) (string-match val key))))))
    (if subpage 
	(browse-url (format "http://www.imagemagick.org/api/%s.php#%s" subpage magickal-name))
      (message "No documentation available for %s" magickal-name))))

(provide 'lisp-magick-doc)
;;; lisp-magick-doc.el ends here

