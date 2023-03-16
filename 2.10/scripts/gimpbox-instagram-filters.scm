;
;
; Instagram filters for GIMP 2.10
;
; --------------------------------------------------------------------
;   - Changelog -
;
; --------------------------------------------------------------------
; MIT License
;
; Copyright (c) 2023 GimpBox https://github.com/gimpbox/
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define
    (script-fu-gimpbox-instagram-filters
        img
        draw
        filter
        bctype
        bs
        merge-layers
    )

    ; Check that active drawable is an image layer
    (if (and (= (car (gimp-item-is-group draw)) FALSE) (= (car (gimp-item-is-layer draw)) TRUE))
    (begin

    (if (= (car (gimp-drawable-is-rgb draw )) FALSE)
        (gimp-image-convert-rgb img)
    )
        
    ; Initialize an undo, so the process can be undone with a single undo
    (gimp-image-undo-group-start img)

    ;#############################################################
    ;
    ; Variables
    ;
    ;#############################################################
    (let*
       (
          (cl 0)                             ; color layer
          (lg 0)                             ; layer group
          (bwl 0)                            ; black and white layer
          (drawcopy 0)                       ; background layer
          (fgcolor 0)                        ; foreground color
          (bgcolor 0)                        ; background color
          (w (car (gimp-image-width img)))   ; image width
          (h (car (gimp-image-height img)))  ; image height
          (lat 0)                            ; size of the longest image side 
       )

    ; Determine the size of the longest image side
    (if (>= w h) (set! lat w))
    (if (> h w) (set! lat h))

    ;#############################################################
    ;
    ; Functions
    ;
    ;#############################################################
    (define (set-pt a index x y)
       (begin
          (aset a (* index 2) x)
          (aset a (+ (* index 2) 1) y)
       )
    )

    ;=============================================================
    ; Create layer group
    ;=============================================================
    (define (create-layer-group group-name)
       ; Create group layer
       (set! lg (car (gimp-layer-group-new img)))

       ; Insert the layer group to the image
       (gimp-item-set-name lg group-name)
       (gimp-image-insert-layer img lg 0 0)

       ; Copy source layer to the group add an alpha channel to the copied layer
       (set! drawcopy
          (car 
             (gimp-layer-copy draw TRUE )
          )
       )
       (gimp-image-insert-layer img drawcopy lg -1)
       (gimp-item-set-name drawcopy "Background")
    )

    ;############################################################
    ;
    ; M A I N
    ;
    ;############################################################
	
    ; Save foreground color
    (set! fgcolor (car (gimp-context-get-foreground)))
    ; Save background color
    (set! bgcolor (car (gimp-context-get-background)))
     
    ; ===========================================================
    ; 1977
    ; ===========================================================
    (if (= filter 0)
        (begin
        (create-layer-group "1977")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 26 'double)))
              (set-pt a 0 0.0 0.317647059)          ; 0/81
              (set-pt a 1 0.125490196 0.317647059)  ; 32/81
              (set-pt a 2 0.156862745 0.321568627)  ; 40/82
              (set-pt a 3 0.2 0.37254902)           ; 51/95
              (set-pt a 4 0.305882353 0.498039216)  ; 78/127
              (set-pt a 5 0.37254902 0.580392157)   ; 95/148
              (set-pt a 6 0.498039216 0.71372549)   ; 127/182
              (set-pt a 7 0.623529412 0.839215686)  ; 159/214
              (set-pt a 8 0.658823529 0.874509804)  ; 168/223
              (set-pt a 9 0.690196078 0.88627451)   ; 176/226
              (set-pt a 10 0.749019608 0.88627451)  ; 191/226
              (set-pt a 11 0.874509804 0.88627451)  ; 223/226
              (set-pt a 12 1.0 0.88627451)          ; 255/226
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 26 'double)))
              (set-pt a 0 0.0 0.223529412)          ; 0/57
              (set-pt a 1 0.125490196 0.223529412)  ; 32/57
              (set-pt a 2 0.176470588 0.223529412)  ; 45/57 
              (set-pt a 3 0.2 0.239215686)          ; 51/61 
              (set-pt a 4 0.250980392 0.290196078)  ; 64/74 
              (set-pt a 5 0.321568627 0.376470588)  ; 82/96 
              (set-pt a 6 0.435294118 0.501960784)  ; 111/128 
              (set-pt a 7 0.498039216 0.560784314)  ; 127/143 
              (set-pt a 8 0.623529412 0.682352941)  ; 159/174 
              (set-pt a 9 0.749019608 0.792156863)  ; 191/202 
              (set-pt a 10 0.839215686 0.878431373) ; 214/224 
              (set-pt a 11 0.929411765 0.945098039) ; 237/241 
              (set-pt a 12 1.0 0.988235294)         ; 255/252
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 26 'double)))
              (set-pt a 0 0.0 0.247058824)          ; 0/63 
              (set-pt a 1 0.125490196 0.247058824)  ; 32/63 
              (set-pt a 2 0.17254902 0.250980392)   ; 44/64 
              (set-pt a 3 0.211764706 0.266666667)  ; 54/68 
              (set-pt a 4 0.250980392 0.31372549)   ; 64/80 
              (set-pt a 5 0.376470588 0.478431373)  ; 96/122 
              (set-pt a 6 0.501960784 0.623529412)  ; 128/159 
              (set-pt a 7 0.615686275 0.752941176)  ; 157/192 
              (set-pt a 8 0.674509804 0.811764706)  ; 172/207 
              (set-pt a 9 0.690196078 0.82745098)   ; 176/211 
              (set-pt a 10 0.729411765 0.831372549) ; 186/212 
              (set-pt a 11 0.874509804 0.831372549) ; 223/212  
              (set-pt a 12 1.0 0.831372549)         ; 255/212 
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 26 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 26 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 26 (spline-blue)) 
        ) ; end begin
    ) ; end if 

    ;===========================================================
    ; Aden
    ;===========================================================
    (if (= filter 1)
       (begin
     (let*
        ( 
          (ml 0)                           ; mask layer
          (mask 0)                         ; mask
          (merged 0)                       ; merged layer
        )
       (create-layer-group "Aden")

       ;----------------------------------------------------
       ; Create mask layer
       ;----------------------------------------------------
       (set! ml (car (gimp-layer-copy draw TRUE )))
       (gimp-image-insert-layer img ml lg 0)
       (gimp-item-set-name ml "Color")

       ;----------------------------------------------------
       ; Add mask to mask layer
       ;----------------------------------------------------
       (set! mask (car (gimp-layer-create-mask ml ADD-MASK-COPY )))
       (gimp-layer-add-mask ml mask)
       ; Invert mask
       (gimp-drawable-invert mask FALSE) ; 

       ; Fill mask layer with foreground color
       (gimp-context-set-foreground '(66 10 14)) ; HTML #420a0e
       (gimp-edit-fill ml FILL-FOREGROUND)
       
       ; Set mask layer mode to 23 (overlay / ueberlagern)
       (gimp-layer-set-mode ml LAYER-MODE-OVERLAY) 

       ; Merge mask layer down
       (set! merged (car (gimp-image-merge-down img ml EXPAND-AS-NECESSARY)))

       ;----------------------------------------------------
       ; Modify hue, lightness and saturation, overlap 100%
       ;----------------------------------------------------
       (gimp-drawable-hue-saturation merged HUE-RANGE-ALL 0 0 -15 100)
       (gimp-drawable-hue-saturation merged HUE-RANGE-YELLOW -20 0 0 100)
       (gimp-drawable-hue-saturation merged HUE-RANGE-GREEN -20 0 0 100)
       (gimp-drawable-hue-saturation merged HUE-RANGE-CYAN -20 0 0 100)
       (gimp-drawable-hue-saturation merged HUE-RANGE-BLUE -20 0 0 100)

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.149019608)          ; 0/38 
             (set-pt a 1 0.121568627 0.211764706)  ; 31/54  
             (set-pt a 2 0.247058824 0.349019608)  ; 63/89 
             (set-pt a 3 0.37254902 0.482352941)   ; 95/123  
             (set-pt a 4 0.498039216 0.603921569)  ; 127/154 
             (set-pt a 5 0.623529412 0.71372549)   ; 159/182 
             (set-pt a 6 0.749019608 0.807843137)  ; 191/206 
             (set-pt a 7 0.874509804 0.88627451)   ; 223/226 
             (set-pt a 8 1.0 0.929411765)          ; 255/237
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.117647059)          ; 0/30 
             (set-pt a 1 0.121568627 0.2)          ; 31/51 
             (set-pt a 2 0.247058824 0.337254902)  ; 63/86 
             (set-pt a 3 0.37254902 0.466666667)   ; 95/119
             (set-pt a 4 0.498039216 0.576470588)  ; 127/147 
             (set-pt a 5 0.623529412 0.682352941)  ; 159/174 
             (set-pt a 6 0.749019608 0.784313725)  ; 191/200 
             (set-pt a 7 0.874509804 0.862745098)  ; 223/220 
             (set-pt a 8 1.0 0.91372549)           ; 255/233
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.145098039)          ; 0/37 
             (set-pt a 1 0.121568627 0.231372549)  ; 31/59 
             (set-pt a 2 0.247058824 0.337254902)  ; 63/86 
             (set-pt a 3 0.37254902 0.431372549)   ; 95/110 
             (set-pt a 4 0.498039216 0.537254902)  ; 127/137 
             (set-pt a 5 0.623529412 0.650980392)  ; 159/166 
             (set-pt a 6 0.749019608 0.756862745)  ; 191/193
             (set-pt a 7 0.874509804 0.847058824)  ; 223/216 
             (set-pt a 8 1.0 0.901960784)          ; 255/230
          a)
       )
       (gimp-drawable-curves-spline merged HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline merged HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline merged HISTOGRAM-BLUE 18 (spline-blue)) 
        
     ))) ;end let begin if

     ;===========================================================
     ; Amaro
     ;===========================================================
     (if (= filter 2)
     (begin
     (let*
        ( 
          (ml 0)                           ; mask layer
          (maskml 0)                       ; mask
          (nl 0)                           ; noise layer
          (masknl 0)                       ; noise layer mask
          (feather (* (/ lat 100) 30))     ; 30% of the longest image side
          (delta (* 5 (/ lat 100)))        ; 5% of the longest image side
        )
        (create-layer-group "Amaro")

        ;----------------------------------------------------
        ; Create color layer
        ;----------------------------------------------------
        ; Type: RGB
        ; Opacity: 70%
        ; Mode: 45 (soft light / weiche Kanten)     
        (set! cl (car (gimp-layer-new img w h RGB-IMAGE "Color" 70 LAYER-MODE-SOFTLIGHT)))
        
        ; Set foreground color
        (gimp-context-set-foreground '(78 12 112)) ; HTML #4e0c70

        ; Add the new layer to the group and fill the color layer with foreground color
        (gimp-image-insert-layer img cl lg -1)
        (gimp-drawable-fill cl 0)

        ;----------------------------------------------------
        ; Create mask layer
        ;----------------------------------------------------
        (set! ml (car (gimp-layer-copy draw TRUE )))
        (gimp-image-insert-layer img ml lg 0)
        (gimp-item-set-name ml "Mask")
        
        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.094117647)           ; 0/24 
              (set-pt a 1 0.121568627 0.219607843)   ; 31/56 
              (set-pt a 2 0.247058824 0.368627451)   ; 63/94 
              (set-pt a 3 0.37254902 0.576470588)    ; 95/147 
              (set-pt a 4 0.498039216 0.71372549)    ; 127/182 
              (set-pt a 5 0.623529412 0.764705882)   ; 159/195 
              (set-pt a 6 0.749019608 0.82745098)    ; 191/211
              (set-pt a 7 0.874509804 0.909803922)   ; 223/232 
              (set-pt a 8 1.0 0.988235294)           ; 255/252
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.003921569)           ; 0/1 
              (set-pt a 1 0.121568627 0.149019608)   ; 31/38 
              (set-pt a 2 0.247058824 0.337254902)   ; 63/86 
              (set-pt a 3 0.37254902 0.576470588)    ; 95/147 
              (set-pt a 4 0.498039216 0.725490196)   ; 127/185 
              (set-pt a 5 0.623529412 0.788235294)   ; 159/201 
              (set-pt a 6 0.749019608 0.847058824)   ; 191/216   
              (set-pt a 7 0.874509804 0.929411765)   ; 223/237 
              (set-pt a 8 1.0 0.996078431)           ; 255/254 
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.094117647)           ; 0/24 
              (set-pt a 1 0.121568627 0.250980392)   ; 31/64 
              (set-pt a 2 0.247058824 0.443137255)   ; 63/113 
              (set-pt a 3 0.37254902 0.592156863)    ; 95/151 
              (set-pt a 4 0.498039216 0.682352941)   ; 127/174 
              (set-pt a 5 0.623529412 0.733333333)   ; 159/187 
              (set-pt a 6 0.749019608 0.768627451)   ; 191/196  
              (set-pt a 7 0.874509804 0.862745098)   ; 223/220 
              (set-pt a 8 1.0 0.949019608)           ; 255/242  
           a)
        )
        (gimp-drawable-curves-spline ml HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline ml HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline ml HISTOGRAM-BLUE 18 (spline-blue)) 

        ;----------------------------------------------------
        ; Add mask to mask layer
        ;----------------------------------------------------
        (set! maskml (car (gimp-layer-create-mask ml ADD-MASK-BLACK )))
        (gimp-layer-add-mask ml maskml)
        (gimp-image-select-ellipse img CHANNEL-OP-ADD delta delta (- w (* 2 delta)) (- h (* 2 delta)))
        (gimp-selection-feather img feather)
        (gimp-context-set-foreground '(255 255 255))
        (gimp-edit-fill maskml FILL-FOREGROUND)
        (gimp-selection-clear img)
        
        ;----------------------------------------------------
        ; Create noise layer
        ;----------------------------------------------------
        ; Type: RGB
        ; Opacity: 100%
        ; Mode: 23 (overlay / ueberlagern)     
        (set! nl (car (gimp-layer-new img w h RGB-IMAGE "Noise" 100 LAYER-MODE-OVERLAY)))
        
        ; Set foreground color
        (gimp-context-set-foreground '(128 128 128)) ; HTML #808080

        ; Add the new layer to the group and fill the color layer with foreground color
        (gimp-image-insert-layer img nl lg -1)
        (gimp-drawable-fill nl 0)
        
        ;----------------------------------------------------
        ; Add noise
        ; Dulling: 4
        ; Hue: 0
        ; Saturation: 0
        ; Value: 0,4 (100)
        ;----------------------------------------------------
        (plug-in-hsv-noise 1 img nl 4 0 0 100) 
        (plug-in-gauss 1 img nl 2 2 1)
        ;(plug-in-gauss-rle 1 img nl 2 TRUE TRUE)
        ;----------------------------------------------------
        ; Add mask to noise layer
        ;----------------------------------------------------
        (set! masknl (car (gimp-layer-create-mask nl ADD-MASK-BLACK )))
        (gimp-layer-add-mask nl masknl)

        ; Select the original image, copy and paste it as a layer mask into the noise layer
        (gimp-selection-all img)
        (gimp-edit-copy drawcopy)
        (gimp-floating-sel-anchor (car (gimp-edit-paste masknl TRUE)))
        (gimp-selection-none img)
        
        ;----------------------------------------------------
        ; Set color curves of noise layer mask, so that only gray areas become grainy
        ;----------------------------------------------------
        ; Value
        (define (spline-value)
           (let* ((a (cons-array 6 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.501960784 0.501960784)   ; 128/128 
              (set-pt a 2 1.0 0.0)                   ; 255/0
           a)
        )
        (gimp-drawable-curves-spline masknl HISTOGRAM-VALUE 6 (spline-value))        
        
     ))) ; end let begin if

     ;===========================================================
     ; Apollo
     ;===========================================================
     (if (= filter 3)
     (begin
     (let*
        ( 
          (vl1 0)                        ; vignette layer
          (feather (* (/ lat 100) 30))   ; 30% of the longest image side
          (ex 0)                         ; x coordinate of upper-left corner of ellipse bounding box
          (ey 0)                         ; y coordinate of upper-left corner of ellipse bounding box
          (ew w)                         ; width of the ellipse
          (eh h)                         ; height of the ellipse
        )
        (create-layer-group "Apollo")
        ;----------------------------------------------------
        ; Copy source layer to the group 
        ;----------------------------------------------------
        (set! bwl (car (gimp-layer-copy draw TRUE )))
        (gimp-image-insert-layer img bwl lg 0)
        (gimp-item-set-name bwl "BlackWhite")

        ; Desaturate copied layer
        (gimp-drawable-desaturate bwl DESATURATE-LIGHTNESS)

        ; Set opacity to 50%
        (gimp-layer-set-opacity bwl 50)

        ;----------------------------------------------------
        ; Add vignette
        ;
        ; opacity: 65%
        ; mode: 28 (normal)
        ; color: black
        ;----------------------------------------------------

        ; Create vignette layer
        (set! vl1 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette" 65 LAYER-MODE-NORMAL)))
   
        ; Add vignette layer to the group
        (gimp-image-insert-layer img vl1 lg -1)

        ; Create vignette
        (gimp-image-select-ellipse img CHANNEL-OP-ADD ex ey ew eh)
        (gimp-selection-feather img feather)
        (gimp-selection-invert img)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill vl1 FILL-FOREGROUND)
        (gimp-selection-clear img)

        ; Set foreground color
        (gimp-context-set-foreground '(62 205 42)) ; HTML #3ecd2a

        ;----------------------------------------------------
        ; Create a color layer
        ;----------------------------------------------------
        ; Type: RGB
        ; Opacity: 40%
        ; Mode: 45 (soft light / weiche Kanten)
		;----------------------------------------------------
        (set! cl (car (gimp-layer-new img w h RGB-IMAGE "Color" 40 LAYER-MODE-SOFTLIGHT)))
        
        ; Add the new layer to the group and fill the color layer with foreground color
        (gimp-image-insert-layer img cl lg -1)
        (gimp-drawable-fill cl 0)

     ))) ; end let begin if
     
    ;===========================================================
    ; Ashby
    ;===========================================================
    (if (= filter 4)
        (begin
        (create-layer-group "Ashby")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.121568627 0.219607843)   ; 31/56 
              (set-pt a 2 0.247058824 0.392156863)   ; 63/100 
              (set-pt a 3 0.37254902 0.537254902)    ; 95/137 
              (set-pt a 4 0.498039216 0.647058824)   ; 127/165 
              (set-pt a 5 0.623529412 0.745098039)   ; 159/190 
              (set-pt a 6 0.749019608 0.835294118)   ; 191/213 
              (set-pt a 7 0.874509804 0.901960784)   ; 223/230 
              (set-pt a 8 1.0 0.952941176)           ; 255/243
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.121568627 0.2)           ; 31/51 
              (set-pt a 2 0.247058824 0.360784314)   ; 63/92 
              (set-pt a 3 0.37254902 0.490196078)    ; 95/125 
              (set-pt a 4 0.498039216 0.596078431)   ; 127/152 
              (set-pt a 5 0.623529412 0.68627451)    ; 159/175 
              (set-pt a 6 0.749019608 0.764705882)   ; 191/195 
              (set-pt a 7 0.874509804 0.823529412)   ; 223/210 
              (set-pt a 8 1.0 0.882352941)           ; 255/225 
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.121568627 0.168627451)   ; 31/43 
              (set-pt a 2 0.247058824 0.305882353)   ; 63/78 
              (set-pt a 3 0.37254902 0.423529412)    ; 95/108 
              (set-pt a 4 0.498039216 0.525490196)   ; 127/134 
              (set-pt a 5 0.623529412 0.607843137)   ; 159/155 
              (set-pt a 6 0.749019608 0.698039216)   ; 191/178 
              (set-pt a 7 0.874509804 0.756862745)   ; 223/193 
              (set-pt a 8 1.0 0.784313725)           ; 255/200 
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 
        ) ; end begin
    ) ; end if 

     ;===========================================================
     ; Brannan
     ;===========================================================
     (if (= filter 5)
        (begin
        (create-layer-group "Brannan")
        (let*
           ( 
             (merged 0)                     ; merged layer
             (vl1 0)                        ; vignette layer 1
             (feather (* (/ lat 100) 30))   ; 30% of the longest image side
             (delta (* (/ lat 100) 5))      ; 5% of the longest image side
             (ex (+ 0 delta))               ; x coordinate of upper-left corner of ellipse bounding box
             (ey (+ 0 delta))               ; y coordinate of upper-left corner of ellipse bounding box
             (ew (- w (* delta 2)))         ; width of the ellipse
             (eh (- h (* delta 2)))         ; height of the ellipse            
           )        

        ;----------------------------------------------------
        ; Create and copy black and white layer to the group
        ;----------------------------------------------------
        (set! bwl (car (gimp-layer-copy draw TRUE )))
        (gimp-image-insert-layer img bwl lg 0)
        (gimp-item-set-name bwl "BlackWhite")

        ; Desaturate copied layer
        (gimp-drawable-desaturate bwl DESATURATE-LIGHTNESS)

        ; Set opacity to 37%
        (gimp-layer-set-opacity bwl 37)
        (gimp-layer-set-mode bwl LAYER-MODE-OVERLAY)

        ; Adjust hue saturation of background
        (gimp-drawable-hue-saturation drawcopy HUE-RANGE-ALL 0 0 -30 100)

        ; Merge layer
        (set! merged (car (gimp-image-merge-down img bwl EXPAND-AS-NECESSARY)))

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.141176471)           ; 0/36 
              (set-pt a 1 0.121568627 0.196078431)   ; 31/50 
              (set-pt a 2 0.247058824 0.298039216)   ; 63/76 
              (set-pt a 3 0.37254902 0.458823529)    ; 95/117 
              (set-pt a 4 0.498039216 0.650980392)   ; 127/166 
              (set-pt a 5 0.62745098 0.831372549)    ; 160/212 
              (set-pt a 6 0.749019608 0.945098039)   ; 191/241 
              (set-pt a 7 0.874509804 0.992156863)   ; 223/253 
              (set-pt a 8 1.0 1.0)                   ; 255/255
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.003921569)           ; 0/1 
              (set-pt a 1 0.121568627 0.08627451)    ; 31/22 
              (set-pt a 2 0.247058824 0.235294118)   ; 63/60 
              (set-pt a 3 0.37254902 0.458823529)    ; 95/117 
              (set-pt a 4 0.498039216 0.650980392)   ; 127/166 
              (set-pt a 5 0.623529412 0.77254902)    ; 159/197 
              (set-pt a 6 0.749019608 0.870588235)   ; 191/222 
              (set-pt a 7 0.874509804 0.949019608)   ; 223/242 
              (set-pt a 8 1.0 0.988235294)           ; 255/252
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.129411765)           ;  0/33 
              (set-pt a 1 0.121568627 0.17254902)    ; 31/44 
              (set-pt a 2 0.247058824 0.28627451)    ; 63/73 
              (set-pt a 3 0.37254902 0.462745098)    ; 95/118 
              (set-pt a 4 0.498039216 0.580392157)   ; 127/148 
              (set-pt a 5 0.623529412 0.670588235)   ; 159/171 
              (set-pt a 6 0.749019608 0.752941176)   ; 191/192 
              (set-pt a 7 0.874509804 0.847058824)   ; 223/216 
              (set-pt a 8 1.0 0.937254902)           ; 255/239
           a)
        )
        (gimp-drawable-curves-spline merged HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline merged HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline merged HISTOGRAM-BLUE 18 (spline-blue)) 

        ;----------------------------------------------------
        ; Add vignette
        ;
        ; opacity: 50%
        ; mode: 45 (soft light / Weiche Kanten)
        ; color: black
        ;----------------------------------------------------

        ; Create vignette layer
        (set! vl1 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette" 50 LAYER-MODE-SOFTLIGHT)))
   
        ; Add vignette layer to the group
        (gimp-image-insert-layer img vl1 lg -1)

        ; Create vignette
        (gimp-image-select-ellipse img CHANNEL-OP-ADD ex ey ew eh)
        (gimp-selection-feather img feather)
        (gimp-selection-invert img)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill vl1 FILL-FOREGROUND)
        (gimp-selection-clear img)

     ))) ; end let begin if

    ;===========================================================
    ; Brooklyn
    ;===========================================================
    (if (= filter 6)
        (begin
        (create-layer-group "Brooklyn")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.015686275)          ; 0/4 
              (set-pt a 1 0.121568627 0.28627451)   ; 31/73 
              (set-pt a 2 0.247058824 0.384313725)  ; 63/98 
              (set-pt a 3 0.37254902 0.490196078)   ; 95/125 
              (set-pt a 4 0.498039216 0.607843137)  ; 127/155 
              (set-pt a 5 0.623529412 0.721568627)  ; 159/184 
              (set-pt a 6 0.749019608 0.854901961)  ; 191/218 
              (set-pt a 7 0.874509804 0.968627451)  ; 223/247 
              (set-pt a 8 1.0 0.992156863)          ; 255/253
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.0)                  ; 0/0 
              (set-pt a 1 0.121568627 0.117647059)  ; 31/30 
              (set-pt a 2 0.247058824 0.341176471)  ; 63/87 
              (set-pt a 3 0.37254902 0.537254902)   ; 95/137 
              (set-pt a 4 0.498039216 0.682352941)  ; 127/174 
              (set-pt a 5 0.623529412 0.780392157)  ; 159/199 
              (set-pt a 6 0.749019608 0.854901961)  ; 191/218 
              (set-pt a 7 0.874509804 0.925490196)  ; 223/236 
              (set-pt a 8 1.0 0.988235294)          ; 255/252
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.007843137)          ; 0/2 
              (set-pt a 1 0.121568627 0.192156863)  ; 31/49 
              (set-pt a 2 0.247058824 0.419607843)  ; 63/107 
              (set-pt a 3 0.37254902 0.541176471)   ; 95/138 
              (set-pt a 4 0.498039216 0.623529412)  ; 127/159 
              (set-pt a 5 0.623529412 0.705882353)  ; 159/180 
              (set-pt a 6 0.749019608 0.784313725)  ; 191/200 
              (set-pt a 7 0.874509804 0.862745098)  ; 223/220 
              (set-pt a 8 1.0 0.925490196)          ; 255/236
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 
        ) ; end begin
    ) ; end if 

    ;===========================================================
    ; Charmes
    ;===========================================================
    (if (= filter 7)
        (begin
        (create-layer-group "Charmes")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.149019608)          ; 0/38 
              (set-pt a 1 0.121568627 0.176470588)  ; 31/45 
              (set-pt a 2 0.247058824 0.345098039)  ; 63/88 
              (set-pt a 3 0.37254902 0.517647059)   ; 95/132 
              (set-pt a 4 0.498039216 0.68627451)   ; 127/175 
              (set-pt a 5 0.623529412 0.8)          ; 159/204 
              (set-pt a 6 0.749019608 0.874509804)  ; 191/223 
              (set-pt a 7 0.874509804 0.941176471)  ; 223/240 
              (set-pt a 8 1.0 0.945098039)          ; 255/241
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.02745098)           ; 0/7 
              (set-pt a 1 0.121568627 0.043137255)  ; 31/11 
              (set-pt a 2 0.247058824 0.2)          ; 63/51 
              (set-pt a 3 0.37254902 0.439215686)   ; 95/112 
              (set-pt a 4 0.498039216 0.623529412)  ; 127/159 
              (set-pt a 5 0.623529412 0.737254902)  ; 159/188 
              (set-pt a 6 0.749019608 0.843137255)  ; 191/215 
              (set-pt a 7 0.874509804 0.937254902)  ; 223/239 
              (set-pt a 8 1.0 0.945098039)          ; 255/241
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.137254902)          ; 0/35 
              (set-pt a 1 0.121568627 0.168627451)  ; 31/43 
              (set-pt a 2 0.247058824 0.352941176)  ; 63/90 
              (set-pt a 3 0.37254902 0.478431373)   ; 95/122 
              (set-pt a 4 0.498039216 0.568627451)  ; 127/145 
              (set-pt a 5 0.623529412 0.639215686)  ; 159/163 
              (set-pt a 6 0.749019608 0.725490196)  ; 191/185 
              (set-pt a 7 0.874509804 0.823529412)  ; 223/210 
              (set-pt a 8 1.0 0.823529412)          ; 255/210
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 
        ) ; end begin
    ) ; end if 


     ;===========================================================
     ; Clarendon
     ;===========================================================
     (if (= filter 8)
        (begin
        (create-layer-group "Clarendon")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.003921569)          ; 0/1  
              (set-pt a 1 0.121568627 0.066666667)  ; 31/17 
              (set-pt a 2 0.247058824 0.156862745)  ; 63/40 
              (set-pt a 3 0.37254902 0.333333333)   ; 95/85  
              (set-pt a 4 0.498039216 0.576470588)  ; 127/147  
              (set-pt a 5 0.623529412 0.729411765)  ; 159/186 
              (set-pt a 6 0.749019608 0.815686275)  ; 191/208 
              (set-pt a 7 0.874509804 0.894117647)  ; 223/228 
              (set-pt a 8 1.0 0.956862745)          ; 255/244
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.02745098)           ; 0/7 
              (set-pt a 1 0.121568627 0.098039216)  ; 31/25 
              (set-pt a 2 0.247058824 0.22745098)   ; 63/58 
              (set-pt a 3 0.37254902 0.450980392)   ; 95/115 
              (set-pt a 4 0.498039216 0.643137255)  ; 127/164 
              (set-pt a 5 0.623529412 0.77254902)   ; 159/197 
              (set-pt a 6 0.749019608 0.858823529)  ; 191/219 
              (set-pt a 7 0.874509804 0.941176471)  ; 223/240 
              (set-pt a 8 1.0 1.0)                  ; 255/255
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 20 'double)))
              (set-pt a 0 0.0 0.02745098)           ; 0/7 
              (set-pt a 1 0.121568627 0.129411765)  ; 31/33 
              (set-pt a 2 0.247058824 0.28627451)   ; 63/73 
              (set-pt a 3 0.37254902 0.478431373)   ; 95/122 
              (set-pt a 4 0.498039216 0.650980392)  ; 127/166 
              (set-pt a 5 0.623529412 0.796078431)  ; 159/203 
              (set-pt a 6 0.749019608 0.890196078)  ; 191/227 
              (set-pt a 7 0.874509804 0.97254902)   ; 223/248 
              (set-pt a 8 0.929411765 0.996078431)  ; 237/254 
              (set-pt a 9 1.0 1.0)                  ; 255/255
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 20 (spline-blue)) 

        ) ; end begin
      ) ; end if 
	  
     ;===========================================================
     ; Crema
     ;===========================================================
     (if (= filter 9)
        (begin
        (let*
           ( 
             (grad 0)                             ; new gradient map
             (actgrad 0)                          ; active gradient
             (merged 0)                           ; merged layer
             (leftcolor '(0 0 0))                 ; left gradient color
             (rightcolor '(255 255 255))          ; right gradient color
           )
       (create-layer-group "Crema")

       ;----------------------------------------------------
       ; Create black and white layer
       ;----------------------------------------------------
       (set! bwl (car (gimp-layer-copy draw TRUE )))
       (gimp-image-insert-layer img bwl lg -1)
       (gimp-layer-set-opacity bwl 30)
       (gimp-layer-set-name bwl "BlackWhite")

       ;----------------------------------------------------
       ; Create new gradient
       ;----------------------------------------------------
       ; Save active gradient
       (set! actgrad (car (gimp-context-get-gradient)))       
       ; Create new gradient
       (set! grad (car (gimp-gradient-new "GimpBox Gradient")))
       ; Set left segment color       
       (gimp-gradient-segment-set-left-color grad 0 leftcolor 100)   
       ; Set right segment color
       (gimp-gradient-segment-set-right-color grad 0 rightcolor 100)	
       (gimp-context-set-gradient grad)
       (plug-in-gradmap RUN-NONINTERACTIVE img bwl)
       (gimp-gradient-delete grad)
       ; Restore active gradient
       (gimp-context-set-gradient actgrad)
       
       ; Merge layer down
       (set! merged (car (gimp-image-merge-down img bwl EXPAND-AS-NECESSARY)))

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.066666667)           ; 0/17 
             (set-pt a 1 0.121568627 0.121568627)   ; 31/31 
             (set-pt a 2 0.247058824 0.243137255)   ; 63/62 
             (set-pt a 3 0.37254902 0.419607843)    ; 95/107 
             (set-pt a 4 0.498039216 0.607843137)   ; 127/155 
             (set-pt a 5 0.623529412 0.733333333)   ; 159/187 
             (set-pt a 6 0.749019608 0.823529412)   ; 191/210 
             (set-pt a 7 0.874509804 0.88627451)    ; 223/226 
             (set-pt a 8 1.0 0.898039216)           ; 255/229
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.062745098)           ; 0/16 
             (set-pt a 1 0.121568627 0.121568627)   ; 31/31 
             (set-pt a 2 0.247058824 0.250980392)   ; 63/64 
             (set-pt a 3 0.37254902 0.423529412)    ; 95/108 
             (set-pt a 4 0.498039216 0.580392157)   ; 127/148 
             (set-pt a 5 0.623529412 0.690196078)   ; 159/176 
             (set-pt a 6 0.749019608 0.792156863)   ; 191/202 
             (set-pt a 7 0.874509804 0.878431373)   ; 223/224 
             (set-pt a 8 1.0 0.925490196)           ; 255/236 
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.098039216)           ; 0/25 
             (set-pt a 1 0.121568627 0.168627451)   ; 31/43 
             (set-pt a 2 0.247058824 0.278431373)   ; 63/71 
             (set-pt a 3 0.37254902 0.411764706)    ; 95/105 
             (set-pt a 4 0.498039216 0.537254902)   ; 127/137 
             (set-pt a 5 0.623529412 0.639215686)   ; 159/163 
             (set-pt a 6 0.749019608 0.737254902)   ; 191/188 
             (set-pt a 7 0.874509804 0.823529412)   ; 223/210 
             (set-pt a 8 1.0 0.88627451)            ; 255/226
          a)
       )
       (gimp-drawable-curves-spline merged HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline merged HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline merged HISTOGRAM-BLUE 18 (spline-blue))        
    
     ))) ; end let begin if

     ;===========================================================
     ; Dogpatch
     ;===========================================================
     (if (= filter 10)
        (begin
        (create-layer-group "Dogpatch")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.0)                  ; 0/0 
              (set-pt a 1 0.145098039 0.015686275)  ; 37/4 
              (set-pt a 2 0.223529412 0.125490196)  ; 57/32 
              (set-pt a 3 0.321568627 0.321568627)  ; 82/82 
              (set-pt a 4 0.498039216 0.635294118)  ; 127/162 
              (set-pt a 5 0.623529412 0.784313725)  ; 159/200 
              (set-pt a 6 0.749019608 0.894117647)  ; 191/228 
              (set-pt a 7 0.88627451 0.976470588)   ; 226/249 
              (set-pt a 8 1.0 0.992156863)          ; 255/253
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.0)                  ; 0/0 
              (set-pt a 1 0.156862745 0.019607843)  ; 40/5 
              (set-pt a 2 0.22745098 0.129411765)   ; 58/33 
              (set-pt a 3 0.376470588 0.447058824)  ; 96/114 
              (set-pt a 4 0.498039216 0.654901961)  ; 127/167 
              (set-pt a 5 0.623529412 0.796078431)  ; 159/203 
              (set-pt a 6 0.749019608 0.905882353)  ; 191/231 
              (set-pt a 7 0.874509804 0.976470588)  ; 223/249 
              (set-pt a 8 1.0 0.984313725)          ; 255/251
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 22 'double)))
              (set-pt a 0 0.0 0.0)                  ; 0/0 
              (set-pt a 1 0.125490196 0.015686275)  ; 32/4 
              (set-pt a 2 0.152941176 0.031372549)  ; 39/8 
              (set-pt a 3 0.250980392 0.196078431)  ; 64/50 
              (set-pt a 4 0.337254902 0.376470588)  ; 86/96 
              (set-pt a 5 0.403921569 0.501960784)  ; 103/128 
              (set-pt a 6 0.501960784 0.643137255)  ; 128/164 
              (set-pt a 7 0.623529412 0.780392157)  ; 159/199 
              (set-pt a 8 0.749019608 0.823529412)  ; 191/210 
              (set-pt a 9 0.874509804 0.964705882)  ; 223/246 
              (set-pt a 10 1.0 0.976470588)         ; 255/249
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 22 (spline-blue)) 

        ) ; end begin
      ) ; end if 

     ;===========================================================
     ; Earlybird
     ;===========================================================
     (if (= filter 11)
        (begin
        (let*
           ( 
              (vl1 0)                        ; vignette layer 1
              (vl2 0)                        ; vignette layer 2
              (feather (* (/ lat 100) 30))   ; 30% of the longest image side
              (delta (* (/ lat 100) 2))      ; 2% of the longest image side
              (ex (+ 0 delta))               ; x coordinate of upper-left corner of ellipse bounding box
              (ey (+ 0 delta))               ; y coordinate of upper-left corner of ellipse bounding box
              (ew (- w (* delta 2)))         ; width of the ellipse
              (eh (- h (* delta 2)))         ; height of the ellipse
           )
        (create-layer-group "Earlybird")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 12 'double)))
             (set-pt a 0 0.0 0.098039216)              ; 0/25
             (set-pt a 1 0.17254902 0.31372549)        ; 44/80
             (set-pt a 2 0.333333333 0.529411765)      ; 85/135
             (set-pt a 3 0.470588235 0.725490196)      ; 120/185
             (set-pt a 4 0.901960784 0.941176471)      ; 230/240
             (set-pt a 5 1.0 1.0)                      ; 255/255
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 14 'double)))
             (set-pt a 0 0.0 0.0)                      ; 0/0
             (set-pt a 1 0.156862745 0.215686275)      ; 40/55
             (set-pt a 2 0.345098039 0.439215686)      ; 88/112
             (set-pt a 3 0.517647059 0.674509804)      ; 132/172
             (set-pt a 4 0.658823529 0.776470588)      ; 168/198
             (set-pt a 5 0.843137255 0.854901961)      ; 215/218
             (set-pt a 6 1.0 0.941176471)              ; 255/240			 
          a)
       )
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 12 'double)))
             (set-pt a 0 0.0 0.070588235)              ; 0/18
             (set-pt a 1 0.164705882 0.22745098)       ; 42/58
             (set-pt a 2 0.352941176 0.4)              ; 90/102
             (set-pt a 3 0.470588235 0.509803922)      ; 120/130
             (set-pt a 4 0.831372549 0.764705882)      ; 212/195
             (set-pt a 5 1.0 0.823529412)              ; 255/210			 
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 12 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 14 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 12 (spline-blue)) 

       ; Reduce saturation of background
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-ALL 0 0 -18 100)       
       
       ;----------------------------------------------------
       ; Add vignette 1
       ;
       ; opacity: 60%
       ; mode: 43 (burn / nachbelichten )
       ; color: HTML #b8b8b8
       ;----------------------------------------------------

       ; Create vignette layer
       (set! vl1 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette1" 60 LAYER-MODE-BURN)))
   
       ; Add vignette layer to the group
       (gimp-image-insert-layer img vl1 lg -1)

       ; Create vignette
       (gimp-image-select-ellipse img CHANNEL-OP-ADD ex ey ew eh)
       (gimp-selection-feather img feather)
       (gimp-selection-invert img)
       (gimp-context-set-foreground '(184 184 184))
       (gimp-edit-fill vl1 FILL-FOREGROUND)
       (gimp-selection-clear img)
       
       ;----------------------------------------------------
       ; Add vignette 2
       ;
       ; opacity: 100%
       ; mode: 45 (soft light / Weiche Kanten)
       ; color: black
       ;----------------------------------------------------

       ; Create vignette layer
       (set! vl2 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette2" 100 LAYER-MODE-SOFTLIGHT)))
   
       ; Add vignette layer to the group
       (gimp-image-insert-layer img vl2 lg -1)

       ; Create vignette
       (gimp-image-select-ellipse img CHANNEL-OP-ADD ex ey ew eh)
       (gimp-selection-feather img feather)
       (gimp-selection-invert img)
       (gimp-context-set-foreground '(0 0 0))
       (gimp-edit-fill vl2 FILL-FOREGROUND)
       (gimp-selection-clear img)
	   
    ))) ; end let begin if


    ;===========================================================
    ; Gingham
    ;===========================================================
    (if (= filter 12)
    (begin
       (create-layer-group "Gingham")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.17254902)            ; 0/44 
              (set-pt a 1 0.121568627 0.211764706)   ; 31/54 
              (set-pt a 2 0.247058824 0.329411765)   ; 63/84 
              (set-pt a 3 0.37254902 0.494117647)    ; 95/126 
              (set-pt a 4 0.498039216 0.635294118)   ; 127/162 
              (set-pt a 5 0.623529412 0.721568627)   ; 159/184 
              (set-pt a 6 0.749019608 0.780392157)   ; 191/199 
              (set-pt a 7 0.874509804 0.831372549)   ; 223/212 
              (set-pt a 8 1.0 0.835294 0.835294118)  ; 255/213
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.17254902)             ; 0/44 
              (set-pt a 1 0.121568627 0.196078431)    ; 31/50 
              (set-pt a 2 0.247058824 0.329411765)    ; 63/84 
              (set-pt a 3 0.37254902 0.5137255)       ; 95/131 
              (set-pt a 4 0.498039216 0.647058824)    ; 127/165 
              (set-pt a 5 0.623529412 0.725490196)    ; 159/185 
              (set-pt a 6 0.749019608 0.780392157)    ; 191/199 
              (set-pt a 7 0.874509804 0.831372549)    ; 223/212 
              (set-pt a 8 1.0 0.831372549)            ; 255/212
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.156862745)            ; 0/40 
              (set-pt a 1 0.121568627 0.211764706)    ; 31/54 
              (set-pt a 2 0.247058824 0.380392157)    ; 63/97 
              (set-pt a 3 0.37254902 0.541176471)     ; 95/138 
              (set-pt a 4 0.498039216 0.658823529)    ; 127/168 
              (set-pt a 5 0.623529412 0.737254902)    ; 159/188 
              (set-pt a 6 0.749019608 0.7843137255)   ; 191/200 
              (set-pt a 7 0.874509804 0.831372549)    ; 223/212 
              (set-pt a 8 1.0 0.835294118)            ; 255/213
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue))  

    )) ; end begin if 

    ;===========================================================
    ; Ginza
    ;===========================================================
    (if (= filter 13)
    (begin
        (create-layer-group "Ginza")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 20 'double)))
              (set-pt a 0 0.0 0.0156862745)          ; 0/4 
              (set-pt a 1 0.121568627 0.133333333)   ; 31/34 
              (set-pt a 2 0.247058824 0.278431373)   ; 63/71 
              (set-pt a 3 0.37254902 0.47843137255)  ; 95/122 
              (set-pt a 4 0.498039216 0.635294118)   ; 127/162 
              (set-pt a 5 0.623529412 0.760784314)   ; 159/194 
              (set-pt a 6 0.749019608 0.8784313725)  ; 191/224 
              (set-pt a 7 0.874509804 0.976470588)   ; 223/249 
			  (set-pt a 8 0.9098039216 1.0)          ; 232/255 
              (set-pt a 9 1.0 1.0)                   ; 255/255
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 20 'double)))
              (set-pt a 0 0.0 0.019607843)           ; 0/5 
              (set-pt a 1 0.121568627 0.149019608)   ; 31/38 
              (set-pt a 2 0.247058824 0.290196078)   ; 63/74 
              (set-pt a 3 0.37254902 0.466666667)    ; 95/119
              (set-pt a 4 0.498039216 0.619607843)   ; 127/158 
              (set-pt a 5 0.623529412 0.741176471)   ; 159/189 
              (set-pt a 6 0.749019608 0.8470588235)  ; 191/216 
              (set-pt a 7 0.874509804 0.949019608)   ; 223/242 
			  (set-pt a 8 0.945098039 1.0)           ; 241/255
              (set-pt a 9 1.0 1.0)                   ; 255/255
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.050980392)           ; 0/13 
              (set-pt a 1 0.121568627 0.274509804)   ; 31/70 
              (set-pt a 2 0.247058824 0.439215686)   ; 63/112 
              (set-pt a 3 0.37254902 0.529411765)    ; 95/135 
              (set-pt a 4 0.498039216 0.6156862745)  ; 127/157 
              (set-pt a 5 0.623529412 0.71372549)    ; 159/183 
              (set-pt a 6 0.749019608 0.8156862745)  ; 191/208 
              (set-pt a 7 0.874509804 0.894117647)   ; 223/228 
              (set-pt a 8 1.0 0.945098039)           ; 255/241
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 20 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 20 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 

    )) ; end begin if 

    ;===========================================================
    ; Gotham
    ;===========================================================
    (if (= filter 14)
       (begin
       (let*
          ( 
            (bl 0)                         ; blurred layer
            (nl 0)                         ; noise layer
            (bluel 0)                      ; blue layer
          )
       (create-layer-group "Gotham")

       ; Desaturate background
       (gimp-drawable-desaturate drawcopy DESATURATE-LIGHTNESS)
       
       ; Create blue layer
       (set! bluel (car (gimp-layer-copy drawcopy TRUE )))
       (gimp-image-insert-layer img bluel lg -1)
       (gimp-item-set-name bluel "Blue")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 10 'double)))
             (set-pt a 0 0.0 0.0)                  ; 0/0
             (set-pt a 1 0.247058824 0.384313725)  ; 63/98
             (set-pt a 2 0.501960784 0.501960784)  ; 128/128
             (set-pt a 3 0.741176471 0.623529412)  ; 189/159
             (set-pt a 4 1.0 1.0)                  ; 255/255
          a)
       )
       ; Modify curves color
       (gimp-drawable-curves-spline bluel HISTOGRAM-BLUE 10 (spline-blue))
       ; Set mode
       (gimp-layer-set-mode bluel LAYER-MODE-HARDLIGHT) ; 44
       
       ; Create blurred layer 
       (set! bl (car (gimp-layer-copy drawcopy TRUE )))
       (gimp-image-insert-layer img bl lg -1)
       (gimp-item-set-name bl "Blur")

       ;---------------------------------------------------- 
       ; Add linear motion blur
       ;----------------------------------------------------       
       (plug-in-mblur RUN-NONINTERACTIVE img bl 0 256 0 0 0)

       ;(gimp-layer-set-mode bl LAYER-MODE-SCREEN-LEGACY) ; 4 old
       (gimp-layer-set-mode bl LAYER-MODE-SCREEN) ; 31

       ;(gimp-layer-set-opacity bl 30) ; old
       (gimp-layer-set-opacity bl 15)

       ;----------------------------------------------------
       ; Add noise
       ;----------------------------------------------------
       ; opacity: 80%
       ; mode: 23 (overlay / ueberlagern)
       ; color: gray 
       ;----------------------------------------------------
       
       ; Create a new layer
       (set! nl (car (gimp-layer-new img w h RGBA-IMAGE "Noise" 80 LAYER-MODE-OVERLAY)))
   
       ; Add noise layer to the group
       (gimp-image-insert-layer img nl lg -1)

       (gimp-context-set-foreground '(128 128 128))
       (gimp-edit-fill nl FILL-FOREGROUND)

       ; Add RGB noise
       (plug-in-rgb-noise RUN-NONINTERACTIVE img nl 0 1 0.10 0.10 0.10 0)
       (plug-in-gauss RUN-NONINTERACTIVE img nl 2.0 2.0 0)
       ;(plug-in-gauss-iir RUN-NONINTERACTIVE img nl 2.5 TRUE TRUE)
       ; Remove saturation
       (gimp-drawable-hue-saturation nl HUE-RANGE-ALL 0 0 -100 100)
       

     ))) ; end let begin if

    ;===========================================================
    ; Hefe
    ;===========================================================
    (if (= filter 15)
       (begin
       (let*
          ( 
             (vl1 0)                       ; vignette layer
             (vw (* 90 (/ w 100)))         ; width of the rectangle
             (vh (* 90 (/ h 100)))         ; height of the rectangle
             (vx (/ (- w vw) 2))           ; x coordinate of upper-left corner of rectangle
             (vy (/ (- h vh) 2))           ; y coordinate of upper-left corner of rectangle
             (feather 0)                   ; feather
          )
       (if (>= w h) (set! feather (/ w 4))) ; 25% from width
       (if (> h w) (set! feather (/ h 4)))  ; 25% from height
          
       (create-layer-group "Hefe")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.003921569)          ; 0/1 
             (set-pt a 1 0.121568627 0.078431373)  ; 31/20 
             (set-pt a 2 0.247058824 0.207843137)  ; 63/53 
             (set-pt a 3 0.37254902 0.411764706)   ; 95/105 
             (set-pt a 4 0.498039216 0.580392157)  ; 127/148 
             (set-pt a 5 0.623529412 0.725490196)  ; 159/185 
             (set-pt a 6 0.749019608 0.843137255)  ; 191/215 
             (set-pt a 7 0.874509804 0.929411765)  ; 223/237 
             (set-pt a 8 1.0 0.996078431)          ; 255/254
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.003921569)          ; 0/1 
             (set-pt a 1 0.121568627 0.047058824)  ; 31/12 
             (set-pt a 2 0.247058824 0.145098039)  ; 63/37 
             (set-pt a 3 0.37254902 0.317647059)   ; 95/81 
             (set-pt a 4 0.498039216 0.509803922)  ; 127/130 
             (set-pt a 5 0.623529412 0.666666667)  ; 159/170 
             (set-pt a 6 0.749019608 0.8)          ; 191/204 
             (set-pt a 7 0.874509804 0.901960784)  ; 223/230 
             (set-pt a 8 1.0 0.984313725)          ; 255/251
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.0)                  ; 0/0 
             (set-pt a 1 0.121568627 0.02745098)   ; 31/7 
             (set-pt a 2 0.247058824 0.11372549)   ; 63/29 
             (set-pt a 3 0.37254902 0.258823529)   ; 95/66 
             (set-pt a 4 0.498039216 0.443137255)  ; 127/113 
             (set-pt a 5 0.623529412 0.6)          ; 159/153 
             (set-pt a 6 0.749019608 0.737254902)  ; 191/188 
             (set-pt a 7 0.874509804 0.850980392)  ; 223/217 
             (set-pt a 8 1.0 0.933333333)          ; 255/238
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 

       ;----------------------------------------------------
       ; Add vignette
       ;----------------------------------------------------
       ; opacity: 75%
       ; mode: 23 (overlay / ueberlagern)
       ; color: black
       ;----------------------------------------------------
       
       ; Create a new layer
       (set! vl1 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette" 75 LAYER-MODE-OVERLAY)))
   
       ; Add the vignette layer to the group
       (gimp-image-insert-layer img vl1 lg -1)

       ; Create vignette
       (gimp-image-select-rectangle img CHANNEL-OP-ADD vx vy vw vh)
       (gimp-selection-feather img feather)
       (gimp-selection-invert img)
       (gimp-context-set-foreground '(0 0 0))
       (gimp-edit-fill vl1 FILL-FOREGROUND)
       (gimp-selection-clear img) 

     ))) ; end let begin if

    ;===========================================================
    ; Helena
    ;===========================================================
    (if (= filter 16)
    (begin
        (create-layer-group "Helena")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 20 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.133333333 0.003921569)   ; 34/1 
              (set-pt a 2 0.247058824 0.105882353)   ; 63/27 
              (set-pt a 3 0.37254902 0.333333333)    ; 95/85 
              (set-pt a 4 0.498039216 0.541176471)   ; 127/138 
              (set-pt a 5 0.623529412 0.68627451)    ; 159/175 
              (set-pt a 6 0.749019608 0.803921569)   ; 191/205 
              (set-pt a 7 0.807843137 0.878431373)   ; 206/224 
              (set-pt a 8 0.874509804 0.984313725)   ; 223/251 
              (set-pt a 9 1.0 1.0)                   ; 255/255
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 20 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.121568627 0.098039216)   ; 31/25 
              (set-pt a 2 0.247058824 0.250980392)   ; 63/64 
              (set-pt a 3 0.37254902 0.388235294)    ; 95/99 
              (set-pt a 4 0.498039216 0.549019608)   ; 127/140 
              (set-pt a 5 0.623529412 0.682352941)   ; 159/174 
              (set-pt a 6 0.698039216 0.752941176)   ; 178/192 
              (set-pt a 7 0.843137255 0.878431373)   ; 215/224 
              (set-pt a 8 0.941176471 0.984313725)   ; 240/251 
              (set-pt a 9 1.0 0.992156863)           ; 255/253
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.031372549)           ; 0/8 
              (set-pt a 1 0.121568627 0.101960784)   ; 31/26 
              (set-pt a 2 0.247058824 0.203921569)   ; 63/52 
              (set-pt a 3 0.37254902 0.321568627)    ; 95/82 
              (set-pt a 4 0.498039216 0.490196078)   ; 127/125 
              (set-pt a 5 0.623529412 0.584313725)   ; 159/149 
              (set-pt a 6 0.749019608 0.678431373)   ; 191/173 
              (set-pt a 7 0.874509804 0.780392157)   ; 223/199 
              (set-pt a 8 1.0 0.905882353)           ; 255/231
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 20 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 20 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 

    )) ; end begin if 

    ;===========================================================
    ; Hudson
    ;=========================================================== 
    (if (= filter 17)
       (begin
       (let*
          ( 
            (vl1 0)                         ; vignette layer
            (feather (* (/ lat 100) 30))    ; 30% of the longest image side
            (delta (* (/ lat 100) 5))       ; 5% of the longest image side
            (ex (+ 0 delta))                ; x coordinate of upper-left corner of ellipse bounding box
            (ey (+ 0 delta))                ; y coordinate of upper-left corner of ellipse bounding box
            (ew (- w (* delta 2)))          ; width of the ellipse
            (eh (- h (* delta 2)))          ; height of the ellipse
          )
       (create-layer-group "Hudson")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.141176471)          ; 0/36 
             (set-pt a 1 0.121568627 0.219607843)  ; 31/56 
             (set-pt a 2 0.247058824 0.325490196)  ; 63/83 
             (set-pt a 3 0.37254902 0.466666667)   ; 95/119 
             (set-pt a 4 0.498039216 0.603921569)  ; 127/154 
             (set-pt a 5 0.623529412 0.717647059)  ; 159/183 
             (set-pt a 6 0.749019608 0.811764706)  ; 191/207 
             (set-pt a 7 0.874509804 0.901960784)  ; 223/230 
             (set-pt a 8 1.0 1.0)                  ; 255/255
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.003921569)          ; 0/1 
             (set-pt a 1 0.121568627 0.149019608)  ; 31/38 
             (set-pt a 2 0.247058824 0.31372549)   ; 63/80 
             (set-pt a 3 0.37254902 0.482352941)   ; 95/123 
             (set-pt a 4 0.498039216 0.643137255)  ; 127/164 
             (set-pt a 5 0.623529412 0.737254902)  ; 159/188 
             (set-pt a 6 0.749019608 0.815686275)  ; 191/208 
             (set-pt a 7 0.874509804 0.905882353)  ; 223/231
             (set-pt a 8 1.0 0.988235294)          ; 255/252
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.011764706)          ; 0/3 
             (set-pt a 1 0.121568627 0.192156863)  ; 31/49 
             (set-pt a 2 0.247058824 0.392156863)  ; 63/100 
             (set-pt a 3 0.37254902 0.584313725)   ; 95/149 
             (set-pt a 4 0.498039216 0.737254902)  ; 127/188 
             (set-pt a 5 0.623529412 0.807843137)  ; 159/206 
             (set-pt a 6 0.749019608 0.870588235)  ; 191/222 
             (set-pt a 7 0.874509804 0.921568627)  ; 223/235 
             (set-pt a 8 1.0 0.988235294)          ; 255/252
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 

       ;----------------------------------------------------
       ; Add vignette
       ;
       ; opacity: 90%
       ; mode: 23 (overlay / ueberlagern)
       ; color: black
       ;----------------------------------------------------

       ; Create vignette layer
       (set! vl1 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette" 90 LAYER-MODE-OVERLAY)))
   
       ; Add vignette layer to the group
       (gimp-image-insert-layer img vl1 lg -1)

       ; Create vignette
       (gimp-image-select-ellipse img CHANNEL-OP-ADD ex ey ew eh)
       (gimp-selection-feather img feather)
       (gimp-selection-invert img)
       (gimp-context-set-foreground '(0 0 0))
       (gimp-edit-fill vl1 FILL-FOREGROUND)
       (gimp-selection-clear img)
        
     ))) ; end let begin if

     ;===========================================================
     ; Inkwell
     ;===========================================================
     (if (= filter 18)
     (begin
       (let*
          ( 
            (dl1 0)                        ; decompose layer
          )
       (create-layer-group "Inkwell")

       ;----------------------------------------------------
       ; Decompose image into YCbCr_ITU_R470
       ;----------------------------------------------------
       (define decompose-ycbcr (plug-in-decompose RUN-NONINTERACTIVE img drawcopy "YCbCr_ITU_R470" 0))
       (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car decompose-ycbcr) )) img))
       (set! dl1 (car lyr))
       (gimp-image-remove-layer img drawcopy)
       (gimp-item-set-name dl1 "Background")
       (gimp-image-insert-layer img dl1 lg -1)

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Value
       (define (spline-value)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.0)                   ; 0/0 
             (set-pt a 1 0.121568627 0.133333333)   ; 31/34 
             (set-pt a 2 0.247058824 0.294117647)   ; 63/75 
             (set-pt a 3 0.37254902 0.482352941)    ; 95/123 
             (set-pt a 4 0.498039216 0.662745098)   ; 127/169 
             (set-pt a 5 0.623529412 0.796078431)   ; 159/203 
             (set-pt a 6 0.749019608 0.898039216)   ; 191/229 
             (set-pt a 7 0.874509804 0.964705882)   ; 223/246 
             (set-pt a 8 1.0 1.0)                   ; 255/255
          a)
       )
       (gimp-drawable-curves-spline dl1 HISTOGRAM-VALUE 18 (spline-value))

     ))) ; end let begin if

    ;===========================================================
    ; Juno
    ;=========================================================== 
    (if (= filter 19)
       (begin
       (create-layer-group "Juno")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 12 'double)))
             (set-pt a 0 0.0 0.0)                    ; 0/0
             (set-pt a 1 0.219607843 0.160784314)    ; 56/41
             (set-pt a 2 0.533333333 0.6)            ; 136/153
             (set-pt a 3 0.77254902 0.890196078)     ; 197/227
             (set-pt a 4 0.901960784 0.988235294)    ; 230/252
             (set-pt a 5 1.0 1.0)                    ; 255/255
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 12 'double)))
             (set-pt a 0 0.0 0.0)                     ; 0/0
             (set-pt a 1 0.219607843 0.168627451)     ; 56/43
             (set-pt a 2 0.529411765 0.592156863)     ; 135/151
             (set-pt a 3 0.760784314 0.862745098)     ; 194/220
             (set-pt a 4 0.901960784 0.988235294)     ; 230/252
             (set-pt a 5 1.0 1.0)                     ; 255/255
          a)
       )
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 12 'double)))
             (set-pt a 0 0.0 0.0)                     ; 0/0
             (set-pt a 1 0.223529412 0.133333333)     ; 57/34
             (set-pt a 2 0.517647059 0.556862745)     ; 132/142
             (set-pt a 3 0.77254902 0.878431373)      ; 197/224
             (set-pt a 4 0.901960784 0.980392157)     ; 230/250
             (set-pt a 5 1.0 1.0)                     ; 255/255
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 12 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 12 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 12 (spline-blue)) 
       
       ;----------------------------------------------------
       ; Modify hue, lightness and saturation
       ;----------------------------------------------------
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-RED 8 0 6 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-YELLOW 0 0 -45 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-GREEN 0 0 -27 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-BLUE -10 5 -26 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-MAGENTA 0 5 -11 100)
       
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-RED 0 0 15 100)
       
        
     )) ; end begin if

    ;===========================================================
    ; Kelvin
    ;=========================================================== 
    (if (= filter 20)
        (begin
        (create-layer-group "Kelvin")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.007843137)           ; 0/2 
              (set-pt a 1 0.121568627 0.17254902)    ; 31/44 
              (set-pt a 2 0.247058824 0.392156863)   ; 63/100 
              (set-pt a 3 0.37254902 0.635294118)    ; 95/162 
              (set-pt a 4 0.498039216 0.784313725)   ; 127/200 
              (set-pt a 5 0.623529412 0.874509804)   ; 159/223 
              (set-pt a 6 0.749019608 0.929411765)   ; 191/237 
              (set-pt a 7 0.874509804 0.964705882)   ; 223/246 
              (set-pt a 8 1.0 0.976470588)           ; 255/249
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.003921569)           ; 0/1 
              (set-pt a 1 0.121568627 0.129411765)   ; 31/33 
              (set-pt a 2 0.247058824 0.262745098)   ; 63/67 
              (set-pt a 3 0.37254902 0.423529412)    ; 95/108 
              (set-pt a 4 0.498039216 0.588235294)   ; 127/150 
              (set-pt a 5 0.623529412 0.741176471)   ; 159/189 
              (set-pt a 6 0.749019608 0.882352941)   ; 191/225 
              (set-pt a 7 0.866666667 0.996078431)   ; 221/254 
              (set-pt a 8 1.0 0.996078431)           ; 255/254
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 22 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.125490196 0.007843137)   ; 32/2 
              (set-pt a 2 0.247058824 0.007843137)   ; 63/2 
              (set-pt a 3 0.341176471 0.02745098)    ; 87/7 
              (set-pt a 4 0.403921569 0.133333333)   ; 103/34 
              (set-pt a 5 0.501960784 0.364705882)   ; 128/93 
              (set-pt a 6 0.623529412 0.662745098)   ; 159/169 
              (set-pt a 7 0.749019608 0.850980392)   ; 191/217 
              (set-pt a 8 0.874509804 0.964705882)   ; 223/246 
              (set-pt a 9 0.933333333 0.996078431)   ; 238/254 
              (set-pt a 10 1.0 1.0)                  ; 255/255
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 22 (spline-blue)) 
                
     )) ; end begin if
     
    ;===========================================================
    ; Lark
    ;=========================================================== 
    (if (= filter 21)
        (begin
        (create-layer-group "Lark")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Value
        (define (spline-value)
            (let* ((a (cons-array 18 'double)))
               (set-pt a 0 0.0 0.054901961)           ; 0/14 
               (set-pt a 1 0.121568627 0.152941176)   ; 31/39 
               (set-pt a 2 0.247058824 0.278431373)   ; 63/71 
               (set-pt a 3 0.37254902 0.458823529)    ; 95/117 
               (set-pt a 4 0.498039216 0.639215686)   ; 127/163 
               (set-pt a 5 0.623529412 0.760784314)   ; 159/194 
               (set-pt a 6 0.749019608 0.850980392)   ; 191/217 
               (set-pt a 7 0.874509804 0.901960784)   ; 223/230 
               (set-pt a 8 1.0 0.925490196)           ; 255/236
            a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-VALUE 18 (spline-value))
                
     )) ; end begin if

     

    ;===========================================================
    ; Lo-Fi
    ;=========================================================== 
    (if (= filter 22)
       (begin
       (let*
           ( 
              (vl1 0)                        ; vignette layer
              (feather (* (/ lat 100) 30))   ; 30% of the longest image side
              (delta (* (/ lat 100) 2))      ; 2% of the longest image side
              (ex (+ 0 delta))               ; x coordinate of upper-left corner of ellipse bounding box
              (ey (+ 0 delta))               ; y coordinate of upper-left corner of ellipse bounding box
              (ew (- w (* delta 2)))         ; width of the ellipse
              (eh (- h (* delta 2)))         ; height of the ellipse
           )
        (create-layer-group "Lo-Fi")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.121568627 0.054901961)   ; 31/14 
              (set-pt a 2 0.247058824 0.17254902)    ; 63/44 
              (set-pt a 3 0.37254902 0.349019608)    ; 95/89 
              (set-pt a 4 0.498039216 0.576470588)   ; 127/147 
              (set-pt a 5 0.623529412 0.745098039)   ; 159/190 
              (set-pt a 6 0.749019608 0.870588235)   ; 191/222 
              (set-pt a 7 0.874509804 0.952941176)   ; 223/243 
              (set-pt a 8 1.0 1.0)                   ; 255/255
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.121568627 0.054901961)   ; 31/14 
              (set-pt a 2 0.247058824 0.17254902)    ; 63/44 
              (set-pt a 3 0.37254902 0.349019608)    ; 95/89 
              (set-pt a 4 0.498039216 0.576470588)   ; 127/147 
              (set-pt a 5 0.623529412 0.745098039)   ; 159/190 
              (set-pt a 6 0.749019608 0.870588235)   ; 191/222 
              (set-pt a 7 0.874509804 0.952941176)   ; 223/243 
              (set-pt a 8 1.0 1.0)                   ; 255/255
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.121568627 0.054901961)   ; 31/14 
              (set-pt a 2 0.247058824 0.17254902)    ; 63/44 
              (set-pt a 3 0.37254902 0.349019608)    ; 95/89 
              (set-pt a 4 0.498039216 0.568627451)   ; 127/145 
              (set-pt a 5 0.623529412 0.733333333)   ; 159/187 
              (set-pt a 6 0.749019608 0.858823529)   ; 191/219 
              (set-pt a 7 0.874509804 0.941176471)   ; 223/240 
              (set-pt a 8 1.0 0.976470588)           ; 255/249
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 

        ;----------------------------------------------------
        ; Add vignette
        ;
        ; opacity: 100%
        ; mode: 45 (soft light / Weiche Kanten)
        ; color: black
        ;----------------------------------------------------

        ; Create vignette layer
        (set! vl1 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette" 100 LAYER-MODE-SOFTLIGHT)))
   
        ; Add vignette layer to the group
        (gimp-image-insert-layer img vl1 lg -1)

        ; Create vignette
        (gimp-image-select-ellipse img CHANNEL-OP-ADD ex ey ew eh)
        (gimp-selection-feather img feather)
        (gimp-selection-invert img)
        (gimp-context-set-foreground '(0 0 0))
        (gimp-edit-fill vl1 FILL-FOREGROUND)
        (gimp-selection-clear img)
  
     ))) ; end let begin if

    ;===========================================================
    ; Ludwig
    ;=========================================================== 
    (if (= filter 23)
       (begin
       (create-layer-group "Ludwig")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.0)                   ; 0/0 
             (set-pt a 1 0.121568627 0.082352941)   ; 31/21 
             (set-pt a 2 0.247058824 0.22745098)    ; 63/58 
             (set-pt a 3 0.37254902 0.384313725)    ; 95/98 
             (set-pt a 4 0.498039216 0.537254902)   ; 127/137 
             (set-pt a 5 0.623529412 0.71372549)    ; 159/182 
             (set-pt a 6 0.749019608 0.850980392)   ; 191/217 
             (set-pt a 7 0.874509804 0.964705882)   ; 223/246 
             (set-pt a 8 1.0 1.0)                   ; 255/255 
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.0)                   ; 0/0 
             (set-pt a 1 0.121568627 0.070588235)   ; 31/18 
             (set-pt a 2 0.247058824 0.203921569)   ; 63/52 
             (set-pt a 3 0.37254902 0.356862745)    ; 95/91 
             (set-pt a 4 0.498039216 0.517647059)   ; 127/132 
             (set-pt a 5 0.623529412 0.701960784)   ; 159/179 
             (set-pt a 6 0.749019608 0.850980392)   ; 191/217 
             (set-pt a 7 0.874509804 0.960784314)   ; 223/245 
             (set-pt a 8 1.0 1.0)                   ; 255/255
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.0)                   ; 0/0 
             (set-pt a 1 0.121568627 0.035294118)   ; 31/9 
             (set-pt a 2 0.247058824 0.176470588)   ; 63/45 
             (set-pt a 3 0.37254902 0.341176471)    ; 95/87 
             (set-pt a 4 0.498039216 0.501960784)   ; 127/128 
             (set-pt a 5 0.623529412 0.68627451)    ; 159/175 
             (set-pt a 6 0.749019608 0.847058824)   ; 191/216 
             (set-pt a 7 0.874509804 0.952941176)   ; 223/243 
             (set-pt a 8 1.0 1.0)                   ; 255/255
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 
         
       ;----------------------------------------------------
       ; Modify hue, lightness and saturation, overlap 100%
       ;----------------------------------------------------
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-RED 7 0 0 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-YELLOW -3 6 -24 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-GREEN 3 0 -70 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-CYAN 0 0 -50 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-BLUE 0 0 -48 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-MAGENTA 0 0 -75 100)       
        
     )) ; end begin if

    ;===========================================================
    ; Maven
    ;=========================================================== 
    (if (= filter 24)
        (begin
        (create-layer-group "Maven")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.058823529)           ; 0/15 
              (set-pt a 1 0.121568627 0.141176471)   ; 31/36 
              (set-pt a 2 0.247058824 0.203921569)   ; 63/52 
              (set-pt a 3 0.37254902 0.278431373)    ; 95/71 
              (set-pt a 4 0.498039216 0.498039216)   ; 127/127 
              (set-pt a 5 0.623529412 0.709803922)   ; 159/181 
              (set-pt a 6 0.749019608 0.854901961)   ; 191/218 
              (set-pt a 7 0.874509804 0.929411765)   ; 223/237 
              (set-pt a 8 1.0 0.97254902)            ; 255/248
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.054901961)           ; 0/14 
              (set-pt a 1 0.121568627 0.133333333)   ; 31/34 
              (set-pt a 2 0.247058824 0.235294118)   ; 63/60 
              (set-pt a 3 0.37254902 0.396078431)    ; 95/101 
              (set-pt a 4 0.498039216 0.51372549)    ; 127/131 
              (set-pt a 5 0.623529412 0.635294118)   ; 159/162 
              (set-pt a 6 0.749019608 0.780392157)   ; 191/199 
              (set-pt a 7 0.874509804 0.88627451)    ; 223/226 
              (set-pt a 8 1.0 0.949019608)           ; 255/242
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.047058824)           ; 0/12 
              (set-pt a 1 0.121568627 0.117647059)   ; 31/30 
              (set-pt a 2 0.247058824 0.282352941)   ; 63/72 
              (set-pt a 3 0.37254902 0.37254902)     ; 95/95 
              (set-pt a 4 0.498039216 0.423529412)   ; 127/108 
              (set-pt a 5 0.623529412 0.494117647)   ; 159/126 
              (set-pt a 6 0.749019608 0.584313725)   ; 191/149 
              (set-pt a 7 0.874509804 0.670588235)   ; 223/171 
              (set-pt a 8 1.0 0.756862745)           ; 255/193
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 
                
    )) ; end begin if

    ;===========================================================
    ; Mayfair
    ;=========================================================== 
    (if (= filter 25)
       (begin
       (let*
          ( 
            (ml 0)                              ; mask layer
            (bg2 0)                             ; background layer
            (feather (* 0.3 lat))               ; 30% of the longest image side            
            (sel 0)                             ; item of selection
            (centerx (/ w 2))                   ; x coordinate of image center
            (centery (/ h 2))                   ; y coordinate of image center
            ; Middle ellipse
            (ew (* 0.65 w))                     ; ellipse width: 65% of image width
            (eh (* 0.35 h))                     ; ellipse height: 35% of image height
            (dx (/ ew 2))                       ; delta x
            (dy (/ eh 2))                       ; delta y
            (offset (* 0.08 w))                 ; x offset from the center
            (x1 (- centerx (+ dx offset)))      ; x coordinate of upper-left corner of ellipse bounding box
            (y1 (- centery dy))                 ; y coordinate of upper-left corner of ellipse bounding box
            ; Top and bottom Ellipses
            (tew (* 0.5 w))                     ; ellipse width: 50% of image width
            (teh (* 0.5 h))                     ; ellipse height: 50% of image height
            (dtex (/ tew 2))                    ; ellipse delta x
            (dtey (/ teh 2))                    ; ellipse delta y
            
            (x2 (- centerx (- dtex offset)))    ; x coordinate of upper-left corner of top ellipse bounding box
            (y2 (- 0 dtey))                     ; y coordinate of upper-left corner of top ellipse bounding box
            (x3 (- centerx (- dtex offset)))    ; x coordinate of upper-left corner of bottom ellipse bounding box
            (y3 (+ centery dtey))               ; y coordinate of upper-left corner of bottom ellipse bounding box
          )
       (create-layer-group "Mayfair")

       ;----------------------------------------------------
       ; Copy background layer
       ;----------------------------------------------------
       (set! bg2 (car (gimp-layer-copy drawcopy TRUE )))
       (gimp-image-insert-layer img bg2 lg -1)
       (gimp-item-set-name bg2 "Mask")       

       ;----------------------------------------------------
       ; Ajust curves color of background layer
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.058823529)          ; 0/15 
             (set-pt a 1 0.121568627 0.101960784)  ; 31/26 
             (set-pt a 2 0.247058824 0.152941176)  ; 63/39 
             (set-pt a 3 0.37254902 0.22745098)    ; 95/58 
             (set-pt a 4 0.498039216 0.309803922)  ; 127/79 
             (set-pt a 5 0.623529412 0.501960784)  ; 159/128 
             (set-pt a 6 0.749019608 0.694117647)  ; 191/177 
             (set-pt a 7 0.874509804 0.831372549)  ; 223/212 
             (set-pt a 8 1.0 0.933333333)          ; 255/238 
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.031372549)          ; 0/8 
             (set-pt a 1 0.121568627 0.082352941)  ; 31/21 
             (set-pt a 2 0.247058824 0.141176471)  ; 63/36 
             (set-pt a 3 0.37254902 0.207843137)   ; 95/53 
             (set-pt a 4 0.498039216 0.298039216)  ; 127/76 
             (set-pt a 5 0.623529412 0.490196078)  ; 159/125 
             (set-pt a 6 0.749019608 0.647058824)  ; 191/165 
             (set-pt a 7 0.874509804 0.764705882)  ; 223/195 
             (set-pt a 8 1.0 0.839215686)          ; 255/214
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.019607843)          ; 0/5 
             (set-pt a 1 0.121568627 0.070588235)  ; 31/18 
             (set-pt a 2 0.247058824 0.129411765)  ; 63/33 
             (set-pt a 3 0.37254902 0.196078431)   ; 95/50 
             (set-pt a 4 0.498039216 0.278431373)  ; 127/71 
             (set-pt a 5 0.623529412 0.458823529)  ; 159/117 
             (set-pt a 6 0.749019608 0.62745098)   ; 191/160 
             (set-pt a 7 0.874509804 0.752941176)  ; 223/192 
             (set-pt a 8 1.0 0.854901961)          ; 255/218
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue))
        
       ;----------------------------------------------------
       ; Ajust curves color of mask layer
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.129411765)          ; 0/33 
             (set-pt a 1 0.121568627 0.235294118)  ; 31/60 
             (set-pt a 2 0.247058824 0.356862745)  ; 63/91 
             (set-pt a 3 0.37254902 0.529411765)   ; 95/135 
             (set-pt a 4 0.498039216 0.71372549)   ; 127/182 
             (set-pt a 5 0.623529412 0.803921569)  ; 159/205 
             (set-pt a 6 0.749019608 0.874509804)  ; 191/223 
             (set-pt a 7 0.874509804 0.937254902)  ; 223/239 
             (set-pt a 8 1.0 0.980392157)          ; 255/250
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.08627451)           ; 0/22 
             (set-pt a 1 0.121568627 0.196078431)  ; 31/50 
             (set-pt a 2 0.247058824 0.325490196)  ; 63/83 
             (set-pt a 3 0.37254902 0.474509804)   ; 95/121 
             (set-pt a 4 0.498039216 0.666666667)  ; 127/170 
             (set-pt a 5 0.623529412 0.764705882)  ; 159/195 
             (set-pt a 6 0.749019608 0.847058824)  ; 191/216 
             (set-pt a 7 0.874509804 0.878431373)  ; 223/224 
             (set-pt a 8 1.0 0.905882353)          ; 255/231
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.066666667)          ; 0/17 
             (set-pt a 1 0.121568627 0.180392157)  ; 31/46 
             (set-pt a 2 0.247058824 0.309803922)  ; 63/79 
             (set-pt a 3 0.37254902 0.454901961)   ; 95/116 
             (set-pt a 4 0.498039216 0.611764706)  ; 127/156 
             (set-pt a 5 0.623529412 0.729411765)  ; 159/186 
             (set-pt a 6 0.749019608 0.803921569)  ; 191/205 
             (set-pt a 7 0.874509804 0.854901961)  ; 223/218 
             (set-pt a 8 1.0 0.878431373)          ; 255/224
          a)
       )
       (gimp-drawable-curves-spline bg2 HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline bg2 HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline bg2 HISTOGRAM-BLUE 18 (spline-blue))              

       ;----------------------------------------------------
       ; Add mask to mask layer
       ;----------------------------------------------------
       (set! ml (car (gimp-layer-create-mask bg2 ADD-MASK-BLACK )))
       (gimp-layer-add-mask bg2 ml)

       ;----------------------------------------------------
       ; Create vignette
       ;----------------------------------------------------
       ; Create an elliptical selection in the image center
       (gimp-image-select-ellipse img CHANNEL-OP-ADD x1 y1 ew eh)      
       (set! sel (car (gimp-image-get-selection img)))
       ; Rotate selection
       (gimp-item-transform-rotate sel 0.52 FALSE centerx centery) ; angle of rotation: 0.52 = 30°
       
	   ; Create top and bottom ellipses
       (gimp-image-select-ellipse img CHANNEL-OP-ADD x2 y2 tew teh)
       (gimp-image-select-ellipse img CHANNEL-OP-ADD x3 y3 tew teh)  
       ; Feathering the selection   
       (gimp-selection-feather img feather)
       ; Set foreground color 
       (gimp-context-set-foreground '(255 255 255))
       ; Fill selection with foreground color
       (gimp-edit-fill ml FILL-FOREGROUND)
       (gimp-selection-clear img)	         
        
    ))) ; end let begin if

    ;===========================================================
    ; Moon
    ;=========================================================== 
    (if (= filter 26)
       (begin
       (create-layer-group "Moon")
       (let*
          ( 
            (dl1 0)                        ; decomposed layer YCbCr_ITU_R470
            (dl2 0)                        ; decomposed layer Red
            (merged 0)                     ; merged layer
            (rl 0)                         ; red layer
          )
       ;----------------------------------------------------
       ; Decompose image into YCbCr_ITU_R470
       ;----------------------------------------------------
       (define decompose-ycbcr (plug-in-decompose RUN-NONINTERACTIVE img drawcopy "YCbCr_ITU_R470" 0))
       (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car decompose-ycbcr) )) img))
       (set! dl1 (car lyr))
       (gimp-item-set-name dl1 "Background2")
       (gimp-image-insert-layer img dl1 lg -1)

       ;----------------------------------------------------
       ; Decompose image into red chanel
       ;----------------------------------------------------
       (define decompose-red (plug-in-decompose RUN-NONINTERACTIVE img drawcopy "Red" 0))
       (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car decompose-red) )) img))
       (set! rl (car lyr))
       (gimp-item-set-name rl "Red")
       (gimp-image-insert-layer img rl lg -1)
       (gimp-layer-set-mode rl LAYER-MODE-SOFTLIGHT)

       ;----------------------------------------------------
       ; Create desaturated red layer
       ;----------------------------------------------------
       ;(set! rl (car (gimp-layer-copy drawcopy FALSE)))
       ;(gimp-item-set-name rl "Red")
       ;(gimp-layer-set-mode rl LAYER-MODE-SOFTLIGHT)
       ;(gimp-image-insert-layer img rl lg -1)
       ;(plug-in-colors-channel-mixer 1 img rl TRUE
       ;                           1 0 0 ;R
       ;                           0 0 0 ;G
       ;                           0 0 0 ;B
       ;)

       (gimp-image-remove-layer img drawcopy)
       
       ; Merge layer down
       (set! merged (car (gimp-image-merge-down img rl EXPAND-AS-NECESSARY)))
       (gimp-item-set-name merged "Background")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Value
       (define (spline-value)
           (let* ((a (cons-array 22 'double)))
              (set-pt a 0 0.0 0.2)                   ; 0/51 
              (set-pt a 1 0.101960784 0.247058824)   ; 26/63 
              (set-pt a 2 0.211764706 0.360784314)   ; 54/92 
              (set-pt a 3 0.305882353 0.51372549)    ; 78/131 
              (set-pt a 4 0.4 0.631372549)           ; 102/161 
              (set-pt a 5 0.498039216 0.705882353)   ; 127/180 
              (set-pt a 6 0.623529412 0.776470588)   ; 159/198 
              (set-pt a 7 0.749019608 0.82745098)    ; 191/211 
              (set-pt a 8 0.874509804 0.882352941)   ; 223/225
              (set-pt a 9 0.976470588 0.941176471)   ; 249/240
              (set-pt a 10 1.0 0.980392157)          ; 255/250
           a)
       )
       (gimp-drawable-curves-spline merged HISTOGRAM-VALUE 22 (spline-value))
 
    ))) ; end let begin if

    ;===========================================================
    ; Nashville
    ;=========================================================== 
    (if (= filter 27)
       (begin
       (create-layer-group "Nashville")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 28 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0
              (set-pt a 1 0.125490196 0.054901961)   ; 32/14 
              (set-pt a 2 0.196078431 0.082352941)   ; 50/21 
              (set-pt a 3 0.254901961 0.11372549)    ; 65/29 
              (set-pt a 4 0.270588235 0.129411765)   ; 69/33 
              (set-pt a 5 0.305882353 0.254901961)   ; 78/65 
              (set-pt a 6 0.349019608 0.380392157)   ; 89/97 
              (set-pt a 7 0.419607843 0.501960784)   ; 107/128 
              (set-pt a 8 0.51372549 0.62745098)     ; 131/160 
              (set-pt a 9 0.62745098 0.756862745)    ; 160/193 
              (set-pt a 10 0.745098039 0.878431373)  ; 190/224 
              (set-pt a 11 0.776470588 0.909803922)  ; 198/232 
              (set-pt a 12 0.874509804 0.949019608)  ; 223/242 
              (set-pt a 13 1.0 1.0)                  ; 255/255
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 26 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.078431373 0.035294118)   ; 20/9 
              (set-pt a 2 0.101960784 0.058823529)   ; 26/15 
              (set-pt a 3 0.156862745 0.184313725)   ; 40/47 
              (set-pt a 4 0.250980392 0.321568627)   ; 64/82 
              (set-pt a 5 0.360784314 0.439215686)   ; 92/112 
              (set-pt a 6 0.466666667 0.545098039)   ; 119/139 
              (set-pt a 7 0.556862745 0.62745098)    ; 142/160 
              (set-pt a 8 0.678431373 0.737254902)   ; 173/188 
              (set-pt a 9 0.768627451 0.811764706)   ; 196/207 
              (set-pt a 10 0.807843137 0.831372549)  ; 206/212 
              (set-pt a 11 0.921568627 0.874509804)  ; 235/223 
              (set-pt a 12 1.0 0.905882353)          ; 255/231 
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 20 'double)))
              (set-pt a 0 0.0 0.258823529)          ; 0/66 
              (set-pt a 1 0.08627451 0.298039216)   ; 22/76 
              (set-pt a 2 0.125490196 0.325490196)  ; 32/83 
              (set-pt a 3 0.203921569 0.380392157)  ; 52/97 
              (set-pt a 4 0.376470588 0.490196078)  ; 96/125 
              (set-pt a 5 0.501960784 0.564705882)  ; 128/144 
              (set-pt a 6 0.623529412 0.635294118)  ; 159/162 
              (set-pt a 7 0.749019608 0.705882353)  ; 191/180 
              (set-pt a 8 0.874509804 0.760784314)  ; 223/194 
              (set-pt a 9 1.0 0.807843137)          ; 255/206
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 28 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 26 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 20 (spline-blue)) 
        
    )) ; end begin if

    ;===========================================================
    ; Perpetua
    ;=========================================================== 
    (if (= filter 28)
       (begin
       (let*
          ( 
            (cl1 0)                        ; color layer 1
            (x1 (/ w 2))                   ; x coordinate of blend's starting point
            (y1 0)                         ; y coordinate of blend's starting point
            (x2 (/ w 2))                   ; x coordinate of blend's ending point
            (y2 h)                         ; y coordinate of blend's ending point
          )
       (create-layer-group "Perpetua")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.070588235)          ; 0/18 
             (set-pt a 1 0.121568627 0.11372549)   ; 31/29 
             (set-pt a 2 0.247058824 0.211764706)  ; 63/54 
             (set-pt a 3 0.37254902 0.341176471)   ; 95/87 
             (set-pt a 4 0.498039216 0.494117647)  ; 127/126 
             (set-pt a 5 0.623529412 0.670588235)  ; 159/171 
             (set-pt a 6 0.749019608 0.8)          ; 191/204 
             (set-pt a 7 0.874509804 0.909803922)  ; 223/232 
             (set-pt a 8 1.0 0.996078431)          ; 255/254
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.078431373)          ; 0/20 
             (set-pt a 1 0.121568627 0.133333333)  ; 31/34 
             (set-pt a 2 0.247058824 0.254901961)  ; 63/65 
             (set-pt a 3 0.37254902 0.396078431)   ; 95/101 
             (set-pt a 4 0.498039216 0.533333333)  ; 127/136 
             (set-pt a 5 0.623529412 0.701960784)  ; 159/179 
             (set-pt a 6 0.749019608 0.811764706)  ; 191/207 
             (set-pt a 7 0.874509804 0.917647059)  ; 223/234 
             (set-pt a 8 1.0 0.996078431)          ; 255/254
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.031372549)          ; 0/8 
             (set-pt a 1 0.121568627 0.094117647)  ; 31/24 
             (set-pt a 2 0.247058824 0.211764706)  ; 63/54 
             (set-pt a 3 0.37254902 0.341176471)   ; 95/87 
             (set-pt a 4 0.498039216 0.447058824)  ; 127/114 
             (set-pt a 5 0.623529412 0.654901961)  ; 159/167 
             (set-pt a 6 0.749019608 0.77254902)   ; 191/197 
             (set-pt a 7 0.874509804 0.870588235)  ; 223/222 
             (set-pt a 8 1.0 0.964705882)          ; 255/246
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 

       ;----------------------------------------------------
       ; Modify hue, lightness and saturation, overlap 100%
       ;----------------------------------------------------
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-RED 9 0 -30 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-YELLOW -9 0 -40 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-GREEN 1 0 -25 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-CYAN 2 0 -40 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-BLUE 2 0 -20 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-MAGENTA -1 0 -30 100)       

       ;----------------------------------------------------
       ; Create gradient layer
       ;----------------------------------------------------
       ; Type: RGBA (transparent)
       ; Mode: 45 (soft light / weiche Kanten)
       ; Opacity: 50%
	   ;----------------------------------------------------
       ; Set foreground color for linear gradient
       (gimp-context-set-foreground '(0 115 195)) ; HTML #0073c3
       ; Set background color for linear gradient
       (gimp-context-set-background '(208 118 0)) ; HTML #d07600

       (set! cl1 (car (gimp-layer-new img w h RGBA-IMAGE "Gradient" 50 LAYER-MODE-SOFTLIGHT)))
       ; Add the new layer to the group
       (gimp-image-insert-layer img cl1 lg -1)
       ; Add radial gradient
       (gimp-edit-blend cl1 BLEND-FG-BG-RGB LAYER-MODE-NORMAL GRADIENT-LINEAR 100 0 REPEAT-NONE FALSE FALSE 4 2 TRUE x1 y1 x2 y2)

        
    ))) ; end begin if

    ;===========================================================
    ; Poprocket
    ;=========================================================== 
    (if (= filter 29)
       (begin
       (let*
          ( 
            (cl1 0)                        ; color layer 1
            (cl2 0)                        ; color layer 2
            (delta1 (* 5 (/ w 100)))       ; 5% of the image width
            (delta2 (* 30 (/ w 100)))      ; 30% of the image width
          )
       (create-layer-group "Poprocket")

       ;----------------------------------------------------
       ; Create color layer 1
       ;----------------------------------------------------
       ; Type: RGBA (transparent)
       ; Mode: 31 (screen / Bildschirm)
       ; Opacity: 75%
       ;----------------------------------------------------
       ; Set foreground for radial gradient
       (gimp-context-set-foreground '(206 39 70)) ; HTML #ce2746
       (set! cl1 (car (gimp-layer-new img w h RGBA-IMAGE "Color1" 75 LAYER-MODE-SCREEN)))
       ; Add the new layer to the group
       (gimp-image-insert-layer img cl1 lg -1)
       ; Add radial gradient
       (gimp-edit-blend cl1 BLEND-FG-TRANSPARENT LAYER-MODE-NORMAL GRADIENT-RADIAL 100 0 REPEAT-NONE FALSE FALSE 4 2 TRUE (/ w 2) (/ h 2) (- w delta1) (/ h 2))

       ;----------------------------------------------------
       ; Create color layer 2
       ;----------------------------------------------------
       ; Type: RGBA (transparent)
       ; Mode: 23 (overlay / Ueberlagern)
       ; Opacity: 100% 
       ;----------------------------------------------------   
       ; Set foreground for gradient
       (gimp-context-set-foreground '(15 5 46)) ; HTML #0f052e
       (set! cl2 (car (gimp-layer-new img w h RGBA-IMAGE "Color2" 100 LAYER-MODE-OVERLAY)))
       ; Add color layer to the group
       (gimp-image-insert-layer img cl2 lg -1)
       ; Add invert radial gradient
       (gimp-edit-blend cl2 BLEND-FG-TRANSPARENT LAYER-MODE-NORMAL GRADIENT-RADIAL 100 0 REPEAT-NONE TRUE FALSE 4 2 TRUE (/ w 2) (/ h 2) (+ w delta2) (/ h 2))

    ))) ; end let begin if

     ;===========================================================
     ; Reyes
     ;===========================================================
     (if (= filter 30)
     (begin
     (let*
        ( 
          (drawcopy2 0)                       ; tmp layer
          (merged 0)                          ; merged layer
        )
        (create-layer-group "Reyes")

        ; Desaturate bacjground layer
        ;(gimp-drawable-desaturate drawcopy DESATURATE-LIGHTNESS)
        (gimp-drawable-desaturate drawcopy DESATURATE-LUMINANCE)

        ;----------------------------------------------------
        ; Copy source layer to the group 
        ;----------------------------------------------------
        (set! drawcopy2 (car (gimp-layer-copy draw TRUE )))
        (gimp-image-insert-layer img drawcopy2 lg 0)
        (gimp-item-set-name drawcopy2 "Tmp Layer")
        (gimp-layer-set-opacity drawcopy2 70)
        
        ; Merge layer down
        (set! merged (car (gimp-image-merge-down img drawcopy2 EXPAND-AS-NECESSARY)))

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.192156863)          ; 0/49 
              (set-pt a 1 0.121568627 0.235294118)  ; 31/60 
              (set-pt a 2 0.247058824 0.415686275)  ; 63/106
              (set-pt a 3 0.37254902 0.592156863)   ; 95/151 
              (set-pt a 4 0.498039216 0.705882353)  ; 127/180 
              (set-pt a 5 0.623529412 0.788235294)  ; 159/201 
              (set-pt a 6 0.749019608 0.839215686)  ; 191/214 
              (set-pt a 7 0.874509804 0.858823529)  ; 223/219 
              (set-pt a 8 1.0 0.866666667)          ; 255/221
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.17254902)           ; 0/44 
              (set-pt a 1 0.121568627 0.215686275)  ; 31/55 
              (set-pt a 2 0.247058824 0.388235294)  ; 63/99 
              (set-pt a 3 0.37254902 0.568627451)   ; 95/145
              (set-pt a 4 0.498039216 0.678431373)  ; 127/173 
              (set-pt a 5 0.623529412 0.756862745)  ; 159/193
              (set-pt a 6 0.749019608 0.807843137)  ; 191/206 
              (set-pt a 7 0.874509804 0.839215686)  ; 223/214
              (set-pt a 8 1.0 0.854901961)          ; 255/218
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.156862745)          ; 0/40 
              (set-pt a 1 0.121568627 0.192156863)  ; 31/49 
              (set-pt a 2 0.247058824 0.349019608)  ; 63/89
              (set-pt a 3 0.37254902 0.51372549)    ; 95/131
              (set-pt a 4 0.498039216 0.639215686)  ; 127/163 
              (set-pt a 5 0.623529412 0.721568627)  ; 159/184 
              (set-pt a 6 0.749019608 0.77254902)   ; 191/197
              (set-pt a 7 0.874509804 0.811764706)  ; 223/207
              (set-pt a 8 1.0 0.835294118)          ; 255/213
           a)
        )
        (gimp-drawable-curves-spline merged HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline merged HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline merged HISTOGRAM-BLUE 18 (spline-blue)) 

     ))) ; end let begin if

    ;===========================================================
    ; Rise
    ;=========================================================== 
    (if (= filter 31)
        (begin
        (let*
           ( 
             (cl1 0)                        ; color layer 1
             (delta1 (* 30 (/ w 100)))      ; 30% of the image width
           )
        (create-layer-group "Rise")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.090196078)          ; 0/23 
              (set-pt a 1 0.121568627 0.262745098)  ; 31/67 
              (set-pt a 2 0.247058824 0.48627451)   ; 63/124 
              (set-pt a 3 0.37254902 0.635294118)   ; 95/162 
              (set-pt a 4 0.498039216 0.717647059)  ; 127/183 
              (set-pt a 5 0.623529412 0.768627451)  ; 159/196 
              (set-pt a 6 0.749019608 0.823529412)  ; 191/210 
              (set-pt a 7 0.874509804 0.890196078)  ; 223/227 
              (set-pt a 8 1.0 0.992156863)          ; 255/253
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.074509804)          ; 0/19 
              (set-pt a 1 0.121568627 0.219607843)  ; 31/56 
              (set-pt a 2 0.247058824 0.403921569)  ; 63/103 
              (set-pt a 3 0.37254902 0.592156863)   ; 95/151 
              (set-pt a 4 0.498039216 0.698039216)  ; 127/178 
              (set-pt a 5 0.623529412 0.768627451)  ; 159/196 
              (set-pt a 6 0.749019608 0.839215686)  ; 191/214 
              (set-pt a 7 0.874509804 0.91372549)   ; 223/233 
              (set-pt a 8 1.0 1.0)                  ; 255/255
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.117647059)          ; 0/30 
              (set-pt a 1 0.121568627 0.243137255)  ; 31/62 
              (set-pt a 2 0.247058824 0.376470588)  ; 63/96 
              (set-pt a 3 0.37254902 0.525490196)   ; 95/134 
              (set-pt a 4 0.498039216 0.631372549)  ; 127/161 
              (set-pt a 5 0.623529412 0.71372549)   ; 159/182 
              (set-pt a 6 0.749019608 0.776470588)  ; 191/198 
              (set-pt a 7 0.874509804 0.854901961)  ; 223/218 
              (set-pt a 8 1.0 0.968627451)          ; 255/247
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 

        ;----------------------------------------------------	   
        ; Create color layer
        ; Type: RGBA (transparent)
        ; Opacity: 100%
        ; Mode: 23 (Overlay / Ueberlagern)
        ;----------------------------------------------------
        ; Set foreground color for gradient
        (gimp-context-set-foreground '(0 9 25)) ; HTML: #000919
        (set! cl1 (car (gimp-layer-new img w h RGBA-IMAGE "Color" 100 LAYER-MODE-OVERLAY)))
        ; Add color layer to the group
        (gimp-image-insert-layer img cl1 lg -1)
        ; Add invert radial gradient
        (gimp-edit-blend cl1 BLEND-FG-TRANSPARENT LAYER-MODE-NORMAL GRADIENT-RADIAL 100 0 REPEAT-NONE TRUE FALSE 4 2 TRUE (/ w 2) (/ h 2) (+ w delta1) (/ h 2))

    ))) ; end let begin if

    ;===========================================================
    ; Sierra
    ;=========================================================== 
    (if (= filter 32)
        (begin
        (let*
           ( 
             (vl1 0)                        ; vignette layer
             (feather (* (/ lat 100) 30))   ; 30% of the longest image side
             (delta (* (/ lat 100) 5))      ; 5% of the longest image side
             (ex (+ 0 delta))               ; x coordinate of upper-left corner of ellipse bounding box
             (ey (+ 0 delta))               ; y coordinate of upper-left corner of ellipse bounding box
             (ew (- w (* delta 2)))         ; width of the ellipse
             (eh (- h (* delta 2)))         ; height of the ellipse  
           )
        (create-layer-group "Sierra")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
        ; Red channel
        (define (spline-red)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.043137255)          ; 0/11 
              (set-pt a 1 0.121568627 0.28627451)   ; 31/73 
              (set-pt a 2 0.247058824 0.431372549)  ; 63/110 
              (set-pt a 3 0.37254902 0.552941176)   ; 95/141 
              (set-pt a 4 0.498039216 0.678431373)  ; 127/173 
              (set-pt a 5 0.623529412 0.760784314)  ; 159/194 
              (set-pt a 6 0.749019608 0.831372549)  ; 191/212 
              (set-pt a 7 0.874509804 0.905882353)  ; 223/231 
              (set-pt a 8 1.0 0.980392157)          ; 255/250
           a)
        )
        ; Green channel
        (define (spline-green)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.043137255)          ; 0/11 
              (set-pt a 1 0.121568627 0.270588235)  ; 31/69 
              (set-pt a 2 0.247058824 0.403921569)  ; 63/103 
              (set-pt a 3 0.37254902 0.517647059)   ; 95/132 
              (set-pt a 4 0.498039216 0.623529412)  ; 127/159 
              (set-pt a 5 0.623529412 0.71372549)   ; 159/182 
              (set-pt a 6 0.749019608 0.784313725)  ; 191/200 
              (set-pt a 7 0.874509804 0.847058824)  ; 223/216 
              (set-pt a 8 1.0 0.901960784)          ; 255/230
           a)
        )        
        ; Blue channel
        (define (spline-blue)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.109803922)          ; 0/28 
              (set-pt a 1 0.121568627 0.290196078)  ; 31/74 
              (set-pt a 2 0.247058824 0.407843137)  ; 63/104 
              (set-pt a 3 0.37254902 0.509803922)   ; 95/130 
              (set-pt a 4 0.498039216 0.607843137)  ; 127/155 
              (set-pt a 5 0.623529412 0.682352941)  ; 159/174 
              (set-pt a 6 0.749019608 0.752941176)  ; 191/192 
              (set-pt a 7 0.874509804 0.815686275)  ; 223/208 
              (set-pt a 8 1.0 0.870588235)          ; 255/222
           a)
        )
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
        (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 

       ;----------------------------------------------------
       ; Add vignette
       ;
       ; opacity: 100%
       ; mode: 45 (soft light / Weiche Kanten)
       ; Color: 111438 (RGB 17 20 56)
       ;----------------------------------------------------

       ; Create vignette layer
       (set! vl1 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette" 100 LAYER-MODE-SOFTLIGHT)))
   
       ; Add vignette layer to the group
       (gimp-image-insert-layer img vl1 lg -1)

       ; Create vignette
       (gimp-image-select-ellipse img CHANNEL-OP-ADD ex ey ew eh)
       (gimp-selection-feather img feather)
       (gimp-selection-invert img)
       (gimp-context-set-foreground '(17 20 56))
       (gimp-edit-fill vl1 FILL-FOREGROUND)
       (gimp-selection-clear img)

    ))) ; end let begin if

    ;===========================================================
    ; Skyline
    ;=========================================================== 
    (if (= filter 33)
       (begin
       (create-layer-group "Skyline")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 26 'double)))
             (set-pt a 0 0.0 0.082352941)           ; 0/21 
             (set-pt a 1 0.125490196 0.184313725)   ; 32/47 
             (set-pt a 2 0.188235294 0.215686275)   ; 48/55 
             (set-pt a 3 0.298039216 0.305882353)   ; 76/78 
             (set-pt a 4 0.376470588 0.439215686)   ; 96/112 
             (set-pt a 5 0.407843137 0.498039216)   ; 104/127 
             (set-pt a 6 0.501960784 0.62745098)    ; 128/160 
             (set-pt a 7 0.623529412 0.741176471)   ; 159/189 
             (set-pt a 8 0.749019608 0.847058824)   ; 191/216 
             (set-pt a 9 0.823529412 0.921568627)   ; 210/235 
             (set-pt a 10 0.874509804 0.980392157)  ; 223/250 
             (set-pt a 11 0.898039216 0.992156863)  ; 229/253 
             (set-pt a 12 1.0 1.0)                  ; 255/255
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 22 'double)))
             (set-pt a 0 0.0 0.043137255)           ; 0/11 
             (set-pt a 1 0.125490196 0.133333333)   ; 32/34 
             (set-pt a 2 0.250980392 0.298039216)   ; 64/76 
             (set-pt a 3 0.37254902 0.482352941)    ; 95/123 
             (set-pt a 4 0.498039216 0.631372549)   ; 127/161 
             (set-pt a 5 0.623529412 0.741176471)   ; 159/189 
             (set-pt a 6 0.749019608 0.847058824)   ; 191/216 
             (set-pt a 7 0.823529412 0.921568627)   ; 210/235 
             (set-pt a 8 0.874509804 0.980392157)   ; 223/250 
             (set-pt a 9 0.898039216 0.992156863)   ; 229/253 
             (set-pt a 10 1.0 1.0)                  ; 255/255
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.007843137)           ; 0/2 
             (set-pt a 1 0.121568627 0.125490196)   ; 31/32 
             (set-pt a 2 0.247058824 0.305882353)   ; 63/78 
             (set-pt a 3 0.37254902 0.466666667)    ; 95/119 
             (set-pt a 4 0.498039216 0.592156863)   ; 127/151 
             (set-pt a 5 0.623529412 0.690196078)   ; 159/176 
             (set-pt a 6 0.749019608 0.784313725)   ; 191/200 
             (set-pt a 7 0.874509804 0.956862745)   ; 223/244 
             (set-pt a 8 1.0 1.0)                   ; 255/255
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 26 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 22 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 

       ;----------------------------------------------------
       ; Modify hue, lightness and saturation, overlap 100%
       ;----------------------------------------------------
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-BLUE 0 5 15 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-MAGENTA 0 0 20 100)  
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-RED 0 0 20 100)
       (gimp-drawable-hue-saturation drawcopy HUE-RANGE-YELLOW 0 0 20 100)
               
    )) ; end begin if

    ;===========================================================
    ; Slumber
    ;=========================================================== 
    (if (= filter 34)
       (begin
       (create-layer-group "Slumber")
       (let*
          ( 
            (redl 0)                       ; decomposed red channel layer
            (bluel 0)                      ; decomposed blue channel layer 
            (merged 0)                     ; merged layer
          )

       ;----------------------------------------------------
       ; Decompose image into red chanel
       ;----------------------------------------------------
       (define decompose-red (plug-in-decompose RUN-NONINTERACTIVE img drawcopy "Red" 0))
       (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car decompose-red) )) img))
       (set! redl (car lyr))
       (gimp-item-set-name redl "Red")
       (gimp-image-insert-layer img redl lg -1)
       (gimp-layer-set-opacity redl 50)
       (gimp-layer-set-mode redl LAYER-MODE-DARKEN-ONLY) ; Nur abdunkeln (35)

       ;----------------------------------------------------
       ; Decompose image into blue chanel
       ;----------------------------------------------------
       (define decompose-blue (plug-in-decompose RUN-NONINTERACTIVE img drawcopy "Blue" 0))
       (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car decompose-blue) )) img))
       (set! bluel (car lyr))
       (gimp-item-set-name bluel "Blue")
       (gimp-image-insert-layer img bluel lg -1)
       (gimp-layer-set-opacity bluel 50)
       (gimp-layer-set-mode bluel LAYER-MODE-DARKEN-ONLY) ; Nur abdunkeln (35)

       ; Merge red layer down
       (set! merged (car (gimp-image-merge-down img redl EXPAND-AS-NECESSARY)))
       (gimp-item-set-name merged "Background")

       ; Merge blue layer down
       (set! merged (car (gimp-image-merge-down img bluel EXPAND-AS-NECESSARY)))
       (gimp-item-set-name merged "Background")
       
       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Value
       (define (spline-value)
           (let* ((a (cons-array 10 'double)))
              (set-pt a 0 0.0 0.0)                   ; 0/0 
              (set-pt a 1 0.121568627 0.121568627)   ; 31/31 
              (set-pt a 2 0.37254902 0.447058824)    ; 95/114 
              (set-pt a 3 0.623529412 0.68627451)    ; 159/175 
              (set-pt a 4 1.0 1.0)                   ; 255/255
           a)
       )
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.160784314)           ; 0/41 
             (set-pt a 1 0.121568627 0.207843137)   ; 31/53 
             (set-pt a 2 0.247058824 0.290196078)   ; 63/74
             (set-pt a 3 0.37254902 0.407843137)    ; 95/104
             (set-pt a 4 0.498039216 0.564705882)   ; 127/144
             (set-pt a 5 0.623529412 0.705882353)   ; 159/180
             (set-pt a 6 0.749019608 0.831372549)   ; 191/212 
             (set-pt a 7 0.874509804 0.933333333)   ; 223/238
             (set-pt a 8 1.0 0.97254902)            ; 255/248
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.11372549)            ; 0/29
             (set-pt a 1 0.121568627 0.164705882)   ; 31/42
             (set-pt a 2 0.247058824 0.247058824)   ; 63/63
             (set-pt a 3 0.37254902 0.376470588)    ; 95/96
             (set-pt a 4 0.498039216 0.533333333)   ; 127/136
             (set-pt a 5 0.623529412 0.674509804)   ; 159/172
             (set-pt a 6 0.749019608 0.803921569)   ; 191/205
             (set-pt a 7 0.874509804 0.917647059)   ; 223/234
             (set-pt a 8 1.0 0.960784314)           ; 255/245
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.082352941)           ; 0/21
             (set-pt a 1 0.121568627 0.11372549)    ; 31/29
             (set-pt a 2 0.247058824 0.2)           ; 63/51
             (set-pt a 3 0.37254902 0.325490196)    ; 95/83
             (set-pt a 4 0.498039216 0.454901961)   ; 127/116
             (set-pt a 5 0.623529412 0.592156863)   ; 159/151
             (set-pt a 6 0.749019608 0.741176471)   ; 191/189
             (set-pt a 7 0.874509804 0.854901961)   ; 223/218
             (set-pt a 8 1.0 0.921568627)           ; 255/235
          a)
       )
       (gimp-drawable-curves-spline merged HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline merged HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline merged HISTOGRAM-BLUE 18 (spline-blue))
       (gimp-drawable-curves-spline merged HISTOGRAM-VALUE 10 (spline-value))
        
    ))) ; end let begin if

    ;===========================================================
    ; Stinson
    ;=========================================================== 
    (if (= filter 35)
       (begin
       (create-layer-group "Stinson")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.192156863)          ; 0/49 
             (set-pt a 1 0.121568627 0.247058824)  ; 31/63 
             (set-pt a 2 0.247058824 0.341176471)  ; 63/87 
             (set-pt a 3 0.37254902 0.478431373)   ; 95/122 
             (set-pt a 4 0.498039216 0.611764706)  ; 127/156 
             (set-pt a 5 0.623529412 0.717647059)  ; 159/183 
             (set-pt a 6 0.749019608 0.811764706)  ; 191/207 
             (set-pt a 7 0.874509804 0.898039216)  ; 223/229 
             (set-pt a 8 1.0 0.925490196)          ; 255/236
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.160784314)          ; 0/41 
             (set-pt a 1 0.121568627 0.211764706)  ; 31/54 
             (set-pt a 2 0.247058824 0.298039216)  ; 63/76 
             (set-pt a 3 0.37254902 0.439215686)   ; 95/112 
             (set-pt a 4 0.498039216 0.580392157)  ; 127/148 
             (set-pt a 5 0.623529412 0.694117647)  ; 159/177 
             (set-pt a 6 0.749019608 0.788235294)  ; 191/201 
             (set-pt a 7 0.882352941 0.878431373)  ; 225/224 
             (set-pt a 8 1.0 0.901960784)          ; 255/230
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.156862745)          ; 0/40 
             (set-pt a 1 0.121568627 0.203921569)  ; 31/52 
             (set-pt a 2 0.247058824 0.282352941)  ; 63/72 
             (set-pt a 3 0.37254902 0.419607843)   ; 95/107 
             (set-pt a 4 0.498039216 0.560784314)  ; 127/143 
             (set-pt a 5 0.623529412 0.666666667)  ; 159/170 
             (set-pt a 6 0.749019608 0.756862745)  ; 191/193 
             (set-pt a 7 0.874509804 0.843137255)  ; 223/215 
             (set-pt a 8 1.0 0.882352941)          ; 255/225
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 
               
    )) ; end begin if

    ;===========================================================
    ; Sutro
    ;=========================================================== 
    (if (= filter 36)
       (begin
       (let*
          ( 
            (vl1 0)                        ; vignette layer 1
            (vl2 0)                        ; vignette layer 2
            (grad 0)                       ; new gradient map
            (actgrad 0)                    ; active gradient
            (merged 0)                     ; merged layer
            (leftcolor '(0 0 0))           ; left gradient color
            (rightcolor '(255 255 255))    ; right gradient color          
            (vw (* 80 (/ w 100)))          ; width of the rectangle, 80% of image width
            (vh (* 80 (/ h 100)))          ; height of the rectangle, 80% of image height
            (vx (* 10 (/ w 100)))          ; x coordinate of upper-left corner of rectangle
            (vy (* 10 (/ h 100)))          ; y coordinate of upper-left corner of rectangle
            (feather 0)                    ; feather
          )

       (if (>= w h) (set! feather (* w 0.3))) ; 30% from width
       (if (> h w) (set! feather (* h 0.3)))  ; 30% from heigt
       
       (create-layer-group "Sutro")

       ;----------------------------------------------------
       ; Create black and white layer
       ;----------------------------------------------------
       (set! bwl (car (gimp-layer-copy draw TRUE )))
       (gimp-image-insert-layer img bwl lg -1)
       (gimp-layer-set-opacity bwl 20)
       (gimp-layer-set-name bwl "BlackWhite")

       ;----------------------------------------------------
       ; Create new gradient
       ;----------------------------------------------------
       ; Save active gradient
       (set! actgrad (car (gimp-context-get-gradient)))       
       ; Create new gradient
       (set! grad (car (gimp-gradient-new "GimpBox Gradient")))
       ; Set left segment color       
       (gimp-gradient-segment-set-left-color grad 0 leftcolor 100)   
       ; Set right segment color
       (gimp-gradient-segment-set-right-color grad 0 rightcolor 100)	
       (gimp-context-set-gradient grad)
       (plug-in-gradmap RUN-NONINTERACTIVE img bwl)
       (gimp-gradient-delete grad)
       ; Restore active gradient
       (gimp-context-set-gradient actgrad)
       
       ; Merge layer down
       (set! merged (car (gimp-image-merge-down img bwl EXPAND-AS-NECESSARY)))

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.0)                   ; 0/0 
             (set-pt a 1 0.125490196 0.109803922)   ; 32/28 
             (set-pt a 2 0.250980392 0.239215686)   ; 64/61 
             (set-pt a 3 0.376470588 0.376470588)   ; 96/96 
             (set-pt a 4 0.501960784 0.517647059)   ; 128/132 
             (set-pt a 5 0.623529412 0.635294118)   ; 159/162 
             (set-pt a 6 0.749019608 0.749019608)   ; 191/191 
             (set-pt a 7 0.874509804 0.854901961)   ; 223/218 
             (set-pt a 8 1.0 0.956862745)           ; 255/244
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.0)           ; 0/0 
             (set-pt a 1 0.125490196 0.078431373)   ; 32/20 
             (set-pt a 2 0.250980392 0.192156863)   ; 64/49 
             (set-pt a 3 0.37254902 0.329411765)    ; 95/84 
             (set-pt a 4 0.498039216 0.470588235)   ; 127/120 
             (set-pt a 5 0.623529412 0.57254902)    ; 159/146 
             (set-pt a 6 0.749019608 0.674509804)   ; 191/173 
             (set-pt a 7 0.874509804 0.788235294)   ; 223/201 
             (set-pt a 8 1.0 0.898039216)           ; 255/229 
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 14 'double)))
             (set-pt a 0 0.0 0.0)                   ; 0/0 
             (set-pt a 1 0.37254902 0.352941176)    ; 95/90 
             (set-pt a 2 0.501960784 0.450980392)   ; 128/115 
             (set-pt a 3 0.623529412 0.525490196)   ; 159/134 
             (set-pt a 4 0.749019608 0.623529412)   ; 191/159 
             (set-pt a 5 0.874509804 0.749019608)   ; 223/191 
             (set-pt a 6 1.0 0.850980392)           ; 255/217 
          a)
       )
       (gimp-drawable-curves-spline merged HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline merged HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline merged HISTOGRAM-BLUE 14 (spline-blue)) 

       ;----------------------------------------------------
       ; Add vignette 1
       ;----------------------------------------------------
       ; opacity: 70%
       ; mode: 43 (burn / nachbelichten)
       ; color: a8a8a8 (RGB: 168 168 168)
       ;----------------------------------------------------
       
       ; Create a new layer
       (set! vl1 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette1" 70 LAYER-MODE-BURN)))
   
       ; Add the vignette layer to the group
       (gimp-image-insert-layer img vl1 lg -1)

       ; Create vignette
       (gimp-image-select-rectangle img CHANNEL-OP-ADD vx vy vw vh)
       (gimp-selection-feather img feather)
       (gimp-selection-invert img)
       (gimp-context-set-foreground '(168 168 168))
       (gimp-edit-fill vl1 FILL-FOREGROUND)
       (gimp-selection-clear img) 

       ;----------------------------------------------------
       ; Add vignette 2
       ;----------------------------------------------------
       ; opacity: 100%
       ; mode: 45 (soft light / Weiche Kanten)
       ; color: 111438 (RGB: 17 20 56)
       ;----------------------------------------------------
       
       ; Create a new layer
       (set! vl2 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette2" 100 LAYER-MODE-SOFTLIGHT)))
   
       ; Add the vignette layer to the group
       (gimp-image-insert-layer img vl2 lg -1)

       ; Create vignette
       (gimp-image-select-rectangle img CHANNEL-OP-ADD vx vy vw vh)
       (gimp-selection-feather img feather)
       (gimp-selection-invert img)
       (gimp-context-set-foreground '(17 20 56))
       (gimp-edit-fill vl2 FILL-FOREGROUND)
       (gimp-selection-clear img) 

    ))) ; end let begin if

    ;===========================================================
    ; Toaster
    ;==========================================================
    (if (= filter 37)
        (begin
        (let*
           ( 
             (grad 0)                             ; gradient map
             (actgrad 0)                          ; active gradient
             (tmpl)                               ; temp layer
             (leftcolor '(58 10 89))              ; left gradient color, HTML: #3a0a59
             (rightcolor '(254 169 87))           ; right gradient color, HTML: #fea957
             (ml1 0)                              ; mask layer 1
             (cl2 0)                              ; color layer 2
             (feather 0)
             (delta 0)
           )
        (create-layer-group "Toaster")


        ;----------------------------------------------------
        ; Create gradient layer
        ;----------------------------------------------------
        (set! bwl (car (gimp-layer-copy drawcopy TRUE )))
        (gimp-image-insert-layer img bwl lg -1)
        (gimp-layer-set-name bwl "Gradient")

        ;----------------------------------------------------
        ; Create gradient
        ;----------------------------------------------------
        ; Save active gradient
        (set! actgrad (car (gimp-context-get-gradient)))       
        ; Create new gradient
        (set! grad (car (gimp-gradient-new "GimpBox Gradient")))
        ; Set left segment color       
        (gimp-gradient-segment-set-left-color grad 0 leftcolor 100)   
        ; Set right segment color
        (gimp-gradient-segment-set-right-color grad 0 rightcolor 100)	
        (gimp-context-set-gradient grad)
        (plug-in-gradmap RUN-NONINTERACTIVE img bwl)
        (gimp-gradient-delete grad)
        ; Restore active gradient
        (gimp-context-set-gradient actgrad)
        ; Set layer opacity to 70%
        ;(gimp-layer-set-opacity bwl 70)
        ; Set layer mode to overlay
        (gimp-layer-set-mode bwl LAYER-MODE-OVERLAY)
       
        ;----------------------------------------------------
        ; Create color layer 1
        ;----------------------------------------------------
        ; Type: RGB
        ; Opacity: 100%
        ; Mode: 36 (lighten only / nur aufhellen)     
        (set! cl (car (gimp-layer-new img w h RGB-IMAGE "Color 1" 100 LAYER-MODE-LIGHTEN-ONLY)))
        
        ; Add the new layer to the group
        (gimp-image-insert-layer img cl lg -1)
        ; Set foreground color
        (gimp-context-set-foreground '(65 0 65)) ; HTML #410041
        ; Fill the color layer with foreground color
        (gimp-drawable-fill cl 0)
      
        ;----------------------------------------------------
        ; Create color layer 2
        ;----------------------------------------------------
        (set! cl2 (car (gimp-layer-copy drawcopy TRUE )))
        (gimp-image-insert-layer img cl2 lg -1)
        (gimp-layer-set-name cl2 "Color 2")

        ;----------------------------------------------------
        ; Ajust curves color
        ;----------------------------------------------------
	    ; Red channel
	    (define (spline-red)
	      (let* ((a (cons-array 18 'double)))
	         (set-pt a 0 0.0 0.545098039)          ; 0/139 
	         (set-pt a 1 0.121568627 0.623529412)  ; 31/159 
	         (set-pt a 2 0.247058824 0.690196078)  ; 63/176 
	         (set-pt a 3 0.37254902 0.752941176)   ; 95/192 
	         (set-pt a 4 0.498039216 0.811764706)  ; 127/207 
	         (set-pt a 5 0.623529412 0.866666667)  ; 159/221 
	         (set-pt a 6 0.749019608 0.921568627)  ; 191/235 
	         (set-pt a 7 0.874509804 0.964705882)  ; 223/246 
	         (set-pt a 8 1.0 0.996078431)          ; 255/254
	      a)
	    )
	    ; Green channel
	    (define (spline-green)
	      (let* ((a (cons-array 18 'double)))
	         (set-pt a 0 0.0 0.015686275)          ; 0/4 
	         (set-pt a 1 0.121568627 0.28627451)   ; 31/73 
	         (set-pt a 2 0.247058824 0.466666667)  ; 63/119 
	         (set-pt a 3 0.37254902 0.603921569)   ; 95/154 
	         (set-pt a 4 0.498039216 0.705882353)  ; 127/180 
	         (set-pt a 5 0.623529412 0.792156863)  ; 159/202 
	         (set-pt a 6 0.749019608 0.874509804)  ; 191/223 
	         (set-pt a 7 0.874509804 0.933333333)  ; 223/238 
	         (set-pt a 8 1.0 0.988235294)          ; 255/252
	      a)
	    )        
	    ; Blue channel
	    (define (spline-blue)
	      (let* ((a (cons-array 22 'double)))
	         (set-pt a 0 0.0 0.22745098)           ; 0/58 
			 (set-pt a 1 0.235294118 0.235294118)  ; 18/60 
			 (set-pt a 2 0.207843137 0.250980392)  ; 53/64 
			 (set-pt a 3 0.250980392 0.294117647)  ; 64/75 
			 (set-pt a 4 0.31372549 0.380392157)   ; 80/97 
	         (set-pt a 5 0.411764706 0.501960784)  ; 105/128 
	         (set-pt a 6 0.498039216 0.580392157)  ; 127/148 
	         (set-pt a 7 0.623529412 0.670588235)  ; 159/171 
	         (set-pt a 8 0.749019608 0.745098039)  ; 191/190 
	         (set-pt a 9 0.874509804 0.796078431)  ; 223/203       
	         (set-pt a 10 1.0 0.82745098)          ; 255/211
	      a)
	    )
	   (gimp-drawable-curves-spline cl2 HISTOGRAM-RED 18 (spline-red)) 
	   (gimp-drawable-curves-spline cl2 HISTOGRAM-GREEN 18 (spline-green)) 
	   (gimp-drawable-curves-spline cl2 HISTOGRAM-BLUE 22 (spline-blue)) 

       ;----------------------------------------------------
       ; Add mask to color layer
       ;----------------------------------------------------
       (set! feather (* (/ lat 100) 30)) ; 30% of image width
       (set! delta (* (/ lat 100) 10)) ; 10% of image width
       (set! ml1 (car (gimp-layer-create-mask cl2 ADD-MASK-BLACK )))
       (gimp-layer-add-mask cl2 ml1)
       (gimp-image-select-ellipse img CHANNEL-OP-ADD delta delta (- w (* delta 2)) (- h (* delta 2)))
       (gimp-selection-feather img feather)
       (gimp-context-set-foreground '(255 255 255))
       (gimp-edit-fill ml1 FILL-FOREGROUND)
       (gimp-selection-clear img)

    ))) ; end let begin if

    ;===========================================================
    ; Valencia
    ;=========================================================== 
    (if (= filter 38)
       (begin
       (create-layer-group "Valencia")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.078431373)           ; 0/20 
             (set-pt a 1 0.125490196 0.219607843)   ; 32/56 
             (set-pt a 2 0.250980392 0.364705882)   ; 64/93 
             (set-pt a 3 0.376470588 0.494117647)   ; 96/126 
             (set-pt a 4 0.501960784 0.623529412)   ; 128/159 
             (set-pt a 5 0.623529412 0.737254902)   ; 159/188 
             (set-pt a 6 0.752941176 0.823529412)   ; 192/210 
             (set-pt a 7 0.874509804 0.898039216)   ; 223/229 
             (set-pt a 8 1.0 0.964705882)           ; 255/246
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 20 'double)))
             (set-pt a 0 0.0 0.0)                   ; 0/0 
             (set-pt a 1 0.031372549 0.011764706)   ; 8/3 
             (set-pt a 2 0.125490196 0.125490196)   ; 32/32 
             (set-pt a 3 0.250980392 0.298039216)   ; 64/76 
             (set-pt a 4 0.376470588 0.462745098)   ; 96/118 
             (set-pt a 5 0.501960784 0.615686275)   ; 128/157 
             (set-pt a 6 0.623529412 0.737254902)   ; 159/188 
             (set-pt a 7 0.749019608 0.839215686)   ; 191/214 
             (set-pt a 8 0.874509804 0.921568627)   ; 223/235 
             (set-pt a 9 1.0 0.988235294)           ; 255/252
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.066666667)           ; 0/17 
             (set-pt a 1 0.121568627 0.192156863)   ; 31/49 
             (set-pt a 2 0.258823529 0.337254902)   ; 66/86 
             (set-pt a 3 0.376470588 0.458823529)   ; 96/117 
             (set-pt a 4 0.498039216 0.568627451)   ; 127/145 
             (set-pt a 5 0.623529412 0.670588235)   ; 159/171 
             (set-pt a 6 0.749019608 0.764705882)   ; 191/195 
             (set-pt a 7 0.874509804 0.850980392)   ; 223/217 
             (set-pt a 8 1.0 0.909803922)           ; 255/232
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 20 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue))

    )) ; end begin if

    ;===========================================================
    ; Vesper
    ;=========================================================== 
    (if (= filter 39)
       (begin
       (create-layer-group "Vesper")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 26 'double)))
             (set-pt a 0 0.0 0.145098039)           ; 0/37 
             (set-pt a 1 0.023529412 0.156862745)   ; 6/40 
             (set-pt a 2 0.125490196 0.188235294)   ; 32/48 
             (set-pt a 3 0.219607843 0.254901961)   ; 56/65 
             (set-pt a 4 0.31372549 0.376470588)    ; 80/96 
             (set-pt a 5 0.368627451 0.501960784)   ; 94/128 
             (set-pt a 6 0.435294118 0.62745098)    ; 111/160 
             (set-pt a 7 0.501960784 0.701960784)   ; 128/179 
             (set-pt a 8 0.623529412 0.807843137)   ; 159/206 
             (set-pt a 9 0.749019608 0.898039216)   ; 191/229 
             (set-pt a 10 0.874509804 0.960784314)  ; 223/245 
             (set-pt a 11 0.937254902 0.988235294)  ; 239/252 
             (set-pt a 12 1.0 1.0)                  ; 255/255
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 20 'double)))
             (set-pt a 0 0.0 0.074509804)           ; 0/19 
             (set-pt a 1 0.121568627 0.149019608)   ; 31/38 
             (set-pt a 2 0.250980392 0.294117647)   ; 64/75 
             (set-pt a 3 0.360784314 0.501960784)   ; 92/128 
             (set-pt a 4 0.435294118 0.62745098)    ; 111/160 
             (set-pt a 5 0.501960784 0.694117647)   ; 128/177 
             (set-pt a 6 0.623529412 0.788235294)   ; 159/201 
             (set-pt a 7 0.749019608 0.862745098)   ; 191/220 
             (set-pt a 8 0.874509804 0.929411765)   ; 223/237 
             (set-pt a 9 1.0 0.996078431)           ; 255/254
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 22 'double)))
             (set-pt a 0 0.0 0.08627451)            ; 0/22 
             (set-pt a 1 0.019607843 0.117647059)   ; 5/30 
             (set-pt a 2 0.125490196 0.22745098)    ; 32/58 
             (set-pt a 3 0.250980392 0.360784314)   ; 64/92 
             (set-pt a 4 0.360784314 0.501960784)   ; 92/128 
             (set-pt a 5 0.42745098 0.57254902)     ; 109/146 
             (set-pt a 6 0.501960784 0.623529412)   ; 128/159 
             (set-pt a 7 0.62745098 0.709803922)    ; 160/181 
             (set-pt a 8 0.749019608 0.807843137)   ; 191/206 
             (set-pt a 9 0.874509804 0.894117647)   ; 223/228 
             (set-pt a 10 1.0 0.97254902)           ; 255/248
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 26 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 20 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 22 (spline-blue)) 
               
    )) ; end begin if

    ;===========================================================
    ; Walden
    ;=========================================================== 
    (if (= filter 40)
       (begin
       (let*
          ( 
            (vl1 0)                        ; vignette layer 1
            (feather (* (/ lat 100) 20))   ; 20% of the longest image side
            (delta (* (/ lat 100) 5))      ; 5% of the longest image side
            (ex (+ 0 delta))               ; x coordinate of upper-left corner of ellipse bounding box
            (ey (+ 0 delta))               ; y coordinate of upper-left corner of ellipse bounding box
            (ew (- w (* delta 2)))         ; width of the ellipse
            (eh (- h (* delta 2)))         ; height of the ellipse            
          )
       (create-layer-group "Walden")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.039215686)           ; 0/10 
             (set-pt a 1 0.125490196 0.145098039)   ; 32/37 
             (set-pt a 2 0.247058824 0.321568627)   ; 63/82 
             (set-pt a 3 0.37254902 0.537254902)    ; 95/137 
             (set-pt a 4 0.498039216 0.71372549)    ; 127/182 
             (set-pt a 5 0.623529412 0.843137255)   ; 159/215 
             (set-pt a 6 0.749019608 0.921568627)   ; 191/235 
             (set-pt a 7 0.874509804 0.968627451)   ; 223/247 
             (set-pt a 8 1.0 0.984313725)           ; 255/251
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.149019608)           ; 0/38 
             (set-pt a 1 0.125490196 0.270588235)   ; 32/69 
             (set-pt a 2 0.247058824 0.42745098)    ; 63/109 
             (set-pt a 3 0.37254902 0.584313725)    ; 95/149 
             (set-pt a 4 0.498039216 0.705882353)   ; 127/180 
             (set-pt a 5 0.623529412 0.803921569)   ; 159/205 
             (set-pt a 6 0.749019608 0.878431373)   ; 191/224 
             (set-pt a 7 0.874509804 0.925490196)   ; 223/236 
             (set-pt a 8 1.0 0.960784314)           ; 255/245
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.341176471)           ; 0/87 
             (set-pt a 1 0.121568627 0.42745098)    ; 31/109 
             (set-pt a 2 0.247058824 0.521568627)   ; 63/133 
             (set-pt a 3 0.37254902 0.607843137)    ; 95/155 
             (set-pt a 4 0.498039216 0.678431373)   ; 127/173 
             (set-pt a 5 0.623529412 0.737254902)   ; 159/188 
             (set-pt a 6 0.749019608 0.792156863)   ; 191/202 
             (set-pt a 7 0.874509804 0.835294118)   ; 223/213 
             (set-pt a 8 1.0 0.874509804)           ; 255/223
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 18 (spline-blue)) 

       ;----------------------------------------------------
       ; Add vignette
       ;
       ; opacity: 60%
       ; mode: 23 (overlay / ueberlagern)
       ; color: black
       ;----------------------------------------------------

       ; Create vignette layer
       (set! vl1 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette" 60 LAYER-MODE-OVERLAY)))
   
       ; Add vignette layer to the group
       (gimp-image-insert-layer img vl1 lg -1)

       ; Create vignette
       (gimp-image-select-ellipse img CHANNEL-OP-ADD ex ey ew eh)
       (gimp-selection-feather img feather)
       (gimp-selection-invert img)
       (gimp-context-set-foreground '(0 0 0))
       (gimp-edit-fill vl1 FILL-FOREGROUND)
       (gimp-selection-clear img)
           
     ))) ; end let begin if

     ;===========================================================
     ; Willow
     ;===========================================================
     (if (= filter 41)
     (begin
       (let*
          ( 
            (dl1 0)                        ; decompose layer
            (ml 0)                         ; mask layer
            (vl1 0)                        ; vignette layer
            (feather 0)
            (delta 0)            
          )
       (create-layer-group "Willow")

       ;----------------------------------------------------
       ; Decompose image into YCbCr_ITU_R470
       ;----------------------------------------------------
       (define decompose-ycbcr (plug-in-decompose RUN-NONINTERACTIVE img drawcopy "YCbCr_ITU_R470" 0))
       (define lyr (gimp-layer-new-from-drawable ( car( gimp-image-get-active-layer (car decompose-ycbcr) )) img))
       (set! dl1 (car lyr))
       (gimp-image-remove-layer img drawcopy)
       (gimp-item-set-name dl1 "Background")
       (gimp-image-insert-layer img dl1 lg -1)
       
       ; Set foreground color
       (gimp-context-set-foreground '(81 70 77)) ; HTML #51464d

       ;----------------------------------------------------
       ; Create color layer
       ;----------------------------------------------------
       ; Type: RGB
       ; Opacity: 40%
       ; Mode: 45 (soft light / weiche Kanten)
       (set! cl (car (gimp-layer-new img w h RGB-IMAGE "Color" 40 LAYER-MODE-SOFTLIGHT)))
        
       ;----------------------------------------------------
       ; Add the new layer to the group and fill the color layer with foreground color
       ;----------------------------------------------------
       (gimp-image-insert-layer img cl lg -1)
       (gimp-drawable-fill cl 0)       
       
       ;----------------------------------------------------
       ; Copy decomposed layer 
       ;----------------------------------------------------
       (set! bwl (car (gimp-layer-copy dl1 TRUE )))
       (gimp-image-insert-layer img bwl lg -1)
       (gimp-item-set-name bwl "Mask")       

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Value
       (define (spline-value)
           (let* ((a (cons-array 18 'double)))
              (set-pt a 0 0.0 0.11372549)            ; 0/29 
              (set-pt a 1 0.121568627 0.176470588)   ; 31/45 
              (set-pt a 2 0.247058824 0.266666667)   ; 63/68 
              (set-pt a 3 0.37254902 0.368627451)    ; 95/94 
              (set-pt a 4 0.498039216 0.478431373)   ; 127/122 
              (set-pt a 5 0.623529412 0.588235294)   ; 159/150 
              (set-pt a 6 0.749019608 0.701960784)   ; 191/179 
              (set-pt a 7 0.874509804 0.796078431)   ; 223/203 
              (set-pt a 8 1.0 0.866666667)           ; 255/221
           a)
       )
       (gimp-drawable-curves-spline dl1 HISTOGRAM-VALUE 18 (spline-value))
	   
       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
	   (define (spline-red)
	      (let* ((a (cons-array 18 'double)))
	         (set-pt a 0 0.0 0.121568627)          ; 0/31 
	         (set-pt a 1 0.121568627 0.219607843)  ; 31/56 
	         (set-pt a 2 0.247058824 0.380392157)  ; 63/97 
	         (set-pt a 3 0.37254902 0.556862745)   ; 95/142 
	         (set-pt a 4 0.498039216 0.709803922)  ; 127/181 
	         (set-pt a 5 0.623529412 0.8)          ; 159/204 
	         (set-pt a 6 0.749019608 0.858823529)  ; 191/219 
	         (set-pt a 7 0.874509804 0.901960784)  ; 223/230 
	         (set-pt a 8 1.0 0.945098039)          ; 255/241
	      a)
	   )
	   ; Green channel
	   (define (spline-green)
	      (let* ((a (cons-array 18 'double)))
	         (set-pt a 0 0.0 0.121568627)          ; 0/31 
	         (set-pt a 1 0.121568627 0.219607843)  ; 31/56 
	         (set-pt a 2 0.247058824 0.376470588)  ; 63/96 
	         (set-pt a 3 0.37254902 0.541176471)   ; 95/138 
	         (set-pt a 4 0.498039216 0.68627451)   ; 127/175 
	         (set-pt a 5 0.623529412 0.764705882)  ; 159/195 
	         (set-pt a 6 0.749019608 0.819607843)  ; 191/209 
	         (set-pt a 7 0.874509804 0.858823529)  ; 223/219 
	         (set-pt a 8 1.0 0.898039216)          ; 255/229
	      a)
	   )        
	   ; Blue channel
	   (define (spline-blue)
	      (let* ((a (cons-array 18 'double)))
	         (set-pt a 0 0.0 0.121568627)          ; 0/31 
	         (set-pt a 1 0.121568627 0.219607843)  ; 31/56 
	         (set-pt a 2 0.247058824 0.376470588)  ; 63/96 
	         (set-pt a 3 0.37254902 0.541176471)   ; 95/138 
	         (set-pt a 4 0.498039216 0.68627451)   ; 127/175 
	         (set-pt a 5 0.623529412 0.764705882)  ; 159/195 
	         (set-pt a 6 0.749019608 0.819607843)  ; 191/209 
	         (set-pt a 7 0.874509804 0.858823529)  ; 223/219 
	         (set-pt a 8 1.0 0.898039216)          ; 255/229
	      a)
	   )
       (gimp-drawable-curves-spline bwl HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline bwl HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline bwl HISTOGRAM-BLUE 18 (spline-blue)) 

       ;----------------------------------------------------
       ; Add mask to bwl layer
       ;----------------------------------------------------
       (set! feather (* (/ lat 100) 40)) ; 40% of image width
       (set! delta (* (/ lat 100) 15)) ; 15% of image width
       (set! ml (car (gimp-layer-create-mask bwl ADD-MASK-BLACK )))
       (gimp-layer-add-mask bwl ml)
       (gimp-image-select-ellipse img CHANNEL-OP-ADD delta delta (- w (* delta 2)) (- h (* delta 2)))
       (gimp-selection-feather img feather)
       (gimp-context-set-foreground '(255 255 255))
       (gimp-edit-fill ml FILL-FOREGROUND)
       (gimp-selection-clear img)	   

     ))) ; end led begin if

     ;===========================================================
     ; X-Pro II
     ;===========================================================
     (if (= filter 42)
     (begin
       (let*
          ( 
            (vl1 0)                        ; vignette layer
            (feather (* (/ lat 100) 30))   ; 30% of the longest image side
            (delta (* (/ lat 100) 5))      ; 5% of the longest image side
            (ex (+ 0 delta))               ; x coordinate of upper-left corner of ellipse bounding box
            (ey (+ 0 delta))               ; y coordinate of upper-left corner of ellipse bounding box
            (ew (- w (* delta 2)))         ; width of the ellipse
            (eh (- h (* delta 2)))         ; height of the ellipse
          )
       (create-layer-group "X-Pro II")

       ;----------------------------------------------------
       ; Ajust curves color
       ;----------------------------------------------------
       ; Red channel
       (define (spline-red)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.0)                   ; 0/0 
             (set-pt a 1 0.121568627 0.066666667)   ; 31/17 
             (set-pt a 2 0.247058824 0.184313725)   ; 63/47 
             (set-pt a 3 0.37254902 0.337254902)    ; 95/86 
             (set-pt a 4 0.498039216 0.498039216)   ; 127/127 
             (set-pt a 5 0.623529412 0.666666667)   ; 159/170 
             (set-pt a 6 0.749019608 0.819607843)   ; 191/209 
             (set-pt a 7 0.874509804 0.933333333)   ; 223/238 
             (set-pt a 8 1.0 1.0)                   ; 255/255
          a)
       )
       ; Green channel
       (define (spline-green)
          (let* ((a (cons-array 18 'double)))
             (set-pt a 0 0.0 0.0)                   ; 0/0 
             (set-pt a 1 0.121568627 0.066666667)   ; 31/17 
             (set-pt a 2 0.247058824 0.184313725)   ; 63/47 
             (set-pt a 3 0.37254902 0.337254902)    ; 95/86 
             (set-pt a 4 0.498039216 0.498039216)   ; 127/127 
             (set-pt a 5 0.623529412 0.666666667)   ; 159/170 
             (set-pt a 6 0.749019608 0.819607843)   ; 191/209 
             (set-pt a 7 0.874509804 0.933333333)   ; 223/238 
             (set-pt a 8 1.0 1.0)                   ; 255/255
          a)
       )        
       ; Blue channel
       (define (spline-blue)
          (let* ((a (cons-array 6 'double)))
             (set-pt a 0 0.0 0.105882353)            ; 0/27 
             (set-pt a 1 0.498039216 0.498039216)    ; 127/127 
             (set-pt a 2 1.0 0.894117647)            ; 255/228
          a)
       )
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-RED 18 (spline-red)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-GREEN 18 (spline-green)) 
       (gimp-drawable-curves-spline drawcopy HISTOGRAM-BLUE 6 (spline-blue)) 

       ;----------------------------------------------------
       ; Add vignette
       ;
       ; opacity: 100%
       ; mode: 23 (overlay / ueberlagern)
       ; Color: 111438 (RGB 17 20 56)
       ;----------------------------------------------------

       ; Create vignette layer
       (set! vl1 (car (gimp-layer-new img w h RGBA-IMAGE "Vignette" 100 LAYER-MODE-OVERLAY)))
   
       ; Add vignette layer to the group
       (gimp-image-insert-layer img vl1 lg -1)

       ; Create vignette
       (gimp-image-select-ellipse img CHANNEL-OP-ADD ex ey ew eh)
       (gimp-selection-feather img feather)
       (gimp-selection-invert img)
       (gimp-context-set-foreground '(17 20 56))
       (gimp-edit-fill vl1 FILL-FOREGROUND)
       (gimp-selection-clear img)

     ))) ; end led begin if

    ;===========================================================
    ; Add border
    ;===========================================================
    (if (> bctype 0)
    (begin
        (let*
           (
             (bl 0)                             ; border layer
             (border-size 2)                    ; border size, default 2% of longest image side
             (border-color '(255 255 255))      ; default border color: white
           )
           (if (= bctype 1)  ; black border color
           (begin
              (set! border-color '(0 0 0))
           ))
           (if (> bs 5) 
           (begin
              (set! bs 5)
           ))
           (if (< bs 2) 
           (begin
              (set! bs 2)
           ))
           (set! border-size (* bs (/ lat 100)))

           ;----------------------------------------------------
           ; Create a new layer
           ;----------------------------------------------------
           ; Type: RGBA (transparent)
           ; Mode: 28 (normal)
           ; Opacity: 100%
           (set! bl (car (gimp-layer-new img w h RGBA-IMAGE "Border" 100 LAYER-MODE-NORMAL)))
   
           ;----------------------------------------------------
           ; Add the border layer to the image
           ;----------------------------------------------------
           (gimp-image-insert-layer img bl lg -1)
           (gimp-image-select-rectangle img CHANNEL-OP-ADD border-size border-size (- w (* 2 border-size))  (- h (* 2 border-size)) )
           (gimp-selection-invert img )
           (gimp-context-set-foreground border-color) 
           (gimp-edit-fill bl FILL-FOREGROUND)
           (gimp-selection-clear img)
        ) ; end let
    )) ; end if

    ;----------------------------------------------------
    ; Merge group layers 
    ;----------------------------------------------------
    (if (equal? merge-layers TRUE)
       (begin
         (gimp-image-merge-layer-group img lg)
       )
    )  

    ;----------------------------------------------------
    ; Restore foreground and background colors
    ;----------------------------------------------------
    (gimp-context-set-foreground fgcolor)
    (gimp-context-set-background bgcolor)

    ) ; let* variables definition

    ; Ensure the updated image is displayed now
    (gimp-displays-flush)

    ; Complete the undo group
    (gimp-image-undo-group-end img)
	
    ) ; end begin
    (gimp-message "The active drawable must be an image layer!")
    ) ; end if

)



(script-fu-register 
    "script-fu-gimpbox-instagram-filters"
    "Instagram..."
    "This script contains 43 instagram-like filters"
    "GimpBox (https://github.com/gimpbox/)"
    "GimpBox"
    "Last update 14.03.2023, Version 1.00"
    "RGB*"
    SF-IMAGE      "Image"              0
    SF-DRAWABLE   "Layer"              0
    SF-OPTION     "Filter"             '("1977" "Aden" "Amaro" "Apollo" "Ashby" "Brannan" "Brooklyn" "Charmes" "Clarendon" "Crema" "Dogpatch" "Earlybird" "Gingham" "Ginza" "Gotham" "Hefe" "Helena" "Hudson" "Inkwell" "Juno" "Kelvin" "Lark" "Lo-Fi" "Ludwig" "Maven" "Mayfair" "Moon" "Nashville" "Perpetua" "Poprocket" "Reyes" "Rise" "Sierra" "Skyline" "Slumber" "Stinson" "Sutro" "Toaster" "Valencia" "Vesper" "Walden" "Willow" "X-Pro II")
    SF-OPTION     "Border color"       '("None" "Black" "White")
    SF-ADJUSTMENT "Border size in %" '(2 2 5 0.5 0.5 1 1)
    SF-TOGGLE     "Merge layers"       FALSE
)

(script-fu-menu-register
    "script-fu-gimpbox-instagram-filters"
    _"<Image>/Filters/Gimpbox"
)
