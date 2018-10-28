#lang pollen

◊(define maximalSize 28.28 #| Maximize width: 2.5 * sqrt(2*(x)^2)=100 => x=28.28 |#)
◊(define strokeWidth 2)
◊(define gap (+ 3 strokeWidth))
◊(define size (- maximalSize gap ))
◊(define halfGap (/ gap 2.0))
◊(define (offset count) (* count (+ size gap)))
◊(define startX (+ 0 halfGap))
◊(define startY (+ 0 halfGap))
◊(define (offsetX count) (+ startX (offset count)))
◊(define (offsetY count) (+ startY (offset count)))

◊(define checkmarkSize 5)
◊(define (chkSize count) (* checkmarkSize count))
◊(define (checkmarkOffsetX count) (+ (offsetX count) (/ (- size (* 3 checkmarkSize)) 2)))
◊(define (checkmarkOffsetY count) (+ (offsetY count) (/ (- size (* 2 checkmarkSize)) 2)))

◊(define diagonal (sqrt (* (expt (+ size gap) 2) 2)))
◊(define totalHeight (* diagonal 2.5))
◊(define padding (- 100 totalHeight))
◊(define leftPadding (/ padding 2))

<svg width="100" height="100" viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
    <def>
        <g id="checkmark" fill="#fff" stroke="none">
                <path d="M0 0 v ◊(chkSize 2) h ◊(chkSize 3) v ◊(chkSize -1) h ◊(chkSize -2) v ◊(chkSize -1) Z"/>
        </g>
    </def>
    <g style="transform-origin: center; transform: scale(0.95);">
        <g transform="translate(◊leftPadding 50) rotate(-45)" fill="#50bf00" stroke="#fff" stroke-width="◊strokeWidth" filter="url(#dropshadow)">
            <rect width="◊size" height="◊size" x="◊startX" y="◊startY"/>
            <rect width="◊size" height="◊size" x="◊(offsetX 0)" y="◊(offsetY 1)"/>
            <rect width="◊size" height="◊size" x="◊(offsetX 1)" y="◊(offsetY 1)"/>
            <rect width="◊size" height="◊size" x="◊(offsetX 2)" y="◊(offsetY 1)"/>
            <use href="#checkmark" x="◊(checkmarkOffsetX 0)" y="◊(checkmarkOffsetY 0)"/>
            <use href="#checkmark" x="◊(checkmarkOffsetX 0)" y="◊(checkmarkOffsetY 1)"/>
            <use href="#checkmark" x="◊(checkmarkOffsetX 2)" y="◊(checkmarkOffsetY 1)"/>
        </g>
    </g>
    <filter id="dropshadow" width="130%" height="130%">
        <feGaussianBlur in="SourceAlpha" stdDeviation="2"/>
        <feOffset dx="1.5" dy="1.5" result="offsetblur"/>
        <feComponentTransfer>
            <feFuncA type="linear" slope="0.5"/>
        </feComponentTransfer>
        <feMerge> 
            <feMergeNode/>
            <feMergeNode in="SourceGraphic"/>
        </feMerge>
    </filter>
</svg>
