   10 print"{clr}":poke53281,0:poke53280,0
   15 fora=0to15:poke1024+a+40*15,160:next
   16 fora=25to39:poke1024+a+40*15,160:next
   20 x=165:y=64:dx=0:dy=0:r=0
   30 poke2040,13:fors=0to62:readsd:poke832+s,sd:next
   40 v=53248
   50 pokev+21,1
   60 pokev+39,3
   70 pokev+23,1:pokev+29,1
  100 geta$
  110 ifa$="{up}"thendy=dy-1
  120 ifa$="{down}"thendy=dy+1
  130 ifa$="{left}"thendx=dx-1
  140 ifa$="{rght}"thendx=dx+1
  150 ifa$=" "thendx=0:dy=0:x=165:y=64
  160 dx=dx*.95
  170 dy=dy+.3
  180 x=x+dx:y=y+dy
  190 ifx<0andr=0thenx=89:r=1
  200 ifx>255andr=0thenx=0:r=1
  210 ifx<0andr=1thenx=255:r=0
  220 ifx>89andr=1thenx=0:r=0
  230 ify<1theny=1:dy=0
  240 ify>208theny=208:dy=-dy*.6
  245 if(peek(v+31)and1)thenpoke53280,(peek(53280)+1)and15:y=y-dy*2:dy=-dy*.6
  250 ifr=0thenpokev+16,0:goto270
  260 pokev+16,1
  270 pokev,x
  280 pokev+1,y
  290 goto100
  300 data 0,127,0,1,255,192,3,255,224,3,231,224
  310 data 7,217,240,7,223,240,7,217,240,3,231,224
  320 data 3,255,224,3,255,224,2,255,160,1,127,64
  330 data 1,62,64,0,156,128,0,156,128,0,73,0,0,73,0
  340 data 0,62,0,0,62,0,0,62,0,0,28,0
