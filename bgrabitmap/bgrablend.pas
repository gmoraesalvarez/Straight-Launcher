unit BGRABlend;

{ This unit contains pixel blending functions. They take a destination adress as parameter,
  and draw pixels at this address with different blending modes. These functions are used
  by many functions in BGRABitmap library to do the low level drawing. }

{$mode objfpc}{$H+}

interface

uses
  BGRABitmapTypes;

{ Draw one pixel with alpha blending }
procedure DrawPixelInlineWithAlphaCheck(dest: PBGRAPixel; const c: TBGRAPixel); inline; overload;
procedure DrawPixelInlineWithAlphaCheck(dest: PBGRAPixel; c: TBGRAPixel; appliedOpacity: byte); inline; overload;
procedure DrawExpandedPixelInlineWithAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel); inline; overload;
procedure DrawPixelInlineExpandedOrNotWithAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel; c: TBGRAPixel); inline; overload;  //alpha in 'c' parameter
procedure DrawPixelInlineNoAlphaCheck(dest: PBGRAPixel; const c: TBGRAPixel); inline; overload;
procedure DrawExpandedPixelInlineNoAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel; calpha: byte); inline; overload;
procedure ClearTypeDrawPixel(pdest: PBGRAPixel; Cr, Cg, Cb: byte; Color: TBGRAPixel); inline;

procedure CopyPixelsWithOpacity(dest,src: PBGRAPixel; opacity: byte; Count: integer); inline;
function ApplyOpacity(opacity1,opacity2: byte): byte; inline;
function FastRoundDiv255(value: cardinal): cardinal; inline;

{ Draw a series of pixels with alpha blending }
procedure DrawPixelsInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline; overload;
procedure DrawExpandedPixelsInline(dest: PBGRAPixel; ec: TExpandedPixel; Count: integer); inline; overload;
procedure DrawPixelsInlineExpandedOrNot(dest: PBGRAPixel; ec: TExpandedPixel; c: TBGRAPixel; Count: integer); inline; overload;  //alpha in 'c' parameter

{ Draw one pixel with linear alpha blending }
procedure FastBlendPixelInline(dest: PBGRAPixel; const c: TBGRAPixel); inline; overload;
procedure FastBlendPixelInline(dest: PBGRAPixel; c: TBGRAPixel; appliedOpacity: byte); inline; overload;

{ Draw a series of pixels with linear alpha blending }
procedure FastBlendPixelsInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;

{ Replace a series of pixels }
procedure FillInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;

{ Xor a series of pixels }
procedure XorInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;
procedure XorPixels(pdest, psrc: PBGRAPixel; count: integer);

{ Set alpha value for a series of pixels }
procedure AlphaFillInline(dest: PBGRAPixel; alpha: byte; Count: integer); inline;

{ Erase a series of pixels, i.e. decrease alpha value }
procedure ErasePixelInline(dest: PBGRAPixel; alpha: byte); inline;

{ Draw a pixel to the extent the current pixel is close enough to compare value.
  It should not be called on pixels that have not been checked to be close enough }
procedure DrawPixelInlineDiff(dest: PBGRAPixel; c, compare: TBGRAPixel;
  maxDiff: byte); inline;
{ Draw a series of pixel to the extent the current pixel is close enough to compare value }
procedure DrawPixelsInlineDiff(dest: PBGRAPixel; c: TBGRAPixel;
  Count: integer; compare: TBGRAPixel; maxDiff: byte); inline;

{ Blend pixels with scanner content }
procedure ScannerPutPixels(scan: IBGRAScanner; pdest: PBGRAPixel; count: integer; mode: TDrawMode);

{ Perform advanced blending operation }
procedure BlendPixels(pdest: PBGRAPixel; psrc: PBGRAPixel;
  blendOp: TBlendOperation; Count: integer);

//layer blend modes ( http://www.pegtop.net/delphi/articles/blendmodes/ )
procedure LinearMultiplyPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure AddPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearAddPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ColorBurnPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ColorDodgePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ReflectPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure NonLinearReflectPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure GlowPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure NiceGlowPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure OverlayPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearOverlayPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure DifferencePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearDifferencePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure NegationPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LinearNegationPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure LightenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure DarkenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure ScreenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
procedure XorPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;

implementation

procedure ClearTypeDrawPixel(pdest: PBGRAPixel; Cr, Cg, Cb: byte; Color: TBGRAPixel);
var merge,mergeClearType: TBGRAPixel;
    acc: word;
    keep,dont_keep: byte;
begin
  Cr := ApplyOpacity(Cr,color.alpha);
  Cg := ApplyOpacity(Cg,color.alpha);
  Cb := ApplyOpacity(Cb,color.alpha);
  acc := Cr+Cg+Cb;
  if acc = 0 then exit;

  merge := pdest^;
  mergeClearType.red := GammaCompressionTab[(GammaExpansionTab[merge.red] * (not byte(Cr)) +
                GammaExpansionTab[color.red] * Cr + 128) div 255];
  mergeClearType.green := GammaCompressionTab[(GammaExpansionTab[merge.green] * (not byte(Cg)) +
                GammaExpansionTab[color.green] * Cg + 128) div 255];
  mergeClearType.blue := GammaCompressionTab[(GammaExpansionTab[merge.blue] * (not byte(Cb)) +
                GammaExpansionTab[color.blue] * Cb + 128) div 255];
  mergeClearType.alpha := merge.alpha;

  if (mergeClearType.alpha = 255) then
    pdest^:= mergeClearType
  else
  begin
    if Cg <> 0 then
      DrawPixelInlineWithAlphaCheck(@merge, color, Cg);
    dont_keep := mergeClearType.alpha;
    if dont_keep > 0 then
    begin
      keep := not dont_keep;
      merge.red := GammaCompressionTab[(GammaExpansionTab[merge.red] * keep + GammaExpansionTab[mergeClearType.red] * dont_keep) div 255];
      merge.green := GammaCompressionTab[(GammaExpansionTab[merge.green] * keep + GammaExpansionTab[mergeClearType.green] * dont_keep) div 255];
      merge.blue := GammaCompressionTab[(GammaExpansionTab[merge.blue] * keep + GammaExpansionTab[mergeClearType.blue] * dont_keep) div 255];
      merge.alpha := mergeClearType.alpha + ApplyOpacity(merge.alpha, not mergeClearType.alpha);
    end;
    pdest^ := merge;
  end;
end;

procedure ScannerPutPixels(scan: IBGRAScanner; pdest: PBGRAPixel; count: integer; mode: TDrawMode);
var c : TBGRAPixel;
  i: Integer;
  scanNextFunc: function(): TBGRAPixel of object;
begin
  if scan.IsScanPutPixelsDefined then
    scan.ScanPutPixels(pdest,count,mode) else
  begin
    scanNextFunc := @scan.ScanNextPixel;
    case mode of
      dmLinearBlend:
        for i := 0 to count-1 do
        begin
          FastBlendPixelInline(pdest, scanNextFunc());
          inc(pdest);
        end;
      dmDrawWithTransparency:
        for i := 0 to count-1 do
        begin
          DrawPixelInlineWithAlphaCheck(pdest, scanNextFunc());
          inc(pdest);
        end;
      dmSet:
        for i := 0 to count-1 do
        begin
          pdest^ := scanNextFunc();
          inc(pdest);
        end;
      dmXor:
        for i := 0 to count-1 do
        begin
          PDWord(pdest)^ := PDWord(pdest)^ xor DWord(scanNextFunc());
          inc(pdest);
        end;
      dmSetExceptTransparent:
        for i := 0 to count-1 do
        begin
          c := scanNextFunc();
          if c.alpha = 255 then pdest^ := c;
          inc(pdest);
        end;
    end;
  end;
end;

procedure BlendPixels(pdest: PBGRAPixel; psrc: PBGRAPixel;
  blendOp: TBlendOperation; Count: integer);
begin
  case blendOp of
    boLinearBlend: while Count > 0 do
      begin
        FastBlendPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boTransparent: while Count > 0 do
      begin
        DrawPixelInlineWithAlphaCheck(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boMultiply: while Count > 0 do
      begin
        LinearMultiplyPixelInline(pdest, psrc^);  //same look with non linear
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boAdditive: while Count > 0 do
      begin
        AddPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boLinearAdd: while Count > 0 do
      begin
        LinearAddPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boColorBurn: while Count > 0 do
      begin
        ColorBurnPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boColorDodge: while Count > 0 do
      begin
        ColorDodgePixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boReflect: while Count > 0 do
      begin
        ReflectPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boGlow: while Count > 0 do
      begin
        GlowPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boNiceGlow: while Count > 0 do
      begin
        NiceGlowPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boOverlay: while Count > 0 do
      begin
        LinearOverlayPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boDarkOverlay: while Count > 0 do
      begin
        OverlayPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boDifference: while Count > 0 do
      begin
        DifferencePixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boLinearDifference: while Count > 0 do
      begin
        LinearDifferencePixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boNegation: while Count > 0 do
      begin
        NegationPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boLinearNegation: while Count > 0 do
      begin
        LinearNegationPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boLighten: while Count > 0 do
      begin
        LightenPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boDarken: while Count > 0 do
      begin
        DarkenPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boScreen: while Count > 0 do
      begin
        ScreenPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;

    boXor: while Count > 0 do
      begin
        XorPixelInline(pdest, psrc^);
        Inc(pdest);
        Inc(psrc);
        Dec(Count);
      end;
  end;
end;

procedure XorInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer);
begin
  while Count > 0 do
  begin
    PDWord(dest)^ := PDWord(dest)^ xor DWord(c);
    Inc(dest);
    Dec(Count);
  end;
end;

procedure XorPixels(pdest, psrc: PBGRAPixel; count: integer);
begin
  while Count > 0 do
  begin
    PDWord(pdest)^ := PDWord(psrc)^ xor PDWord(pdest)^;
    Inc(pdest);
    Inc(psrc);
    Dec(Count);
  end;
end;

procedure AlphaFillInline(dest: PBGRAPixel; alpha: byte; Count: integer); inline;
begin
  while Count > 0 do
  begin
    dest^.alpha := alpha;
    Inc(dest);
    Dec(Count);
  end;
end;

procedure FillInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer); inline;
begin
  FillDWord(dest^, Count, DWord(c));
end;

procedure FastBlendPixelsInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer);
var
  n: integer;
begin
  if c.alpha = 0 then exit;
  for n := Count - 1 downto 0 do
  begin
    FastBlendPixelInline(dest, c);
    Inc(dest);
  end;
end;

procedure DrawPixelsInline(dest: PBGRAPixel; c: TBGRAPixel; Count: integer);
var
  n: integer;
  ec: TExpandedPixel;
begin
  if c.alpha = 0 then exit;
  if c.alpha = 255 then
  begin
    filldword(dest^,count,longword(c));
    exit;
  end;
  ec := GammaExpansion(c);
  for n := Count - 1 downto 0 do
  begin
    DrawExpandedPixelInlineNoAlphaCheck(dest, ec,c.alpha);
    Inc(dest);
  end;
end;

procedure DrawExpandedPixelsInline(dest: PBGRAPixel; ec: TExpandedPixel;
  Count: integer);
var
  n: integer;
  c: TBGRAPixel;
begin
  if ec.alpha < $0100 then exit;
  if ec.alpha >= $FF00 then
  begin
    c := GammaCompression(ec);
    filldword(dest^,count,longword(c));
    exit;
  end;
  for n := Count - 1 downto 0 do
  begin
    DrawExpandedPixelInlineNoAlphaCheck(dest, ec, ec.alpha shr 8);
    Inc(dest);
  end;
end;

procedure DrawPixelsInlineExpandedOrNot(dest: PBGRAPixel; ec: TExpandedPixel; c: TBGRAPixel; Count: integer
  );
var
  n: integer;
begin
  if c.alpha = 0 then exit;
  if c.alpha = 255 then
  begin
    filldword(dest^,count,longword(c));
    exit;
  end;
  for n := Count - 1 downto 0 do
  begin
    DrawExpandedPixelInlineNoAlphaCheck(dest, ec, c.alpha);
    Inc(dest);
  end;
end;

procedure DrawPixelsInlineDiff(dest: PBGRAPixel; c: TBGRAPixel;
  Count: integer; compare: TBGRAPixel; maxDiff: byte); inline;
var
  n: integer;
begin
  for n := Count - 1 downto 0 do
  begin
    DrawPixelInlineDiff(dest, c, compare, maxDiff);
    Inc(dest);
  end;
end;

{$hints off}
procedure DrawPixelInlineWithAlphaCheck(dest: PBGRAPixel; const c: TBGRAPixel);
begin
  if c.alpha = 0 then
    exit;
  if c.alpha = 255 then
  begin
    dest^ := c;
    exit;
  end;
  DrawPixelInlineNoAlphaCheck(dest,c);
end;

procedure DrawPixelInlineWithAlphaCheck(dest: PBGRAPixel; c: TBGRAPixel; appliedOpacity: byte);
begin
  c.alpha := ApplyOpacity(c.alpha,appliedOpacity);
  if c.alpha = 0 then
    exit;
  if c.alpha = 255 then
  begin
    dest^ := c;
    exit;
  end;
  DrawPixelInlineNoAlphaCheck(dest,c);
end;

procedure CopyPixelsWithOpacity(dest, src: PBGRAPixel; opacity: byte;
  Count: integer);
var c: TBGRAPixel;
begin
  while count > 0 do
  begin
    c := src^;
    c.alpha := ApplyOpacity(c.alpha,opacity);
    dest^ := c;
    inc(src);
    inc(dest);
    dec(count);
  end;
end;

function ApplyOpacity(opacity1, opacity2: byte): byte;
begin
  result := opacity1*(opacity2+1) shr 8;
end;

function FastRoundDiv255(value: cardinal): cardinal; inline;
begin
  result := (value + (value shr 7)) shr 8;
end;

procedure DrawExpandedPixelInlineWithAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel);
var
  calpha: byte;
begin
  calpha := ec.alpha shr 8;
  if calpha = 0 then
    exit;
  if calpha = 255 then
  begin
    dest^ := GammaCompression(ec);
    exit;
  end;
  DrawExpandedPixelInlineNoAlphaCheck(dest,ec,calpha);
end;

procedure DrawPixelInlineExpandedOrNotWithAlphaCheck(dest: PBGRAPixel; const ec: TExpandedPixel; c: TBGRAPixel);
begin
  if c.alpha = 0 then
    exit;
  if c.alpha = 255 then
  begin
    dest^ := c;
    exit;
  end;
  DrawExpandedPixelInlineNoAlphaCheck(dest,ec,c.alpha);
end;

procedure DrawPixelInlineNoAlphaCheck(dest: PBGRAPixel; const c: TBGRAPixel);
var
  p: PByte;
  a1f, a2f, a12, a12m: cardinal;
begin
  a12  := 65025 - (not dest^.alpha) * (not c.alpha);
  a12m := a12 shr 1;

  a1f := dest^.alpha * (not c.alpha);
  a2f := (c.alpha shl 8) - c.alpha;

  p := PByte(dest);

  p^ := GammaCompressionTab[(GammaExpansionTab[dest^.blue] * a1f +
    GammaExpansionTab[c.blue] * a2f + a12m) div a12];
  Inc(p);
  p^ := GammaCompressionTab[(GammaExpansionTab[dest^.green] * a1f +
    GammaExpansionTab[c.green] * a2f + a12m) div a12];
  Inc(p);
  p^ := GammaCompressionTab[(GammaExpansionTab[dest^.red] * a1f +
    GammaExpansionTab[c.red] * a2f + a12m) div a12];
  Inc(p);

  p^ := (a12 + a12 shr 7) shr 8;
end;

procedure DrawExpandedPixelInlineNoAlphaCheck(dest: PBGRAPixel;
  const ec: TExpandedPixel; calpha: byte);
var
  p: PByte;
  a1f, a2f, a12, a12m: cardinal;
begin
  a12  := 65025 - (not dest^.alpha) * (not calpha);
  a12m := a12 shr 1;

  a1f := dest^.alpha * (not calpha);
  a2f := (calpha shl 8) - calpha;

  p := PByte(dest);

  p^ := GammaCompressionTab[(GammaExpansionTab[dest^.blue] * a1f +
    ec.blue * a2f + a12m) div a12];
  Inc(p);
  p^ := GammaCompressionTab[(GammaExpansionTab[dest^.green] * a1f +
    ec.green * a2f + a12m) div a12];
  Inc(p);
  p^ := GammaCompressionTab[(GammaExpansionTab[dest^.red] * a1f +
    ec.red * a2f + a12m) div a12];
  Inc(p);

  p^ := (a12 + a12 shr 7) shr 8;
end;

procedure FastBlendPixelInline(dest: PBGRAPixel; const c: TBGRAPixel);
var
  p: PByte;
  a1f, a2f, a12, a12m: cardinal;
begin
  if c.alpha = 0 then
    exit;
  if c.alpha = 255 then
  begin
    dest^ := c;
    exit;
  end;

  a12  := 65025 - (not dest^.alpha) * (not c.alpha);
  a12m := a12 shr 1;

  a1f := dest^.alpha * (not c.alpha);
  a2f := (c.alpha shl 8) - c.alpha;

  p := PByte(dest);

  p^ := (dest^.blue * a1f + c.blue * a2f + a12m) div a12;
  Inc(p);
  p^ := (dest^.green * a1f + c.green * a2f + a12m) div a12;
  Inc(p);
  p^ := (dest^.red * a1f + c.red * a2f + a12m) div a12;
  Inc(p);

  p^ := (a12 + a12 shr 7) shr 8;
end;

procedure FastBlendPixelInline(dest: PBGRAPixel; c: TBGRAPixel;
  appliedOpacity: byte);
begin
  c.alpha := ApplyOpacity(c.alpha,appliedOpacity);
  FastBlendPixelInline(dest,c);
end;

procedure DrawPixelInlineDiff(dest: PBGRAPixel; c, compare: TBGRAPixel;
  maxDiff: byte); inline;
begin
  DrawPixelInlineWithAlphaCheck(dest, BGRA(c.red, c.green, c.blue,
    (c.alpha * (maxDiff + 1 - BGRADiff(dest^, compare)) + (maxDiff + 1) shr 1) div
    (maxDiff + 1)));
end;

procedure ErasePixelInline(dest: PBGRAPixel; alpha: byte); inline;
var
  newAlpha: byte;
begin
  newAlpha := ApplyOpacity(dest^.alpha, not alpha);
  if newAlpha = 0 then
    dest^ := BGRAPixelTransparent
  else
    dest^.alpha := newAlpha;
end;

{$hints on}

{--------------------------------------- Layer blending -----------------------------------------}

function ByteLinearMultiplyInline(a, b: byte): byte;
begin
  Result := (a * b) shr 8;
end;

procedure LinearMultiplyPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteLinearMultiplyInline(dest^.red, c.red) *
    destalpha + c.red * (not destalpha)) shr 8;
  dest^.green := (ByteLinearMultiplyInline(dest^.green, c.green) *
    destalpha + c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteLinearMultiplyInline(dest^.blue, c.blue) *
    destalpha + c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

{$hints off}
function ByteAddInline(a, b: byte): byte;
var
  temp: longword;
begin
  temp := longword(GammaExpansionTab[a]) + longword(GammaExpansionTab[b]);
  if temp > 65535 then
    temp := 65535;
  Result := GammaCompressionTab[temp];
end;

{$hints on}

function ByteLinearAddInline(a, b: byte): byte;
var
  temp: integer;
begin
  temp := integer(a) + integer(b);
  if temp > 255 then
    temp := 255;
  Result := temp;
end;

procedure AddPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := ByteAddInline(dest^.red * destalpha shr 8, c.red);
  dest^.green := ByteAddInline(dest^.green * destalpha shr 8, c.green);
  dest^.blue  := ByteAddInline(dest^.blue * destalpha shr 8, c.blue);
  dest^.alpha := c.alpha;
end;

procedure LinearAddPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := ByteLinearAddInline(dest^.red * destalpha shr 8, c.red);
  dest^.green := ByteLinearAddInline(dest^.green * destalpha shr 8, c.green);
  dest^.blue  := ByteLinearAddInline(dest^.blue * destalpha shr 8, c.blue);
  dest^.alpha := c.alpha;
end;

function ByteBurnInline(a, b: byte): byte; inline;
var
  temp: integer;
begin
  if b = 0 then
    Result := 0
  else
  begin
    temp := 255 - (((255 - a) shl 8) div b);
    if temp < 0 then
      Result := 0
    else
      Result := temp;
  end;
end;

procedure ColorBurnPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteBurnInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteBurnInline(dest^.green, c.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteBurnInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

{$hints off}
function ByteDodgeInline(a, b: byte): byte; inline;
var
  temp: integer;
begin
  if b = 255 then
    Result := 255
  else
  begin
    temp := (a shl 8) div (not b);
    if temp > 255 then
      Result := 255
    else
      Result := temp;
  end;
end;
{$hints on}

procedure ColorDodgePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteDodgeInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteDodgeInline(dest^.green, c.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteDodgeInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

{$hints off}
function ByteNonLinearReflectInline(a, b: byte): byte; inline;
var
  temp: longword;
  wa,wb: word;
begin
  if b = 255 then
    Result := 255
  else
  begin
    wa := GammaExpansionTab[a];
    wb := GammaExpansionTab[b];
    temp := wa * wa div (not wb);
    if temp >= 65535 then
      Result := 255
    else
      Result := GammaCompressionTab[ temp ];
  end;
end;

function ByteReflectInline(a, b: byte): byte; inline;
var
  temp: integer;
begin
  if b = 255 then
    Result := 255
  else
  begin
    temp := a * a div (not b);
    if temp > 255 then
      Result := 255
    else
      Result := temp;
  end;
end;
{$hints on}


procedure ReflectPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteReflectInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteReflectInline(dest^.green, c.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteReflectInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

procedure GlowPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteReflectInline(c.red, dest^.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteReflectInline(c.green, dest^.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteReflectInline(c.blue, dest^.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

procedure NiceGlowPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteReflectInline(c.red, dest^.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteReflectInline(c.green, dest^.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteReflectInline(c.blue, dest^.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;

  if (c.red > c.green) and (c.red > c.blue) then
    dest^.alpha := c.red else
  if (c.green > c.blue) then
    dest^.alpha := c.green else
    dest^.alpha := c.blue;
  dest^.alpha := ApplyOpacity(GammaExpansionTab[dest^.alpha] shr 8,c.alpha);
end;

procedure NonLinearReflectPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteNonLinearReflectInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteNonLinearReflectInline(dest^.green, c.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteNonLinearReflectInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

{$hints off}
function ByteOverlayInline(a, b: byte): byte; inline;
var wa,wb: word;
begin
  wa := GammaExpansionTab[a];
  wb := GammaExpansionTab[b];
  if wa < 32768 then
    Result := GammaCompressionTab[ (wa * wb) shr 15 ]
  else
    Result := GammaCompressionTab[ 65535 - ((not wa) * (not wb) shr 15) ];
end;

function ByteLinearOverlayInline(a, b: byte): byte; inline;
begin
  if a < 128 then
    Result := (a * b) shr 7
  else
    Result := 255 - ((not a) * (not b) shr 7);
end;

{$hints on}

procedure OverlayPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteOverlayInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteOverlayInline(dest^.green, c.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteOverlayInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

procedure LinearOverlayPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteLinearOverlayInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteLinearOverlayInline(dest^.green, c.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteLinearOverlayInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

function ByteDifferenceInline(a, b: byte): byte; inline;
begin
  Result := GammaCompressionTab[abs(integer(GammaExpansionTab[a]) -
    integer(GammaExpansionTab[b]))];
end;

function ByteLinearDifferenceInline(a, b: byte): byte; inline;
begin
  Result := abs(a - b);
end;

procedure DifferencePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteDifferenceInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteDifferenceInline(dest^.green, c.green) *
    destalpha + c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteDifferenceInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

procedure LinearDifferencePixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteLinearDifferenceInline(dest^.red, c.red) *
    destalpha + c.red * (not destalpha)) shr 8;
  dest^.green := (ByteLinearDifferenceInline(dest^.green, c.green) *
    destalpha + c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteLinearDifferenceInline(dest^.blue, c.blue) *
    destalpha + c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

function ByteNegationInline(a, b: byte): byte; inline;
var
  sum: integer;
begin
  sum := integer(GammaExpansionTab[a]) + integer(GammaExpansionTab[b]);
  if sum > 65535 then
    sum  := 131071 - sum;
  Result := GammaCompressionTab[sum];
end;

function ByteLinearNegationInline(a, b: byte): byte; inline;
var
  sum: integer;
begin
  sum := integer(a) + integer(b);
  if sum > 255 then
    Result := 511 - sum
  else
    Result := sum;
end;

procedure NegationPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteNegationInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteNegationInline(dest^.green, c.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteNegationInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

procedure LinearNegationPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteLinearNegationInline(dest^.red, c.red) *
    destalpha + c.red * (not destalpha)) shr 8;
  dest^.green := (ByteLinearNegationInline(dest^.green, c.green) *
    destalpha + c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteLinearNegationInline(dest^.blue, c.blue) *
    destalpha + c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

function ByteLightenInline(a, b: byte): byte; inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

procedure LightenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteLightenInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteLightenInline(dest^.green, c.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteLightenInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

function ByteDarkenInline(a, b: byte): byte; inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

procedure DarkenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ByteDarkenInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ByteDarkenInline(dest^.green, c.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ByteDarkenInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

{$hints off}
function ScreenByteInline(a, b: byte): byte;
begin
  Result := 255 - ((255 - a) * (255 - b) shr 8);
end;

{$hints on}

procedure ScreenPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := (ScreenByteInline(dest^.red, c.red) * destalpha +
    c.red * (not destalpha)) shr 8;
  dest^.green := (ScreenByteInline(dest^.green, c.green) * destalpha +
    c.green * (not destalpha)) shr 8;
  dest^.blue  := (ScreenByteInline(dest^.blue, c.blue) * destalpha +
    c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

procedure XorPixelInline(dest: PBGRAPixel; c: TBGRAPixel); inline;
var
  destalpha: byte;
begin
  destalpha   := dest^.alpha;
  dest^.red   := ((dest^.red xor c.red) * destalpha + c.red * (not destalpha)) shr 8;
  dest^.green := ((dest^.green xor c.green) * destalpha + c.green *
    (not destalpha)) shr 8;
  dest^.blue  := ((dest^.blue xor c.blue) * destalpha + c.blue * (not destalpha)) shr 8;
  dest^.alpha := c.alpha;
end;

end.

