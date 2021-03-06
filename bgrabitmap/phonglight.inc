var
  dist2,LdotN,NdotH,lightEnergy,diffuse : single;
const
  minus_05 = -0.5;
begin
  {$IFDEF CPUI386}If UseSSE then
  begin
    with Context^ do
    begin
      {$IFDEF PARAM_POINTLIGHT}
      vL := FVertex.ViewCoord_128;
      vH := FViewVector;
      {$ELSE}
      vL := FDirection;
      vH := FBetweenDirectionAndObserver;
      {$ENDIF}
    end;
    if UseSSE3 then
    begin
      {$DEFINE PARAM_USESSE3}
      {$i phonglightsse.inc}
      {$UNDEF PARAM_USESSE3}
    end else
    begin
      {$i phonglightsse.inc}
    end;
  end else
  {$ENDIF}
  with Context^ do
  begin
    {$IFDEF PARAM_POINTLIGHT}
    vL := FVertex.ViewCoord_128 - basic.Position;
    Normalize3D_128(vL, dist2);
    //compute bisector of angle between light and observer
    vH := vL + FViewVector;
    Normalize3D_128(vH);
    //Calculate LdotN and NnH
    LdotN := basic.Normal * vL;
    NdotH := basic.Normal * vH;
    {$ELSE}
    LdotN := basic.Normal * FDirection;
    NdotH := basic.Normal * FBetweenDirectionAndObserver;
    {$ENDIF}
  end;

  if LdotN < minus_05 then NdotH := 0 else
  if LdotN < 0 then
  begin
    NdotH := NdotH*(LdotN-minus_05);
    NdotH += NdotH;
  end;

  {$IFDEF PARAM_POINTLIGHT}
  if dist2 = 0 then
    lightEnergy := 0
  else
    lightEnergy := FIntensity / dist2;
  diffuse := LdotN*lightEnergy;
  {$ELSE}
  lightEnergy := 1;
  diffuse := LdotN;
  {$ENDIF}

  if Context^.LightThrough and (diffuse < 0) then diffuse := -diffuse*Context^.LightThroughFactor;
  TBGRAMaterial3D(Context^.material).ComputeDiffuseAndSpecularColor(Context, diffuse, lightEnergy, NdotH, FColorInt);
end;

{$UNDEF PARAM_POINTLIGHT}
