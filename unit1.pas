unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Buttons,
  ExtCtrls, FileCtrl, StdCtrls, ComCtrls, process, BGRABitmap, BGRABitmapTypes, types;

type

  { TForm1 }

  TForm1 = class(TForm)
    FileListBox1: TFileListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ScrollBar1: TScrollBar;
    SpeedButton1: TSpeedButton;
    Timer1: TTimer;
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormPaint(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image1Paint(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Panel2MouseEnter(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure ScrollBar1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ScrollBar1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ScrollBar1Enter(Sender: TObject);
    procedure ScrollBar1Exit(Sender: TObject);
    procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ScrollBar1StartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ScrollBox1Click(Sender: TObject);
    procedure ScrollBox1Paint(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function get_theme_dirs(thelist:TStringList;thetheme:string):boolean;
    function convert_svg(svgfile,theicon:string):string;
    function get_icon_file(dirlist:TStringList;theicon_:string):string;
    function get_value(thefile,key,expect:string): string;
    function rsearch(folder,filename:string): string;
  end; 

var
  Form1: TForm1;
  home:string;
  bg:TBGRABitmap;
  backcolor:TBGRAPixel;
  thewidth,nicons,inity:int64;
  execs:array[0..511] of string;
  originalpos:array[0..511] of TPoint;
  images:array[0..511] of TBGRABitmap;
  down:boolean;
implementation

{$R *.lfm}

{ TForm1 }
function TForm1.get_theme_dirs(thelist:TStringList;thetheme:string):boolean;
var
  temp,usrshare,alt_theme:string;
begin
  usrshare:='/usr/share';
  thelist.Clear;
  if FileExists(home+'/.icons/'+thetheme+'/index.theme') then
  begin
    thelist.Add(home+'/.icons/'+thetheme);
  end
  else if FileExists(usrshare+'/icons/'+thetheme+'/index.theme') then
  begin
    thelist.Add(usrshare+'/icons/'+thetheme);
  end;

  WriteLn('main themedir: '+thelist.Strings[0]);

  temp:=thelist.Strings[0];
  while temp <> 'finish' do
  begin
    alt_theme := get_value(temp+'/index.theme','Inherits','');
    if pos(',',alt_theme) > 0 then alt_theme:=copy(alt_theme,1,pos(',',alt_theme)-1);
    //WriteLn(temp+' Inherits: '+alt_theme);

    if FileExists(home+'/.icons/'+alt_theme+'/index.theme') then
    begin
      temp:=home+'/.icons/'+alt_theme;
      thelist.Add(temp);
    end
    else if FileExists(usrshare+'/icons/'+alt_theme+'/index.theme') then
    begin
      temp:=usrshare+'/icons/'+alt_theme;
      thelist.Add(temp);
    end
    else temp:='finish' ;
    WriteLn('found themedir: '+temp);
  end;
  //WriteLn('alt_theme: '+alt_theme);
  Result:=true;
end;

function TForm1.convert_svg(svgfile,theicon:string):string;
var
  app:TProcess;
begin                                                                                             // -unsharp 0x1
  app := TProcess.Create(self);
  app.CommandLine:='convert -background none -depth 8 "'+svgfile+'" -resize 80x80 -filter cubic -unsharp 0x1 '+home+'/.apps/'+theicon+'.png';
  app.Options:=[poWaitOnExit];
  if FileExists(home+'/.apps/'+theicon+'.png')=false then app.Execute;
  Result:=home+'/.apps/'+theicon+'.png';
  //WriteLn('converted or found '+svgfile);
  app.Free;
end;

function TForm1.get_icon_file(dirlist:TStringList;theicon_:string):string;
var
  i:int64;
  aicon,theicon:string;
begin
  theicon := ExtractFileNameOnly(theicon_);
  theicon := ExtractFileNameWithoutExt(theicon);
  //debug WriteLn('the icon should be '+theicon);

  if FileExists(home+'/.apps/'+theicon+'.png') then Result:=home+'/.apps/'+theicon+'.png' else
  if FileExists(theicon_) then Result:=convert_svg(theicon_,theicon) else
  begin
    aicon:='';
    for i:=0 to dirlist.Count-1 do
    begin
      if aicon='' then aicon:=rsearch(dirlist.Strings[i],theicon+'.png');
    end;
    if aicon='' then
      for i:=0 to dirlist.Count-1 do
      begin
        if aicon='' then aicon:=rsearch(dirlist.Strings[i],theicon+'.svg');
      end;

    if aicon='' then
    begin
      aicon:=rsearch('/usr/share/icons/hicolor/',theicon+'.svg');
      //debug WriteLn('searching for '+theicon+'.svg in /usr/share/icons/hicolor/');
    end;
    if aicon='' then
    begin
      aicon:=rsearch('/usr/share/icons/hicolor/',theicon+'.png');
      //debug WriteLn('searching for '+theicon+'.svg in /usr/share/icons/hicolor/');
    end;
    //WriteLn('search returned '+aicon);

    if Pos('.',aicon)>0 then Result:=convert_svg(aicon,theicon) else
    if FileExists('/usr/share/pixmaps/'+theicon+'.png') then Result:=convert_svg('/usr/share/pixmaps/'+theicon+'.png',theicon) else
    if FileExists('/usr/share/pixmaps/'+theicon+'.xpm') then Result:=convert_svg('/usr/share/pixmaps/'+theicon+'.xpm',theicon) else
    Result:=convert_svg('/usr/share/icons/hicolor/scalable/apps/Terminal.svg',theicon);
  end;

end;

function TForm1.get_value(thefile,key,expect:string): string;
var
  i:int64;
  thetext:TStringList;
  thevalue:string;
begin
  thetext:=TStringList.Create;
  thetext.LoadFromFile(thefile);

  Result:=key+'-not-found';

  //debug ShowMessage(thetext.text);
  for i:=0 to thetext.Count-1 do
  begin
    if Copy(thetext.Strings[i],1,Pos('=',thetext.Strings[i])-1) = key then
    begin
      //debug ShowMessage('found '+key);
      thevalue:=Copy(thetext.Strings[i],Length(key)+1,Length(thetext.Strings[i])); //Length(thetext.Strings[i])
      thevalue:=StringReplace(thevalue,'"','',[rfReplaceAll]);
      thevalue:=StringReplace(thevalue,'=','',[rfReplaceAll]);
      thevalue:=StringReplace(thevalue,'%F','',[rfReplaceAll]);
      thevalue:=StringReplace(thevalue,'%u','',[rfReplaceAll]);
      thevalue:=StringReplace(thevalue,'%U','',[rfReplaceAll]);
      thevalue:=StringReplace(thevalue,'%f','',[rfReplaceAll]);
      if expect='' then Result:=thevalue;
      if expect<>'' then
      begin
        if thevalue=expect then Result:=IntToStr(i)
        else thevalue:=expect+'-does-not-match '+key;
      end;
    end;
  end;

  thetext.Free;
end;

function TForm1.rsearch(folder,filename:string): string;
var
  i,theone:int64;
  max:int64;
  folders:TStringList;
begin
  folders:=TStringList.Create;
  Result:='';
  folders.Add('');

  folders.AddStrings(FindAllFiles(folder,filename));
  if folders.Count > 1 then
  begin
    max := FileSize(folders.Strings[1]);
    theone:=1;
    for i:=1 to folders.Count-1 do
    begin
      if FileSize(folders.Strings[i]) > max then
      begin
        max := FileSize(folders.Strings[i]);
        theone:=i;
      end;
    end;
    Result:=folders.Strings[theone];
    //WriteLn('theone is '+result);
  end;

  folders.Free;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  paint;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
end;

procedure TForm1.FormClick(Sender: TObject);
begin
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FileExists(ProgramDirectory+'/isrunning') then DeleteFile(ProgramDirectory+'/isrunning');
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i,x,y,offset:int64;
  dir,theicon,thetheme,themedir,usrshare,thename,alt_theme:string;
  dest,src:TRect;
  //newimage:TBGRASpeedButton;
  tempimg:TBGRABitmap;
  newlabel:TLabel;
  dirlist:TStringList;
  labelcolor:TBGRAPixel;
begin
  //ScrollBox1.Color:=RGBToColor(30,30,30);
  Panel1.Color:=RGBToColor(40,40,40);
  Panel2.Color:=RGBToColor(40,40,40);
  Panel3.Color:=RGBToColor(40,40,40);
  //Panel4.Color:=RGBToColor(30,30,30);
  labelcolor:=BGRAWhite;
  thewidth:=Screen.Width;
  offset:=96;

  backcolor:=BGRA(40,40,40,255);

  if FileExists(ProgramDirectory+'/widget') = true then
  begin
    SpeedButton1.Enabled:=false;
    SpeedButton1.Hide;
    WindowState:=wsNormal;
    Width:=(112*6)+32;
    Height:=(140*4+16);
    thewidth:=1000;
    Panel3.Hide;
    BorderStyle:=bsSingle;
    Panel1.Color:=clMenuBar;
    Panel2.Color:=clMenuBar;
    offset:=0;
    backcolor:=BGRA(240,240,240,255);
    labelcolor:=BGRABlack;
    BorderStyle:=bsSingle;
  end else WindowState:=wsMaximized;

  bg:=TBGRABitmap.Create(width,Height,backcolor);
  tempimg:=TBGRABitmap.Create(80,80);


  x:=0;
  y:=0;
  nicons:=0;

  home:=GetEnvironmentVariable('HOME');
  usrshare:='/usr/share';

  if DirectoryExists(home+'/.apps')=false then CreateDir(home+'/.apps');
  if DirectoryExists(home+'/.applications') then FileListBox1.Directory:=home+'/.applications'
    else FileListBox1.Directory:='/usr/share/applications';
  thetheme:=get_value(home+'/.gtkrc-2.0','gtk-icon-theme-name','');

  //WriteLn('the theme is '+thetheme);

  dirlist:=TStringList.Create;
  get_theme_dirs(dirlist,thetheme);

  //WriteLn('scrollbox width: '+IntToStr(ScrollBox1.Width));
  for i:=0 to FileListBox1.Items.Count-1 do
  begin
  if get_value('/usr/share/applications/'+FileListBox1.Items.Strings[i],'NoDisplay','')<>'true' then
  begin

    if x>((thewidth-(80+80)) div 112)-1 then
    begin
      y:=y+1;
      x:=0;
    end;

    theicon:=get_value('/usr/share/applications/'+FileListBox1.Items.Strings[i],'Icon','');
    thename:=get_value('/usr/share/applications/'+FileListBox1.Items.Strings[i],'Name','');
    execs[nicons]:=get_value('/usr/share/applications/'+FileListBox1.Items.Strings[i],'Exec','');

    images[nicons] := TBGRABitmap.Create(80,140,backcolor);
    tempimg.LoadFromFile(get_icon_file(dirlist,theicon));
    images[nicons].PutImage(0,0,tempimg,dmDrawWithTransparency,255);
    images[nicons].FontHeight:=12;
    images[nicons].FontQuality:=fqSystem;
    images[nicons].TextRect(Rect(0,82,80,140),thename,taCenter,tlTop,labelcolor);

    originalpos[nicons].Y:=y*140;
    originalpos[nicons].X:=(x*112)+24+offset;

    x:=x+1;
    nicons:=nicons+1;
  end else originalpos[i].X := -1;
  end;
  ScrollBar1.Max:=(y*140)-(Height-140);
  tempimg.Free;
  dirlist.Free;
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin

end;

procedure TForm1.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
begin

end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i:int64;
  app:TProcess;
begin
  {down:=true;
  inity:=Y;}
  for i:=0 to FileListBox1.Count-1 do
  begin
    if (x > originalpos[i].X) and (x < originalpos[i].x+80)
      and (y+ScrollBar1.Position > originalpos[i].Y) and (y+ScrollBar1.Position < originalpos[i].Y+80) then
    begin
      app:=TProcess.Create(self);
      app.CommandLine:=execs[i];
      WriteLn('executed '+app.CommandLine);
      app.Execute;
      app.free;

      if FileExists(ProgramDirectory+'/widget') = false then
      begin
        Close;
      end
      else
      begin
        WindowState:=wsMinimized;
        Timer1.Enabled:=false;
      end;
      Break;
    end;
  end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
//var
  //newpos:integer;
begin
  {if down = true then
  begin
    newpos:=ScrollBar1.Position+(inity-y);
    ScrollBar1.Position:=newpos;
    ScrollBar1Scroll(self,scTrack,newpos);
  end;}
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  {down:=false;}
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBar1.Position:=ScrollBar1.Position-(WheelDelta div 2);
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  i:int64;
begin
  bg.Fill(backcolor);
  for i:=(ScrollBar1.Position div 160) to nicons do
  begin
    if originalpos[i].Y-ScrollBar1.Position<(form1.Height) then
    begin
      bg.PutImage(originalpos[i].X+12,originalpos[i].Y-ScrollBar1.Position+16,images[i],dmSetExceptTransparent,255);
    end;
  end;
  bg.Draw(Canvas,0,0,true);
end;

procedure TForm1.FormWindowStateChange(Sender: TObject);
begin
  if WindowState=wsMinimized then Timer1.Enabled:=false else Timer1.Enabled:=true;
end;

procedure TForm1.Image1Click(Sender: TObject);
//var
  //app:TProcess;
begin
  {app:=TProcess.Create(self);
  //app.CommandLine:=TSpeedButton(sender).Hint;
  app.CommandLine:=TBGRASpeedButton(sender).Hint;
  //WriteLn('executed '+TBGRASpeedButton(sender).Hint);
  app.Execute;
  app.free;

  if FileExists(ProgramDirectory+'/widget') = false then
  begin
    Close;
  end
  else
  begin
    WindowState:=wsMinimized;
  end;}
end;

procedure TForm1.Image1Paint(Sender: TObject);
begin

end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
end;

procedure TForm1.Panel2MouseEnter(Sender: TObject);
begin

end;

procedure TForm1.ScrollBar1Change(Sender: TObject);
begin

end;

procedure TForm1.ScrollBar1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin

end;

procedure TForm1.ScrollBar1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin

end;

procedure TForm1.ScrollBar1Enter(Sender: TObject);
begin

end;

procedure TForm1.ScrollBar1Exit(Sender: TObject);
begin

end;

procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if ScrollPos>ScrollBar1.Max then ScrollPos:=ScrollBar1.Max;
  if ScrollPos<ScrollBar1.Min then ScrollPos:=ScrollBar1.Min;
end;

procedure TForm1.ScrollBar1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin

end;

procedure TForm1.ScrollBox1Click(Sender: TObject);
begin

end;

procedure TForm1.ScrollBox1Paint(Sender: TObject);
begin

end;

end.

