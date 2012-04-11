unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Buttons, ExtCtrls,
  FileCtrl, StdCtrls, ComCtrls, Menus, process, BGRABitmap, BGRABitmapTypes,
  types,LCLIntf, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    FileListBox1: TFileListBox;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    PopupMenu1: TPopupMenu;
    SpeedButton1: TSpeedButton;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
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
    procedure MenuItem1Click(Sender: TObject);
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
    procedure TrayIcon1Click(Sender: TObject);
    procedure TrayIcon1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
    function get_theme_dirs(thelist:TStringList;thetheme:string):boolean;
    function convert_svg(svgfile,theicon:string):string;
    function get_icon_file(dirlist:TStringList;theicon_:string):string;
    function get_value(thefile,key,expect:string): string;
    function rsearch(folder,filename:string): string;
    function draw_scroll(vert:boolean;size,current:int64):boolean;
  end; 

var
  Form1: TForm1;
  home:string;
  bg,sc:TBGRABitmap;
  backcolor:TBGRAPixel;
  thewidth,nicons,inity,formery,scrollMax,lapse,downY,offset:int64;
  execs:array[0..511] of string;
  originalpos:array[0..511] of TPoint;
  images:array[0..511] of TBGRABitmap;
  down,downin:boolean;
implementation

{$R *.lfm}

{ TForm1 }

function TForm1.draw_scroll(vert:boolean;size,current:int64):boolean;
var
  posit:int64;
begin
  posit := 16+round((current*1) / ((size*1)/(bg.Height-96)));
  sc.Rectangle(sc.Width-20,0,sc.Width+1,sc.Height,BGRA(20,20,20,255),BGRA(20,20,20,255),dmSetExceptTransparent);
  sc.RoundRect(sc.Width-15,18,sc.Width-4,sc.Height-18,12,12,BGRA(180,180,180,255),BGRA(150,150,150,255));
  sc.RoundRect(sc.Width-14,posit+4,sc.Width-5,posit+60,8,8,BGRA(30,30,30,255),BGRA(60,60,60,255));
end;

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
  if inity > scrollMax then inity := scrollMax;
  if inity < 0 then inity:=0;
  paint;
  formery:=inity;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
end;

procedure TForm1.TrayIcon1Click(Sender: TObject);
begin
  if Visible = true then
  begin
    hide;
    Timer1.Enabled:=false;
  end else
  begin
    show;
    Timer1.Enabled:=true;
  end;
end;

procedure TForm1.TrayIcon1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
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
  i,x,y:int64;
  dir,theicon,thetheme,themedir,usrshare,thename,alt_theme,desktopfile:string;
  dest,src:TRect;
  tempimg:TBGRABitmap;
  dirlist,config,iconlist:TStringList;
  labelcolor:TBGRAPixel;
begin
  config:=TStringList.Create;
  iconlist:=TStringList.Create;
  if FileExists(ProgramDirectory+'/config') then config.LoadFromFile(ProgramDirectory+'/config');
  //ScrollBox1.Color:=RGBToColor(30,30,30);
  Panel1.Color:=RGBToColor(20,20,20);
  Panel2.Color:=RGBToColor(20,20,20);
  //Panel3.Color:=RGBToColor(60,60,60);
  //Panel4.Color:=RGBToColor(60,60,60);
  labelcolor:=BGRAWhite;
  thewidth:=Screen.Width;
  offset:=0;//round((Screen.Width-thewidth) / 2);
  down:=false;
  Color:=RGBToColor(0,0,0);
  backcolor:=BGRA(20,20,20,255);
  bg:=TBGRABitmap.Create(Screen.Width-19,Screen.Height,backcolor);
  sc:=TBGRABitmap.Create(20,Screen.Height,backcolor);

  if config.Strings[0]='fullscreen=false' then
  begin
    SpeedButton1.Enabled:=false;
    SpeedButton1.Hide;
    WindowState:=wsNormal;
    Width:=(112*6)+32+24;
    Height:=(140*4+16);
    thewidth:=Width;
    bg.free;
    bg:=TBGRABitmap.Create(width-20,Height,backcolor);
    sc.free;
    sc:=TBGRABitmap.Create(20,Height,backcolor);
    //Panel3.Hide;
    //Panel4.Hide;
    Color:=clWindow;
    if config.Strings[1]='border=true' then BorderStyle:=bsToolWindow;
    if config.Strings[2]='use_gtk2=true' then
    begin
      backcolor:=BGRA(255-Red(clWindow),255-Green(clWindow),255-Blue(clWindow),255);//BGRA(240,240,240,255);
      labelcolor:=BGRA(255-Red(cltext),255-Green(cltext),255-Blue(cltext),255);//BGRABlack;
      Panel1.Color:=clMenuBar;
      Panel2.Color:=clMenuBar;
    end;
    offset:=0;
  end else WindowState:=wsMaximized;

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

  for i:=0 to FileListBox1.Items.Count-1 do
  begin
    iconlist.Add(get_value('/usr/share/applications/'+FileListBox1.Items.Strings[i],'Name','')+
      ' --> '+FileListBox1.Items.Strings[i]);
  end;
  iconlist.Sort;

  for i:=0 to iconlist.Count-1 do
  begin
  desktopfile:='/usr/share/applications/'+copy(iconlist.Strings[i],pos(' --> ',iconlist.Strings[i])+5,length(iconlist.Strings[i]));
  thename:=copy(iconlist.Strings[i],1,pos(' --> ',iconlist.Strings[i])-1);
  if get_value(desktopfile,'NoDisplay','')<>'true' then
  begin

    if x>((thewidth-24) div 112)-1 then
    begin
      y:=y+1;
      x:=0;
    end;

    theicon:=get_value(desktopfile,'Icon','');
    execs[nicons]:=get_value(desktopfile,'Exec','');

    images[nicons] := TBGRABitmap.Create(80,140,backcolor);
    tempimg.LoadFromFile(get_icon_file(dirlist,theicon));
    images[nicons].PutImage(0,0,tempimg,dmDrawWithTransparency,255);
    images[nicons].FontHeight:=12;
    images[nicons].FontQuality:=fqSystem;
    images[nicons].TextRect(Rect(0,82,80,140),thename,taCenter,tlTop,labelcolor);

    originalpos[nicons].Y:=y*140;
    originalpos[nicons].X:=(x*112)+24;

    x:=x+1;
    nicons:=nicons+1;
  end else originalpos[i].X := -1;
  end;
  scrollMax:=round(( (y*140)-(Height-180) ));
  tempimg.Free;
  dirlist.Free;
  iconlist.Free;
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin

end;

procedure TForm1.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
begin

end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  down:=True;
  if x > Width-20 then downin:=true;
  Timer1.Enabled:=true;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if down=false then exit;
  if downin=false then exit;
  inity:=round(Y * (scrollMax / (sc.Height-96)) )-140;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i:int64;
  app:TProcess;
begin
  Timer1.Enabled:=false;
  down:=false;
  if downin = true then
  begin
    downin:=false;
    exit;
  end;
  for i:=0 to FileListBox1.Count-1 do
  begin
    if (x > originalpos[i].X) and (x < originalpos[i].x+80)
      and (y+(inity*1) > originalpos[i].Y) and (y+(inity*1) < originalpos[i].Y+80) then
    begin
      app:=TProcess.Create(self);
      app.CommandLine:=execs[i];
      WriteLn('executed '+app.CommandLine);
      app.Execute;
      app.free;

      Timer1.Enabled:=false;
      hide;

      Break;
    end;
  end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inity:=inity-(WheelDelta);
  if inity > scrollMax then inity := scrollMax;
  if inity < 0 then inity:=0;
  paint;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  i:int64;
begin
  bg.Fill(backcolor);
  for i:=((inity) div 140) to nicons do
  begin
    if originalpos[i].Y-(inity)<(form1.Height) then
    begin
      bg.PutImage(originalpos[i].X+12,originalpos[i].Y-(inity)+16,images[i],dmSetExceptTransparent,255);
    end;
  end;
  draw_scroll(true,scrollMax,inity);
  bg.Draw(Canvas,0,0,true);
  sc.Draw(Canvas,Form1.Width-20,0,true);
end;

procedure TForm1.FormWindowStateChange(Sender: TObject);
begin
  {if WindowState=wsMinimized then
  begin
    Timer1.Enabled:=false;
    hide;
  end else Timer1.Enabled:=true;}
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

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  close;
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

