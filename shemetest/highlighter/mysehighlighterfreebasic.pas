{ <PiNote - free source code editor>

Copyright (C) <2021> <Enzo Antonio Calogiuri> <ecalogiuri(at)gmail.com>

This source is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option)
any later version.

This code is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

A copy of the GNU General Public License is available on the World Wide Web
at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
Boston, MA 02110-1335, USA.
}
unit MySEHighlighterFreeBasic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

    { TMySEHighlighterFreeBasic }

    TMySEHighlighterFreeBasic             = Class(TSynFacilSyn)
     Private
      fKeyWordList                       : TStringList;

     Protected
      function IsFilterStored: Boolean; override;
      function GetSampleSource: string; override;

     Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;

      class function GetLanguageName: string; override;
    end;

implementation

Uses SynFacilBasic;

Const
     SYNS_FilterFB        = 'FreeBASIC source Files (*.bas)|*.bas';
     SYNS_LangFB          = 'FreeBASIC';

     FBKeyWords           = 'abs,abstract,access,acos,add,alias,allocate,alpha,' +
                            'and,andalso,any,append,as,asc,asin,asm,assert,' +
                            'assertwarn,atan2,atn,base,beep,bin,binary,bit,' +
                            'bitresetbitset,bload,boolean,bsave,byref,byte,byval,' +
                            'call,callocate,case,cast,cbool,cbyte,cdbl,cdecl,' +
                            'chain,chdir,chr,cint,circle,class,clear,clng,clngint,' +
                            'close,cls,color,command,common,condbroadcast,' +
                            'condcreate,conddestroy,condsignal,condwait,const,' +
                            'constructor,continue,cos,cptr,cshort,csign,csng,' +
                            'csrlin,cubyte,cuint,culng,culngint,cunsg,cushort,' +
                            'custom,cva_arg,cva_copy,cva_end,cva_list,cva_start,' +
                            'cvd,cvi,cvl,cvlongint,cvs,cvshort,data,date,deallocate,' +
                            'declare,defbyte,defdbl,defined,defint,deflng,deflongint,' +
                            'defshort,defsng,defstr,defubyte,defuint,defulongint,' +
                            'defushort,delete,destructor,dim,dir,do,double,draw,' +
                            'dylibfree,dylibload,dylibsymbol,else,elseif,encoding,' +
                            'end,end if,enum,environ,eof,eqv,erase,erfn,erl,ermn,' +
                            'err,error,event,exec,exepath,exit,exp,export,extends,' +
                            'extern,false,field,fix,flip,for,format,frac,fre,freefile,' +
                            'function,get,getjoystick,getkey,getmouse,gosub,goto,' +
                            'hex,hibyte,hiword,if,iif,imageconvertrow,imagecreate,' +
                            'imagedestroy,imageinfo,imp,implements,import,inkey,' +
                            'inp,input,instr,instrrev,int,integer,is,kill,lbound,' +
                            'lcase,left,len,lib,line,lobyte,loc,local,locate,lock,' +
                            'lof,log,long,longint,loop,loword,lpos,lprint,lset,' +
                            'ltrim,mid,mkd,mkdir,mki,mkl,mklongint,mks,mkshort,' +
                            'mod,multikey,mutexcreate,mutexdestroy,mutexlock,' +
                            'mutexunlock,naked,name,namespace,new,next,not,' +
                            'object,oct,offsetof,on,once,open,operator,option,' +
                            'nogosub,nokeyword,or,orelse,out,output,overload,' +
                            'override,paint,palette,pascal,pcopy,peek,pmap,' +
                            'point,pointcoord,pointer,poke,pos,preserve,print,' +
                            'private,procptr,property,protected,pset,ptr,public,' +
                            'put,random,randomize,read,reallocate,redim,rem,reset,' +
                            'restore,resume,return,rgb,rgba,right,rmdir,rnd,rset,' +
                            'rtrim,run,sadd,scope,screen,screencopy,screencontrol,' +
                            'screenevent,screenglproc,screeninfo,screenlist,screenlock,' +
                            'screenptr,screenres,screenset,screensync,screenunlock,' +
                            'seek,select,setdate,setenviron,setmouse,settime,' +
                            'sgn,shared,shell,shl,short,shr,sin,single,sizeof,' +
                            'sleep,space,spc,sqr,static,stdcall,step,stick,stop,' +
                            'str,string,strptr,sub,swap,system,tab,tan,then,this,' +
                            'threadcall,threadcreate,threaddetach,threadwait,time,' +
                            'timer,to,trans,trim,true,type,typeof,ubound,ubyte,' +
                            'ucase,uinteger,ulong,ulongint,union,unlock,unsigned,' +
                            'until,ushort,using,va_args,va_first,va_next,val,' +
                            'vallng,valint,valuint,valulng,var,varptr,view,' +
                            'virtual,wait,wbin,wchar,wend,while,whex,width,window,' +
                            'windowtitle,winput,with,woct,write,wspace,wstr,wstring,' +
                            'xor,zstring';

{ TMySEHighlighterFreeBasic }

function TMySEHighlighterFreeBasic.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterFB;
end;

function TMySEHighlighterFreeBasic.GetSampleSource: string;
begin
 Result := '''This will continue to print "hello" on the screen until the condition (a > 10) is met.' + #13#10 +
           'Dim a As Integer' + #13#10 +
           '' + #13#10 +
           'a = 1' + #13#10 +
           'Do' + #13#10 +
           '    Print "hello"' + #13#10 +
           'a = a + 1' + #13#10 +
           'Loop Until a > 10';
end;

constructor TMySEHighlighterFreeBasic.Create(AOwner: TComponent);
 Var I : Integer;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := FBKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z]', '[A-Za-z0-9]*');

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Free;

 DefTokDelim('"','"', tnString);
 DefTokDelim('''','', tnComment);
 DefTokDelim('__','__', tnDirective);

 //DefTokDelim('/\*', '\*/', tnComment, tdMulLin, False);

 //DefTokContent('__', '[A-Za-z0-9_]*', tnDirective);

 DefTokContent('[0123456789]','[0-9]', tnNumber);
 //DefTokContent('0x','[0-9a-fA-F]', tnNumber);

 fDefaultFilter := SYNS_FilterFB;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterFreeBasic.Destroy;
begin
  inherited Destroy;
end;

class function TMySEHighlighterFreeBasic.GetLanguageName: string;
begin
 Result := SYNS_LangFB;
end;

end.

