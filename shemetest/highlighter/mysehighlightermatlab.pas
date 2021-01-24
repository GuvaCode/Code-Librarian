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
unit MySEHighlighterMatlab;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

    { TMySEHighlighterMatlab }

    TMySEHighlighterMatlab               = Class(TSynFacilSyn)
     Private
      fKeyWordList                       : TStringList;
      tnFunctions                        : Integer;

     Protected
      function IsFilterStored: Boolean; override;
      function GetSampleSource: string; override;

     Public
      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;

      class function GetLanguageName: string; override;
    end;

implementation

Uses SynFacilBasic, SynEditStrConst;

Const
     SYNS_FilterMatlab        = 'MATLAB files (*.m)|*.m';
     SYNS_LangMatlab          = 'MATLAB';

     MatlabKeyWords           = 'break,case,catch,classdef,continue,else,elseif,' +
                                'end,enumeration,events,for,function,global,if,' +
                                'methods,otherwise,parfor,persistent,properties,' +
                                'return,spmd,switch,try,while,true,false';

     MatlabFunctions          = 'abs,acos,acosh,add_annotation,add_block,add_line,' +
                                'add_param,all,and,any,asin,asinh,assignin,atan,' +
                                'atan2,atanh,balance,beep,bitand,bitcmp,bitget,' +
                                'bitor,bitset,bitshift,bitxor,builtin,callstats,' +
                                'cat,cd,ceil,cell,cell2struct,cellhorzcat,cells,' +
                                'char,chdir,check_system,chol,cholinc,cholupdate,' +
                                'class,clc,clear,clock,close_system,colon,' +
                                'compare_system,computer,conj,contourc,conv2,' +
                                'copyobj,cos,cosh,cputime,ctranspose,cumprod,' +
                                'cumsum,dbclear,dbcont,dbdown,dbquit,dbstack,' +
                                'dbstatus,dbstep,dbstop,dbtype,dbup,delete,' +
                                'delete_annotation,delete_block,delete_line,' +
                                'delete_param,det,diag,diary,diff,disp,display,' +
                                'dongarra,dos,double,dragrect,drawnow,echo,eig,eps,' +
                                'eq,error,errortrap,eval,evalc,evalin,exist,exit,' +
                                'exp,expm,eye,fclose,feature,feof,ferror,feval,fft,' +
                                'fftn,fftw,fgets,fieldnames,fields,filesep,fill,' +
                                'fill3,filter,find,find_system,findpackage,findstr,' +
                                'findtype,finite,fix,floor,fopen,format,fprintf,' +
                                'frame2im,fread,fscanf,fschange,fseek,ftell,full,' +
                                'func2str,functions,functionscalled,fwrite,ge,' +
                                'getenv,getframe,gs_get_buttonmotion,gt,handle,' +
                                'handle2struct,hardcopy,hcreate,help,hess,hittest,' +
                                'home,horzcat,hregister,ieee,ifft,ifftn,im2frame,' +
                                'imag,import,inf,inferiorto,inmem,input,inputname,' +
                                'int16,int32,int8,inv,isa,iscell,ischar,isempty,' +
                                'isequal,isfinite,isglobal,ishandle,isieee,isinf,' +
                                'isjava,isletter,islogical,isnan,isreal,isruntime,' +
                                'isspace,issparse,isstr,isstudent,java,java_array,' +
                                'java_method,java_object,javaArray,javaMethod,' +
                                'javaObject,keyboard,lasterr,lastwarn,ldivide,le,' +
                                'length,license,load,log,log2,logical,loglog,lookfor,' +
                                'lower,lt,ltitr,lu,luinc,magic,matlabpath,matlabroot,' +
                                'max,mexext,mfilename,mimofr,min,minus,mislocked,' +
                                'mldivide,mlock,mod,more,movie,mpower,mrdivide,mtimes,' +
                                'munlock,nan,nargin,nargout,ndims,ne,new_system,norm,' +
                                'not,numel,ones,open_system,or,pack,pause,permute,' +
                                'pfile,pi,plot,plot3,plus,pow2,power,prod,qr,qrupdate,' +
                                'quit,qz,rand,randn,rbbox,rcond,rdivide,real,rehash,' +
                                'rem,reset,reshape,rmappdata,round,rtwgen,runtime,' +
                                'save,save_system,schur,selectmoveresize,semilogx,' +
                                'semilogy,setappdata,setstr,sign,sim,simulink,simver,' +
                                'sin,single,sinh,size,sldebug,sort,sparse,sparsfun,' +
                                'sprintf,sqrt,sscanf,str2func,strcmp,strcmpi,strfind,' +
                                'string,strncmp,strncmpi,strrep,struct,struct2cell,' +
                                'struct2handle,subsasgn,subsindex,subsref,sum,' +
                                'superiorto,svd,system,system_dependent,tan,tanh,' +
                                'tic,times,title,toc,transpose,tril,triu,trmginput,' +
                                'type,u_convert_to_gobject,uigetfile,uimenufcn,' +
                                'uint16,uint32,uint8,uipushtool,uiputfile,uisetcolor,' +
                                'uisetfont,uitoggletool,uitoolbar,uminus,unix,uplus,' +
                                'upper,version,vertcat,vms,waitfor,waitforbuttonpress,' +
                                'warning,what,which,who,whos,xlabel,xlate,ylabel,zeros';

{ TMySEHighlighterMatlab }

function TMySEHighlighterMatlab.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterMatlab;
end;

function TMySEHighlighterMatlab.GetSampleSource: string;
begin
 Result := '   %{' + #13#10 +
           '   Comment' + #13#10 +
           '   block' + #13#10 +
           '   %}' + #13#10 +
           '% comment' + #13#10 +
           ' 1' + #13#10 +
           ' .1' + #13#10 +
           ' 1.1' + #13#10 +
           ' .1e1' + #13#10 +
           ' 1.1e1' + #13#10 +
           ' 1e1' + #13#10 +
           ' 1i - (4i)' + #13#10 +
           ' 1j' + #13#10 +
           ' 1e2j' + #13#10 +
           ' 0x2A' + #13#10 +
           ' 0X2A' + #13#10 +
           ' 0b101010' + #13#10 +
           ' 0B101010' + #13#10 +
           ' 0x2Au8' + #13#10 +
           ' 0x2As32' + #13#10 +
           '' + #13#10 +
           'semilogy(x,y1,'#39'-bo;y1;'#39',x,y2,'#39'-kx;y2;'#39');' + #13#10 +
           'title('#39'Plot title'#39');' + #13#10 +
           'xlabel('#39'X Axis'#39');' + #13#10 +
           'print -deps graph.eps' + #13#10 +
           '' + #13#10 +
           'classdef (Sealed = false) classname < baseclass' + #13#10 +
           '   properties (SetAccess = private, GetAccess = true)' + #13#10 +
           '      PropName' + #13#10 +
           '   end' + #13#10 +
           '   methods' + #13#10 +
           '      methodName' + #13#10 +
           '   end' + #13#10 +
           '   events' + #13#10 +
           '      EventName' + #13#10 +
           '   end' + #13#10 +
           '   enumeration' + #13#10 +
           '      EnumName' + #13#10 +
           '   end' + #13#10 +
           'end' + #13#10 +
           '' + #13#10 +
           'function y = average(x)' + #13#10 +
           'end';
end;

constructor TMySEHighlighterMatlab.Create(AOwner: TComponent);
 Var I : Word;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := MatlabKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 tnFunctions := NewTokType(SYNS_AttrFunction);

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Clear;
 fKeyWordList.DelimitedText := MatlabFunctions;

 For I := 0 To fKeyWordList.Count - 1 Do
  AddIdentSpec(fKeyWordList[I], tnFunctions);

 fKeyWordList.Free;

 DefTokDelim('''','''', tnString, tdMulLin);
 DefTokDelim('%','', tnComment);
 DefTokDelim('%{','%}', tnComment, tdMulLin, False);

 DefTokContent('[0123456789eij]','[0-9eij]', tnNumber);
 DefTokContent('0x','[0-9A-Fa-fus]*', tnNumber);
 DefTokContent('0b','[0-1]*', tnNumber);

 fDefaultFilter := SYNS_FilterMatlab;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterMatlab.Destroy;
begin
 Inherited Destroy;
end;

class function TMySEHighlighterMatlab.GetLanguageName: string;
begin
 Result := SYNS_LangMatlab;
end;

end.

