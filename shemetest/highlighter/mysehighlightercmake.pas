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
unit MySEHighlighterCMake;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynFacilHighlighter, SynEditHighlighter;

Type

  { TMySEHighlighterCMake }

  TMySEHighlighterCMake               = Class(TSynFacilSyn)
     Private
      fKeyWordList                       : TStringList;

      Procedure AddDirective(sDirective : String);

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
     SYNS_FilterCMake        = 'CMake source files (*.cmake)|*.cmake';
     SYNS_LangCMake          = 'CMake';

     CMakeKeyWords           = 'add_custom_command,add_custom_target,add_definitions,' +
                               'add_dependencies,add_executable,add_library,' +
                               'add_subdirectory,add_test,aux_source_directory,' +
                               'build_command,build_name,cmake_minimum_required,' +
                               'configure_file,create_test_sourcelist,' +
                               'else,elseif,enable_language,enable_testing,' +
                               'endforeach,endfunction,endif,endmacro,endwhile,' +
                               'exec_program,execute_process,export_library_dependencies,' +
                               'file,find_file,find_library,find_package,find_path,' +
                               'find_program,fltk_wrap_ui,foreach,function,' +
                               'get_cmake_property,get_directory_property,get_filename_component,' +
                               'get_source_file_property,get_target_property,get_test_property,' +
                               'if,include,include_directories,include_external_msproject,' +
                               'include_regular_expression,install,install_files,' +
                               'install_programs,install_targets,link_directories,' +
                               'link_libraries,list,load_cache,load_command,macro,' +
                               'make_directory,mark_as_advanced,math,message,option,' +
                               'output_required_files,project,qt_wrap_cpp,qt_wrap_ui,' +
                               'remove,remove_definitions,separate_arguments,set,' +
                               'set_directory_properties,set_source_files_properties,' +
                               'set_target_properties,set_tests_properties,site_name,' +
                               'source_group,string,subdir_depends,subdirs,' +
                               'target_link_libraries,try_compile,try_run,use_mangled_mesa,' +
                               'utility_source,variable_requires,vtk_make_instantiator,' +
                               'vtk_wrap_java,vtk_wrap_python,vtk_wrap_tcl,while,' +
                               'write_file';

     CMakeDirective          = 'ABSOLUTE,ABSTRACT,ADDITIONAL_MAKE_CLEAN_FILES,' +
                               'ALL,AND,APPEND,APPLE,ARGS,ASCII,BEFORE,BORLAND,CACHE,' +
                               'CACHE_VARIABLES,CLEAR,CMAKE_COMPILER_2005,COMMAND,' +
                               'COMMAND_NAME,COMMANDS,COMMENT,COMPARE,COMPILE_FLAGS,' +
                               'COPYONLY,CYGWIN,DEFINE_SYMBOL,DEFINED,DEPENDS,DOC,' +
                               'EQUAL,ESCAPE_QUOTES,EXCLUDE,EXCLUDE_FROM_ALL,EXISTS,' +
                               'EXPORT_MACRO,EXT,EXTRA_INCLUDE,FATAL_ERROR,' +
                               'FILES,FORCE,GENERATED,GLOB,GLOB_RECURSE,GREATER,' +
                               'GROUP_SIZE,HEADER_FILE_ONLY,HEADER_LOCATION,IMMEDIATE,' +
                               'INCLUDE_INTERNALS,' +
                               'INCLUDES,LESS,LINK_FLAGS,LOCATION,' +
                               'MACOSX_BUNDLE,MACROS,MAIN_DEPENDENCY,' +
                               'MATCH,MATCHALL,MATCHES,MINGW,MODULE,MSVC,MSVC_IDE,' +
                               'MSVC60,MSVC70,MSVC71,MSVC80,MSYS,NAME,NAME_WE,' +
                               'NO_SYSTEM_PATH,NOT,NOTEQUAL,OBJECT_DEPENDS,OFF,ON,' +
                               'OPTIONAL,OR,OUTPUT,OUTPUT_VARIABLE,PATH,PATHS,' +
                               'POST_BUILD,POST_INSTALL_SCRIPT,PRE_BUILD,PRE_INSTALL_SCRIPT,' +
                               'PRE_LINK,PREFIX,PREORDER,PROGRAM,PROGRAM_ARGS,' +
                               'PROPERTIES,QUIET,RANGE,READ,REGEX,REGULAR_EXPRESSION,' +
                               'REPLACE,REQUIRED,RETURN_VALUE,RUNTIME_DIRECTORY,' +
                               'SEND_ERROR,SHARED,SOURCES,STATIC,STATUS,STREQUAL,' +
                               'STRGREATER,STRLESS,SUFFIX,TARGET,TOLOWER,TOUPPER,' +
                               'VAR,VARIABLES,VERSION,WATCOM,WIN32,WRAP_EXCLUDE,WRITE';



{ TMySEHighlighterCMake }

procedure TMySEHighlighterCMake.AddDirective(sDirective: String);
begin
 AddIdentSpec(sDirective, tnDirective);
end;

function TMySEHighlighterCMake.IsFilterStored: Boolean;
begin
 Result := fDefaultFilter <> SYNS_FilterCMake;
end;

function TMySEHighlighterCMake.GetSampleSource: string;
begin
 Result := '#Text' + #13#10 +
           '#Text' + #13#10 +
           'function(verify_app)' + #13#10 +
           '  exec_program(${CMAKE_COMPILER}' + #13#10 +
           '    ARGS -dumpversion)' + #13#10 +
           '  if(NOT verified)' + #13#10 +
           '    message(FATAL_ERROR "error: verify_app failed")' + #13#10 +
           '  endif(NOT verified)' + #13#10 +
           'endfunction(verify_app)';
end;

constructor TMySEHighlighterCMake.Create(AOwner: TComponent);
 Var I, TokVar : Integer;
begin
 fKeyWordList := TStringList.Create;
 fKeyWordList.Delimiter := ',';
 fKeyWordList.StrictDelimiter := True;

 fKeyWordList.DelimitedText := CMakeKeyWords;

 Inherited Create(AOwner);

 ClearMethodTables;
 ClearSpecials;

 DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');

 For I := 0 To fKeyWordList.Count - 1 Do
  AddKeyword(fKeyWordList[I]);

 fKeyWordList.Clear;
 fKeyWordList.DelimitedText := CMakeDirective;

 For I := 0 To fKeyWordList.Count - 1 Do
  AddDirective(fKeyWordList[I]);

 fKeyWordList.Free;

 TokVar := NewTokType(SYNS_AttrVariable);

 DefTokDelim('"','"', tnString, tdMulLin);
 DefTokDelim('#','', tnComment);
 DefTokDelim('${', '}', TokVar);

 DefTokContent('[0123456789]','[0-9]', tnNumber);

 fDefaultFilter := SYNS_FilterCMake;

 Rebuild;

 SetAttributesOnChange(@DefHighlightChange);
end;

destructor TMySEHighlighterCMake.Destroy;
begin
  inherited Destroy;
end;

class function TMySEHighlighterCMake.GetLanguageName: string;
begin
 Result := SYNS_LangCMake;
end;

end.

