// = imcom.inc =======================================================
//
// Common utils/types include file for im-double/tag
// requires: libraries from FPC package (ver. >= 2.2.0)
//           sysutils, strutils, inifiles, classes
//
// [Gromov, MPIC, 2007-2008]
// ===================================================================

// - compilation properties ----------------------------------------
//   please verify these with max_... constants
//

{$M+}
{$M 50331648,33554432}
// {$M 67108864,67108864}
{$MODE DELPHI}
{$COPERATORS ON}

// -------------------------------------------------------------------


// - conditional defines ---------------------------------------------

{$DEFINE xUSE_IF_IN_RESETPT}

{$DEFINE ADD_DUMIND}      // add dummy indices for the species listed in TSL but not present
                          // in current mech (solves missing declaration problems)

{$DEFINE ADDTRACDEF_H2O}  // add a fake H2O tracer record to create tagging tracers, if H2O in TSL

// - used libraries --------------------------------------------------

uses sysutils, strutils, inifiles, classes, unix, unixutil;

// -------------------------------------------------------------------

// - types and system chars ------------------------------------------

const _LF = #10;                              // linefeed char
const _errstr = '<error>';                    // standart error string
const _nonestr = '<none>';                    // standart none string
const _delims = [#0,#9,#27,#32,':',';',','];  // parameters delimiters allowed

const _etagspsy = '?';                        // "ever tagged" param.spec.sym. in eqn ( {/#} )

// common type for strings, etc., species names
type nstr = string[32];

// -------------------------------------------------------------------

// ->- some fundamental physics in this pure programming 
const _elems = 12;          // just in case, those main found in gas.spc + n
                            // update this list in case you use missing atoms here
const elems : array[-1.._elems] of record 
      name : string;
      mass : real;
      end = (
      (name:'?';  mass:01.0000    ),   // unknown quark
      (name:'n';  mass:01.008665  ),   // flegmatic neutron
      (name:'H';  mass:01.00794   ),   // slender hydrogen
      (name:'O';  mass:15.9994    ),   // rolling oxygen
      (name:'N';  mass:14.003074  ),   // turbo nitrogen
      (name:'C';  mass:12.011     ),   // blackhole carbon
      (name:'Cl'; mass:34.968852  ),   // sicky greeny clorine
      (name:'Br'; mass:78.918336  ),   // junky bromine
      (name:'I';  mass:126.904476 ),   // silly iodine
      (name:'S';  mass:32.066     ),   // devil's sulphur
      (name:'F';  mass:18.9984    ),   // teeth fluorine
      (name:'Hg'; mass:200.59     ),   // freddy mercury
      (name:'hv'; mass:0          ),   // happy-nude-photon 
      (name:'ignore'; mass:0      ));  // phantom mecca bozone
// -<- voila!

// -------------------------------------------------------------------

procedure writebreak;
begin
writeln('________________________________________________________________________________');
end;

// to avoid using math unit
function min(o1, o2 : longint) : longint;
begin if (o2<o1) then min:=o2 else min:=o1; end;
function max(o1, o2 : longint) : longint;
begin if (o2<o1) then max:=o1 else max:=o2; end;
function maxr(o1, o2 : real) : real;
begin if (o2<o1) then maxr:=o1 else maxr:=o2; end;

procedure imcom_error(msg : string);
begin
writeln('<!> ',msg);
halt(1);
end;

procedure imcom_check_files_exist(files : array of string);
var i : byte;
begin
for i:=low(files) to high(files) do
    if not(files[i]=_nonestr) then
       if not(fileexists(files[i])) then
          imcom_error('problem, file <'+files[i]+'> is not found. stop');
end;

// --- extracts text from src located between mark1 and mark2
function imcom_ext4marks(src, markL, markR : string) : string;
var i : integer;
    s : string;
begin
if (pos(markL,src)=0) or (pos(markR,src)=0) then
   imcom_ext4marks:=''
else
    begin
    i:=pos(markL,src)+length(markL);
    s:=copy(src,i,length(src)-i+1);
    imcom_ext4marks:=copy(s,1,pos(markR,s)-1);
    end;
end;

// --- removes ini comments (i.e. part of the string starting from ';')
procedure imcom_preprocess_ini(fname : string);
var f, o : text;
    s, p : string;
    i : integer;
begin

assign(f,fname); reset(f);
assign(o,'imcom.tmp'); rewrite(o);

while not(eof(f)) do
      begin
      readln(f,s);
      s:=trim(s);
      if (s='') then continue;
      if (s[1]=';') then continue;
      
      // processing lines, accounting backslashed \;, cutting end after ;
      p:=s[1];
      for i:=2 to length(s) do
          if s[i]<>';' then p+=s[i]
                       else if s[i-1]<>'\' then break
                                           else begin setlength(p,length(p)-1); p+=s[i]; end;
      // adding = to the lines to create keys
      p:=trim(p);
      if (p='') then continue;
      if (pos('=',p)=0) and (pos(']',p)=0) then p+='=';
      writeln(o,trim(p));
      end;

close(o);
close(f);      

end;


// - basics, consts ------------------------------------------------

// limitations: warning! there is no implemented range-check (only by compiler)!
const max_eqs = 3000;	    // max equations qty expected
      max_spc = 2000;       // max species qty expected
      max_prod = 40;	    // max qty of products in one equation expected
      max_tsl = 500;        // maximum number species to tag expected
      max_isos = 5;         // maximum number of isotopologues of one molecule expected
                            //   (or the max number of classes)
      max_form = 20;        // maximum number of different formers

const wrapmargin : integer = 80;  // wrapping margin

// - tagging/doubling configuration data fields ----------------------

// names for isotopologues & their data arrays
// nomenclature: I##A, I - isotopologue, ## - mass, A - atom	    example: I13C

// - general configuration parameters --------------------------------

var cmodel  : string;                      // carrier model prefix (opt.)
   _isos : integer;                        // number of isotopologues
    cfgname : string;                      // configuration name
    clsname : array[1..max_isos] of nstr;  // class names (inform. only, p.e. '12', '13' or 'oz', 'iz')
    isomass : array[1..max_isos] of real;  // isotope masses
    isoatom : string;                      // atom: C, O, N, ....
    isoelem : integer;                     // corresponding entry in elems[]
    tagname : string;                      // tagging (or doubling) name
    kieproc : string;                      // kie-calculation (include) file former
    eqnproc : string;                      // configuration (include) file former
    cparams : string;                      // configuration parameters (via cond.defs)

    sisyntax, drsyntax : string;           // species index, doubled reactions naming syntax (ind_@, D@)
    ptsyntax, ptpsyntax, ptlsyntax : string; // passive tracers +production +loss PTs syntax
    eqnfname, spcfname : string;           // names of files created for MECCA (equation, species)
    tracdef : string;                      // tracers definition file

    eqnfname_conf, spcfname_conf : string;         // names of MECCA files created for configuration (equation, species)
   _form_conf : integer;                           // # of configuration formers
    form_conf : array[1..max_form,1..2] of string; // formers (templates) filenames, output filenames
   _form_intr : integer;                           // # of inter-configuration formers
    form_intr : array[1..max_form,1..2] of string; // interconf.-formers (templates) filenames, output filenames

    nooftagreac : integer;                 // number of tagged reactions (with srcs!)
{$IFDEF USE_PT}
    ptracs_intr : ansistring;   // inter-conf passive tracers list    (':abbr>')
   _ptracs_intr : integer;      //   their count
    ptracs_conf : ansistring;   // configuration passive tracers list
   _ptracs_conf : integer;      //   their count
{$ENDIF}

    addeqs : array[1..max_eqs] of string;
   _addeqs : integer;

// - tagged species list management ----------------------------------

var tsl : array[0..max_tsl] of record
        spec : nstr;			     // spec name
        qatm : integer;			     // quantity of isoatoms (i.e. 1 for CH4, 3 for C3H8) in the molecule
        nspc : integer;                      // corresponding # in total species list (spc)
        mass : real;                         // mass of the molecule (for RMA KIE, derived from composition)
        ival : array[1..max_isos] of nstr;   // initial value (expr. in delta or minfrac, for each minor isot.)
        ifix : boolean;	                     // is a fixed spec?
        iprod, iloss : boolean;	             // write production/loss PTs?
        isos : array[1..max_isos] of nstr;   // isotopologues names (for doubling)
        end;
   _tsl : integer;

var bsl : array[1..max_tsl] of record
        spec : nstr;                         // list of species to budget
        iprod, iloss : boolean;	             // write production/loss PTs?
        end;
   _bsl : integer;

// used tagged species no.
var utsl : array[1..max_tsl] of integer; // references to tsl's which are used in the curr. mech.
   _utsl : integer;

// species acting in the isotope exchange
var iesl : array[1..max_tsl] of integer; // references to tsl's which are used in the curr. mech.
   _iesl : integer;

// - isotopes specific data ------------------------------------------

var kie : array[1..max_eqs*max_isos] of record
        abbr : nstr;                         // reaction name (abbr.)
        eqno : integer;                      // reaction no. (in eqs[])
        isot : nstr;                         // isotopologue to apply to (p.e. I12CO)
        expr : string;                       // expression (like "/alpha") where alpha is an updated KIE fractionation factor
        imec : boolean;                      // is in the mechanizm?
        end;
   _kie : integer;
var rmakie : record 
           include,  exclude : array[1..max_eqs] of nstr; // names of reactions to assign RMA KIE to, and
          _include, _exclude : integer;                   // names of reactions to exclude from the previous list
    end;

var src : array[1..max_eqs] of record	  // array for (missing) sources specification
        abbr : nstr;                         // reaction name
        eqno : integer;                      // reaction no. (in eqs[])
         def : integer;                      // default source entry no (if no products specified)
      _trans : integer;                      // no. of sources in the current reaction
       trans : array[1..max_prod] of record  // isotopic transfer information:
             src : nstr;                         // name of the "source" species,
          weight : real;                         // optional weight
            _dst : integer;
             dst : array[1..max_prod] of record
                 spec : nstr;                    // name of the "destination" species
                 stoi : real;                    // "stoichiometric" coefficient of the isotopic transfer
                 end;
             end;
        end;
   _src : integer;                        // no of equations with sources specification

var iex : array[1..max_eqs] of record     // array for isotope exchange reactions information
        abbr : nstr;                          // reaction name
        exspec : array[1..2] of integer;      // indices of the exchanging species in TSL
        phys : string;                        // reaction rate and other (like {%TrG}) information
        imec : boolean;                       // is in the mechanism?
        end;
   _iex : integer;                        // quantity

// --- formers (templates) management -----------------------------------------

// replacements for constants, names, etc. in {%$} templates

const _imcom_reps = 23;
       imcom_reps : array[1.._imcom_reps,1..2] of string =
		( ('{%TIMEDATE}',''),     // time, date

		  ('{%CASE}',''),         // tagging or doubling case
		  ('{%CMODEL}',''),       // carrier model name

		  ('{%ATOM}',''),         // atom string
		  ('{%A}',''),            // atom repeated
		  ('{%CONF}',''),         // configuration
		  ('{%C}',''),            // same

		  ('{%QSPEC}',''),        // # of tagged/doubled specs
		  ('{%NSPEC}',''),        // # of tagged/doubled specs identifier
		  ('{%QCLASS}',''),       // (same as %QISO) number
		  ('{%NCLASS}',''),       // (same as %NISO) identifier
                  
		  ('{%QISO}',''),         // # of isotopologues
		  ('{%NISO}',''),         // # of isotopologues identifier

		  ('{%NQATOM}',''),       // # of isoatoms in molecule array identifier

		  ('{%PT}',''),           // PT syntax

		  ('{%TAG}',''),          // tagging name
       		  ('{%RSIND}',''),        // array of related regular specs indentifiers
       		  ('{%QCFLDIR}',''),      // # of flow directions in the (C)urrent configuration
       		  ('{%QIFLDIR}',''),      // # of flow directions in all configurations, (I)nter

		  ('{%DBL}',''),          // doubling name
		  ('{%TDIND}',''),        // indices of totals in the mech (reg+dbl) for doubling

		  ('\t',''),              // TAB symbol
		  ('\n','') );            // CR+LF symbol
       imtag_QCFLDIR = 18;
       imtag_QIFLDIR = 19;

procedure imcom_update_reps;
begin

imcom_reps[01,2]:=datetimetostr(now);
imcom_reps[02,2]:={$IFDEF TAG}'tag'{$ENDIF}{$IFDEF DBL}'dbl'{$ENDIF};
imcom_reps[03,2]:=cmodel;
imcom_reps[04,2]:=isoatom;
imcom_reps[05,2]:=isoatom;
imcom_reps[06,2]:=cfgname;
imcom_reps[07,2]:=cfgname;

imcom_reps[08,2]:=inttostr(_utsl);
imcom_reps[09,2]:=tagname+'_'+'N'+{$IFDEF TAG}'T'{$ENDIF}{$IFDEF DBL}'D'{$ENDIF}+'SPEC';

imcom_reps[10,2]:=inttostr(_isos);
imcom_reps[11,2]:=tagname+'_'+'N'+{$IFDEF TAG}'T'{$ENDIF}{$IFDEF DBL}'D'{$ENDIF}+'CLASS';

imcom_reps[12,2]:=inttostr(_isos);
imcom_reps[13,2]:=tagname+'_'+'N'+{$IFDEF TAG}'T'{$ENDIF}{$IFDEF DBL}'D'{$ENDIF}+'ISO';

imcom_reps[14,2]:=tagname+'_'+'Q'+{$IFDEF TAG}'T'{$ENDIF}{$IFDEF DBL}'D'{$ENDIF}+'ATOM';
imcom_reps[15,2]:=ptsyntax;

imcom_reps[16,2]:=tagname;
imcom_reps[17,2]:=tagname+'_'+'RSIND';
imcom_reps[imtag_QCFLDIR,2]:='-1';                    // so far, updated in imtag
imcom_reps[imtag_QIFLDIR,2]:='-1';                    // so far, updated in imtag

imcom_reps[20,2]:=tagname;
imcom_reps[21,2]:=tagname+'_'+'TDIND';

imcom_reps[22,2]:=#9;
imcom_reps[23,2]:=_LF;

end;

procedure imcom_make_reps(var s : ansistring);
var i, p : word;
begin

for i:=1 to _imcom_reps do
    begin
    p:=pos(imcom_reps[i,1],s);
    while (p>0) do
	  begin
	  delete(s,p,length(imcom_reps[i,1]));
	  insert(imcom_reps[i,2],s,p);
	  p:=pos(imcom_reps[i,1],s);
	  end;
    end;

end;


// --- species list management ---------------------------------------

// --- check whether the species is in the tagging-species-list
function in_tsl(spec : nstr) : boolean;      // = (if (no_tsl(spec)>0))
var i : integer;
begin
for i:=1 to _tsl do
    if (spec=tsl[i].spec) then
       begin in_tsl:=true; exit; end;
in_tsl:=false;
end;

// --- no of the species in the tagging-species-list
function no_tsl(spec : nstr) : integer;
var i : integer;
begin
for i:=1 to _tsl do
    if (spec=tsl[i].spec) then
       begin no_tsl:=i; exit; end;
no_tsl:=0;
end;

// --- check whether the species is in the budgeting-species-list
function in_bsl(spec : nstr) : boolean;      // = (if (no_bsl(spec)>0))
var i : integer;
begin
for i:=1 to _bsl do
    if (spec=bsl[i].spec) then
       begin in_bsl:=true; exit; end;
in_bsl:=false;
end;

// --- no of the species in the budgeting-species-list
function no_bsl(spec : nstr) : integer;
var i : integer;
begin
for i:=1 to _bsl do
    if (spec=bsl[i].spec) then
       begin no_bsl:=i; exit; end;
no_bsl:=0;
end;

// --- sources list management ---------------------------------------

// --- no of the src entry for a given reaction
function no_src(reac : nstr) : integer;
var i : integer;
begin
for i:=1 to _src do
    if (reac=src[i].abbr) then
       begin no_src:=i; exit; end;
no_src:=0;
end;

// --- returns the number of the trans with a given source spec.
function no_src_trans(src_no : integer; spec : nstr) : integer;
var i : integer;
begin
for i:=1 to src[src_no]._trans do
    if (spec=src[src_no].trans[i].src) then
       begin no_src_trans:=i; exit; end;
no_src_trans:=0;
end;

// --- returns the number of the dest. species in given src->trans
function no_trans_dst(src_no, trans_no : integer; spec : nstr) : integer;
var i : integer;
begin
for i:=1 to src[src_no].trans[trans_no]._dst do
    if (spec=src[src_no].trans[trans_no].dst[i].spec) then
       begin
       no_trans_dst:=i;
       exit;
       end;
no_trans_dst:=0;
end;

// === read the tagging configuration list ===========================
procedure imcom_read_tag_config(fname : string);

// extract the next first parameter in the line (i.e. remove it)
function ext_next(var s : ansistring) : string;
var a : string;
begin
s:=trim(s); s:=s+' ';
a:=copy(s,1,pos(' ',s));
delete(s,1,length(a)-1);
a:=trim(a);
ext_next:=a;
end;

var i, j, k, l : integer;
    s, a, b : ansistring;
    r : real;

    ini : tinifile;
    info : tstringlist;

begin

writeln('reading tagging info file: ',fname);  // a ext

// preprocessing ini file for correct parsing
imcom_preprocess_ini(fname);                   

// reading tagging info file
ini:=tinifile.create('imcom.tmp');
info:=tstringlist.create;

// ---------------------------------------------------------
// reading TAG section first

_isos:=0;
isoatom:=''; 
kieproc:=''; eqnproc:='';
fillchar(isomass,sizeof(isomass),0);
{$IFDEF USE_PT}
_ptracs_conf:=0; ptracs_conf:='';
{$ENDIF}

ini.readsection('TAG',info);
if (info.count<=0) then
   imcom_error('error: [TAG] section information is missing. stop.');

if ( (info.indexof('configuration')<0) or
     (info.indexof('classes')<0) ) then
   imcom_error('error: some of (configuration,classes) required fields are missing or mistyped in [TAG] section. stop.');

// configuration name
cfgname:=extractword(1,ini.readstring('TAG','configuration',_errstr),_delims);

// classes no & names
s:=ini.readstring('TAG','classes',_nonestr);
if (s=_nonestr) then
   imcom_error('error: no information on tagging classes, check (classes) field in the configuration. stop.');

_isos:=wordcount(s,_delims);                  // # of classes or isotopologues
s:=ini.readstring('TAG','classes',_errstr);
for i:=1 to _isos do
    clsname[i]:=extractword(i,s,_delims);

// optional parameters
cmodel:=extractword(1,ini.readstring('TAG','cmodel',_nonestr),_delims);         // carrier model (added prefix)

// optional parameters for isotopes
isoatom:=extractword(1,ini.readstring('TAG','iatom',_nonestr),_delims);         // atom (optional)
kieproc:=extractword(1,ini.readstring('TAG','kieproc',_nonestr),_delims);       // kie-processing file (optional if no KIE specified)
eqnproc:=extractword(1,ini.readstring('TAG','eqnproc',_nonestr),_delims);       // eqn-processing file (optional if no KIE specified)
s:=ini.readstring('TAG','imasses',_nonestr);
if ((s<>_nonestr) and (s<>'')) then
   for i:=1 to _isos do
       begin
       isomass[i]:=strtofloatdef(extractword(i,s,_delims),-1.0);                // isotope masses
       if (isomass[i]=-1.0) then
          imcom_error('problem recognizing #'+inttostr(i)+' isotope mass ('+extractword(i,s,_delims)+'). stop');
       end;

// species to budget
_bsl:=0; fillchar(bsl,sizeof(bsl),0);
s:=ini.readstring('TAG','budget',_nonestr);
if ((s<>_nonestr) and (s<>'')) then
   for i:=1 to wordcount(s,_delims) do
       begin
       inc(_bsl);
       bsl[_bsl].spec:=extractword(i,s,_delims);
       bsl[_bsl].iprod:=false;
       bsl[_bsl].iloss:=false;
       end;

// user configuration parameters (cond.defs.)
cparams:='';
info.clear;
ini.readsection('CPARAMS',info);
for i:=0 to info.count-1 do
    cparams+=info[i]+' ';

// internal naming (so far)
{$IFDEF TAG}
tagname:='tag_'+cfgname;        // name of tagging, p.e. tag_IC
{$ENDIF}
{$IFDEF DBL}
tagname:='dbl_'+cfgname;        // name of doubling, p.e. dbl_IO
{$ENDIF}

// un peu d'info
if cmodel='' then cmodel:=_nonestr;
if isoatom='' then
   isoatom:=_nonestr
else // searching for corresponding element for
    begin
    isoelem:=-1;
    for i:=1 to _elems do
        if (upcase(isoatom)=upcase(elems[i].name)) then
           isoelem:=i;
    if (isoelem=-1) then
       writeln('warning: selected isotope ('+isoatom+') element is not found in the reference table (elems[]). <may be normal>');
    end;
if kieproc='' then kieproc:=_nonestr;
if eqnproc='' then eqnproc:=_nonestr;

writeln('      carrier: ',cmodel);
writeln('configuration: ',cfgname);
{$IFDEF TAG}
writeln('      tagging: ',tagname);
{$ENDIF}
{$IFDEF DBL}
writeln('     doubling: ',tagname);
{$ENDIF}

  write('      classes: ',_isos,' [ ');
for i:=1 to _isos do
    write(clsname[i],' ');
writeln(']');
writeln('     eqn-proc: ',eqnproc);
writeln('       params: ',cparams);
  write('       budget: '); for i:=1 to _bsl do write(bsl[i].spec,' '); writeln;
writeln(' isotope-specific:');
writeln('        iatom: ',isoatom,' [',isoelem,']');
  write('      imasses: ');
if (isomass[1]>0.0) then
   begin
   write('[ ');
   for i:=1 to _isos do
      write(floattostr(isomass[i]),' ');
   writeln('] (',elems[isoelem].mass,')');
   end
else
    writeln('not specified');
writeln('     kie-proc: ',kieproc);
writeln;

if (cmodel=_nonestr) then cmodel:='';
if (isoatom=_nonestr) then isoatom:='';

// configuration is known by now, updating replacements for templates
imcom_update_reps;

// ---------------------------------------------------------
// MECCA-specific files

// modified MECCA equation file name
eqnfname:=extractword(1,ini.readstring('MECCA','eqn',(cmodel+'_'+tagname+'_mecca.eqn')),_delims);
// modified MECCA additional species file
spcfname:=extractword(1,ini.readstring('MECCA','spc',(cmodel+'_'+tagname+'_mecca.spc')),_delims);
// species' index syntax (opt., default is ind_@)
sisyntax:=extractword(1,ini.readstring('MECCA','sisyntax','ind_@'),_delims);
// PTs syntax (opt., default is PT@)
ptsyntax:=extractword(1,ini.readstring('MECCA','ptsyntax','PT@'),_delims);
   // define prod/loss PTs particular syntax here
   ptlsyntax:=extractword(1,ini.readstring('MECCA','ptlsyntax','PTL@'),_delims);
   ptpsyntax:=extractword(1,ini.readstring('MECCA','ptpsyntax','PTP@'),_delims);
// doubled reac. naming syntax (opt., default is D@)
drsyntax:=extractword(1,ini.readstring('MECCA','drsyntax','D@'),_delims);
// tracers defs
tracdef:=extractword(1,ini.readstring('MECCA','tracdef',(cmodel+'_'+tagname+'.tex')),_delims);

// configuration eqn/spc files
// equation file name
eqnfname_conf:=extractword(1,ini.readstring('MECCA','cfgeqn',(cmodel+'_'+tagname+'.eqn')),_delims);
// additional species file
spcfname_conf:=extractword(1,ini.readstring('MECCA','cfgspc',(cmodel+'_'+tagname+'.spc')),_delims);

imcom_make_reps(sisyntax);
imcom_make_reps(ptsyntax);
imcom_make_reps(eqnfname); imcom_make_reps(spcfname);
imcom_make_reps(tracdef);

imcom_make_reps(eqnfname_conf); imcom_make_reps(spcfname_conf);

// un peu d'info
writeln('MECCA-specific parameters:');
writeln('  spec.ind syntax: ',sisyntax);
writeln('       PTs syntax: ',ptsyntax);
writeln(' doub.reac syntax: ',drsyntax);
writeln('   equation files: ',eqnfname,' / ',eqnfname_conf);
writeln('    species files: ',spcfname,' / ',spcfname_conf);
writeln;

// ---------------------------------------------------------
// code former (template) files

_form_conf:=0;

// reading configuration formers
a:='CODE:'+{$IFDEF TAG}'TAG'{$ENDIF}{$IFDEF DBL}'DBL'{$ENDIF}+'=CONF';
info.clear;
ini.readsection(a,info);

if (info.count>max_form) then
   imcom_error('oops, too many formers to process. see read_tag_config and max_form. stop.');

for i:=0 to info.count-1 do
 if (info[i]<>'') then   // necessary condition to avoid empty keys of occasional trash in cfg
    begin
    inc(_form_conf);           // number of templates

    form_conf[_form_conf,1]:=trim(info[i]);	  // former filename

    // reading postfix
    s:=ini.readstring(a,info[i],_errstr);

    // making the output filename   (p.e. 'messy_mecca_tag_IC_box.f90', where postfix is '_box')
    form_conf[_form_conf,2]:=trim(extractword(1,s,_delims));

    imcom_make_reps(form_conf[_form_conf,1]);
    imcom_make_reps(form_conf[_form_conf,2]);

    end;

// un peu d'info
writeln('#',_form_conf,' configuration former(s):');
for i:=1 to _form_conf do
    writeln(form_conf[i,1]:25,' -> ',form_conf[i,2]);


// reading inter-configuration formers
a:='CODE:'+{$IFDEF TAG}'TAG'{$ENDIF}{$IFDEF DBL}'DBL'{$ENDIF};
info.clear;
ini.readsection(a,info);

writeln('#  inter-configuration former(s):');

if ((_form_intr+info.count)>max_form) then
   imcom_error('oops, too many formers to process. see read_tag_config and max_form. stop.');

for i:=0 to info.count-1 do
 if (info[i]<>'') then   // necessary condition to avoid empty keys of occasional trash in cfg
    begin
    inc(_form_intr);           // number of templates

    form_intr[_form_intr,1]:=trim(info[i]);	  // former filename

    // reading postfix
    s:=ini.readstring(a,info[i],_errstr);

    // making the output filename   (p.e. 'messy_mecca_tag_IC_box.f90', where postfix is '_box')
    form_intr[_form_intr,2]:=trim(extractword(1,s,_delims));

    imcom_make_reps(form_intr[_form_intr,1]);
    imcom_make_reps(form_intr[_form_intr,2]);

    write(form_intr[_form_intr,1]:25,' -> ',form_intr[_form_intr,2]);

    // checking doubled inter-conf formers
    for j:=1 to _form_intr-1 do
        if ((form_intr[j,1]=form_intr[_form_intr,1]) and
	    (form_intr[j,2]=form_intr[_form_intr,2])) then
	   begin
           dec(_form_intr);  // removing last, which is detected to be duped
	   write(' (detected as duplicate)');
	   break;
	   end;

    writeln;
    end;

writeln;

// ---------------------------------------------------------
// reading species section (SPC)

_tsl:=0; _utsl:=0;
fillchar(tsl,sizeof(tsl),0);
fillchar(utsl,sizeof(utsl),0);

tsl[0].spec:='<not found>';

ini.readsection('SPC',info);
if (info.count<=0) then
   imcom_error('error: [SPC] section information is missing. stop.');

info.clear;
ini.readsection('SPC',info);
for i:=0 to info.count-1 do
 if (info[i]<>'') then   // necessary condition to avoid empty keys of occasional trash in cfg
    if (not(in_tsl(extractword(1,info[i],_delims)))) then // avoid doubling mistype
       begin
       inc(_tsl);                         // number of specs. to be tagged

       tsl[_tsl].spec:=trim(extractword(1,info[i],_delims)); // species name

       // # of isoatoms in the molecule
       j:=strtointdef(extractword(2,info[i],_delims),0);
       tsl[_tsl].qatm:=j;		  // # of isoatoms in the molecule

       // reading species initial compostition data
       s:=ini.readstring('SPC',info[i],_errstr);

       // number of values in the line, filling in backward-direction
       //   i.e. for isotopes, I16 I17 I18 reading first 18th, then 17th, 16th isn't necessary
       //        for fractions, F1 F2 F3 are read 3 - 2 - 1 completing all, ot 3 - 2, leaving 1st for autom. definition
       k:=min(wordcount(s,_delims),_isos);
       for j:=_isos downto (_isos-k+1) do
           tsl[_tsl].ival[j]:=extractword(j-(_isos-k),s,_delims);
//     for j:=min(_isos,2) to _isos do      // initial value (delta or frac), (opt., def 0)
//         tsl[_tsl].ival[j]:=extractword(max(j-1,1),s,_delims);  // for one class read at least one value, for more read values for minors

       // is fixed species?
       tsl[_tsl].ifix:=(pos('*',s)>0);
       // write prod/loss PTs?
       tsl[_tsl].iprod:=(pos('P',s)>0);
       tsl[_tsl].iloss:=(pos('L',s)>0);
       end
     else
         writeln(' warning: double entry for ',info[i]);

// filling isotopologues names
// -- make corrections to names here
for i:=1 to _tsl do
    for j:=1 to _isos do
        tsl[i].isos[j]:=clsname[j]+tsl[i].spec;

// un peu d'info
writeln('#',_tsl,' species to tag: (initialisation units to be defined in the code, ''?'' denotes that atoms count is to be picked from SPC)');
  write('name':10,' (#',isoatom,') / ');
for j:=1 to _isos do
    write(clsname[j],isoatom,' ');
writeln;

for i:=1 to _tsl do
    with tsl[i] do
        begin
        write(spec:10,' (');
        if (qatm>0) then write(qatm) else write('?');
        write(isoatom,') / ');
        for j:=1 to _isos do
            write(ival[j]+' ');
        if (ifix) then write(' (indicated as fixed species)');
        if (iprod) then write(' (adding production PT)');
        if (iloss) then write(' (adding loss PT)');
        writeln;
        end;
writeln;


// ---------------------------------------------------------
// reading KIE info section

_kie:=0;
fillchar(kie,sizeof(kie),0);

info.clear;
ini.readsection('KIE',info);
for i:=0 to info.count-1 do
 if (info[i]<>'') then   // necessary condition to avoid empty keys due to occasional trash in cfg
    begin
    inc(_kie);
    kie[_kie].abbr:=extractword(1,trim(info[i]),_delims);  // reaction abbr.
    kie[_kie].isot:=extractword(2,trim(info[i]),_delims);  // isotopologue

    // expression = whole string
    s:=ini.readstring('KIE',info[i],_errstr);
    kie[_kie].expr:=trim(s);

    kie[_kie].eqno:=-1;               // eq. no in eqs[] is not known yet
    kie[_kie].imec:=false;            // by default, not present in the mech
    end;

writeln('#',_kie,' KIE-specific reaction(s): ');
for i:=1 to _kie do
    with kie[i] do
	 writeln(abbr:6,': for ',isot,' [ ',expr,' ]');
writeln;

// and RMA KIE information
fillchar(rmakie,sizeof(rmakie),0);

info.clear;
ini.readsection('KIE:RMA',info);

if (info.count>0) then
   begin
   
   // processing include-list
   a:=trim(ini.readstring('KIE:RMA','include',''));
   if (a<>'') then
      for i:=1 to wordcount(a,_delims) do
          with rmakie do
               begin
               inc(_include);
               include[_include]:=extractword(i,a,_delims);
               end;

   // and exclude-list
   a:=trim(ini.readstring('KIE:RMA','exclude',''));
   if (a<>'') then
      for i:=1 to wordcount(a,_delims) do
          with rmakie do
               begin
               inc(_exclude);
               exclude[_exclude]:=extractword(i,a,_delims);
               end;
   
   with rmakie do
        if (_include>0) or (_exclude>0) then
           begin
           writeln('RMA KIE info:');
             write(' #',_include,' to include ( ');
           for i:=1 to _include do
               write(include[i],' ');
           writeln(')');
             write(' #',_exclude,' to exclude ( ');
           for i:=1 to _exclude do
               write(exclude[i],' ');
           writeln(')');
           end;
   end
else
    writeln('RMA KIE: no information');

writeln;

// ---------------------------------------------------------
// reading IEX info section

_iex:=0;
fillchar(iex,sizeof(iex),0);

info.clear;
ini.readsection('IEX',info);
for i:=0 to info.count-1 do
 if (info[i]<>'') then   // necessary condition to avoid empty keys due to occasional trash in cfg
    begin
    inc(_iex);
    iex[_iex].abbr:=trim(info[i]);  // reaction abbr.

    // expression = whole string
    s:=ini.readstring('IEX',info[i],_errstr);
    for j:=1 to 2 do
        iex[_iex].exspec[j]:=no_tsl(extractword(j,s,_delims));  // exchanging species
    with iex[_iex] do
         if ((exspec[1]*exspec[2])=0) then    // in_tsl returns 0 if the spec is not found in TSL
            begin
            dec(_iex);
	    writeln(' warn: parsing [IEX] ',abbr:6,' for ',extractword(1,s,_delims),
                    ' <-> ',extractword(2,s,_delims),' : some of species are not found in TSL');
            end
         else
             begin
             phys:=trim(copy(s,pos(':',s)+1,length(s)-pos(':',s)));
             if (pos(';',phys)=0) then  // adding ';' to the rate expr. (limitation of tinifile unit)
                phys+=';';
             imec:=false;              // by default, not present in the mech
             end;
    end;

writeln('#',_iex,' isotope exchange reaction(s): ');
for i:=1 to _iex do
    with iex[i] do
	 writeln(abbr:6,': ',tsl[exspec[1]].spec,' <-> ',tsl[exspec[2]].spec,' ',phys);
writeln;

// ---------------------------------------------------------
// reading sources specification section (SRC)

write('source specification record(s): ');

_src:=0;
fillchar(src,sizeof(src),0);

a:='';
info.clear;
ini.readsection('SRC',info);
for i:=0 to info.count-1 do
 if (info[i]<>'') then   // necessary condition to avoid empty keys of occasional trash in cfg
    begin
    // reaction abbr.
    s:=extractword(1,trim(info[i]),_delims);

    j:=no_src(s);        // if already in the list, getting the entry no
    if (j=0) then
       begin                 // otherwise creating a new entry
       inc(_src);
       src[_src].abbr:=s;
       j:=_src;
       end;

    with src[j] do
         begin

         // getting source species weight and name
         s:=trim(extractword(2,info[i],_delims));
         if (s[1] in ['.','-','+','0'..'9']) then   // determine optional coefficient
            begin
            r:=strtofloatdef(s,1.0);                // storing coefficient value
            s:=trim(extractword(3,info[i],_delims));      // reading following species name
            end
         else
             r:=1.0;

         if not(in_tsl(s)) then  // not a tagged species, neglecting
            begin a+=s+' '; continue; end;

         k:=no_src_trans(j,s);
         if (k=0) then
            begin
            inc(_trans);         // new source entry
            trans[_trans].src:=s;
            trans[_trans].weight:=r;
            k:=_trans;
            end;
       
         // destination species
         s:=ini.readstring('SRC',info[i],_errstr);
         s:=trim(s);
         
         l:=1;
         if (s='') then          // indicates that all products are from current source
            def:=_trans          // not reading further
         else
             while (l<=wordcount(s,_delims)) do
                   begin
                   b:=extractword(l,s,_delims);      // b - destination spec. name

                   if (b[1] in ['.','-','+','0'..'9']) then   // determine optional coefficient
                      begin
                      r:=strtofloatdef(b,1.0);                // storing coefficient value
                      inc(l);
                      b:=trim(extractword(l,s,_delims)); // reading following species name
                      end
                   else
                       r:=1.0;

                   if not(in_tsl(b)) then
                      a+=b+' '
                   else
                       if (no_trans_dst(j,k,b)=0) then
                          with trans[_trans] do
                               begin
                               inc(_dst);
                               dst[_dst].spec:=b;
                               dst[_dst].stoi:=r;
                               end;
                   inc(l);
                   end;
         end;

    src[_src].eqno:=0;		             // equation no. in eqs[], not initialized so far
    end;

if _src>0 then writeln('#',_src) else writeln('no');

for i:=1 to _src do
    begin
    write(src[i].abbr:6,': (',src[i]._trans:2,', default: ');
    if (src[i].def>0) then write(format('%-10s',[src[i].trans[src[i].def].src]))
                      else write(format('%-10s',['-']));
    write(') : ');

    for j:=1 to src[i]._trans do
        begin
        if (src[i].trans[j].weight<>1.0) then
           write(floattostr(src[i].trans[j].weight)+' ');
        write(src[i].trans[j].src,' ->');
        for k:=1 to src[i].trans[j]._dst do
            begin
            write(' ');
            if (src[i].trans[j].dst[k].stoi<>1.0) then
               write(floattostr(src[i].trans[j].dst[k].stoi)+' ');
//             write(formatfloat('##0.#####',src[i].trans[j].dst[k].stoi)+' ');
            write(src[i].trans[j].dst[k].spec);
            end;
        if (j<src[i]._trans) then write(', ');
        end;
    writeln;
    end;
if (a<>'') then 
   writeln('following entries were not recognized (not in TSL?): ',a);
writeln;

// ---------------------------------------------------------
// reading additionally specified equations

_addeqs:=0;
fillchar(addeqs,sizeof(addeqs),0);

a:='';
info.clear;
ini.readsection('EQN',info);
for i:=0 to info.count-1 do
    begin
    inc(_addeqs);
    s:=ini.readstring('EQN',info[i],'');
    addeqs[_addeqs]:=info[i];
    if (s<>'') then 
      addeqs[_addeqs]+=' = '+s;
    end;
    
if (_addeqs>0) then
   begin
   writeln('additional EQN lines specified: ');
   for i:=1 to _addeqs do
       writeln(addeqs[i]);
   writeln;
   end;

// ---------------------------------------------------------

info.destroy;
ini.destroy;

writeln('read_tag_config: done');

writebreak;
end;


// - management des equations ----------------------------------------

// equation file by lines
var eqnfile : array[1..3*max_eqs] of record
	    line : ansistring;	      // read line
	    iseq : boolean;	      // "is an equation record?" flag
	    eqno : word;	      // equation # (supposed to coincide with internal # in mecca)
	    end;
   _eqnfile : word;		      // # of lines in eqnfile
   
var spcfile : array[1..3*max_spc] of record
	    line : ansistring;	      // read line
	    ispc : boolean;           // "is a species record?" flag
	    nspc : word;              // species # (k.a. if supposed to coincide with internal # in mecca)
	    end;
   _spcfile : word;		      // # of lines in spcfile

// species data-array
var spc : array[1..max_spc] of record
        spec, comp : nstr;            // name and contents (those that embrace = )
        capt : string;                // {@caption}, after the ;
        icomp : array[1..max_isos] of nstr; // isot. subsitutes composition
        icapt : array[1..max_isos] of string; // isot. subsitute caption
        qatm : integer;               // number of interesting isotope atoms (e.g. 2 for O in CH3O2)
        mass : real;                  // calculated with elem data (see below)
        tslno : integer;              // corresponding # in TSL
        end;
   _spc : integer;                    // number of species found in spc file

// equations data-array
var eqs : array[1..max_eqs] of record
	abbr : nstr;		      // abbreviature: <YO777>

       _educ : integer;
	educ : array[1..2] of nstr;   // educts:  ME + YOU

       _prod : integer;			   // quantite' des produits   (quantity of the products)
	prod : array[1..max_prod] of nstr; // products:  WE + THEY + US + ...
	stoi : array[1..max_prod] of real; // corr. stoichiometric coefficients:  1, many, some

	phys : string;		      // right-part: {%DrSmSl} super*fast*rate;
        
	isrc : boolean; 	      // source-specification flag     (pipe me!)
	nsrc : integer;		      // specification list entry no.
        
        iiex : boolean;               // is isotope exchange r-n?
        niex : integer;               // iex internal number

	itag : boolean; 	      // is tagged?
        etag : integer;               // is ever tagged? >0 tells how many times
	ntag : integer;               // tagging internal number
	
	if2t : boolean;               // are both educts tagged?
        if2s : boolean;               // are the educts same? (i.e. self-reacting)
	end;
   _eqs : word; 		      // quantite' des reactions


// --- f-n returns back the number in eqs array for a certain reac abbr
function no_eqs(abbr : nstr) : integer;
var i : integer;
begin
for i:=1 to _eqs do
    if (abbr=eqs[i].abbr) then
       begin no_eqs:=i; exit; end;
no_eqs:=0;
end;

// --- f-n returns back the number in eqs array for a certain reac abbr
function no_educ(eqno : integer; ceduc : nstr) : integer;
var i : integer;
begin
with eqs[eqno] do
     for i:=1 to _educ do
         if (educ[i]=ceduc) then
            begin no_educ:=i; exit; end;
no_educ:=0;
end;

// --- f-n returns back the true if a spec in the  spc array
function in_spc(spec : nstr) : boolean;      // = (if (no_tsl(spec)>0))
var i : integer;
begin
for i:=1 to _spc do
    if (spec=spc[i].spec) then
       begin in_spc:=true; exit; end;
in_spc:=false;
end;


// --- no of the species in the spc array
function no_spc(spec : nstr) : integer;
var i : integer;
begin
for i:=1 to _spc do
    if (spec=spc[i].spec) then
       begin no_spc:=i; exit; end;
no_spc:=0;
end;


// --- checks whether the species acts in selected mechanism
function is_usedspec(cs : string) : boolean;
var s, r : word;
begin

for s:=1 to _eqs do
    with eqs[s] do
     begin

     if ((cs=educ[1]) or (cs=educ[2])) then
	begin
	is_usedspec:=true;
	exit;
	end;

     for r:=1 to _prod do
	 if (cs=prod[r]) then
	    begin
	    is_usedspec:=true;
	    exit;
	    end;
     end;

is_usedspec:=false;
end;

// --- check whether certain reaction has a KIE
function is_kie(abbr : nstr) : boolean;
var i : word;
begin
is_kie:=false;
for i:=1 to _kie do
    if (kie[i].imec) then
       if (abbr=kie[i].abbr) then
          begin is_kie:=true; break; end;
end;

// --- check whether species acts in KIE reaction(s)
function in_kie(spec : nstr) : boolean;
var i, j : word;
begin
in_kie:=false;
for j:=1 to _kie do
    if (kie[j].imec) then
       with eqs[kie[j].eqno] do
	    begin
	    for i:=1 to 2 do
		if (in_tsl(educ[i]) and (educ[i]=spec)) then
                   begin in_kie:=true; exit; end;
	    for i:=1 to _prod do
		if (in_tsl(prod[i]) and (prod[i]=spec)) then
		   begin in_kie:=true; exit; end;
	    end;
end;

// function substitutes _to instead of _what in _in and gives back the result
function substr(_in, _what, _to : string) : string;
var h, l : byte;
begin
if (_in='') or (_to='') then exit;
h:=pos(_what,_in);
while (h>0) do
      begin
      if (_in[h-1]<>'\') then         // here '\' is a canceller for control symbols!
         begin
         delete(_in,h,length(_what));
         insert(_to,_in,h);
         inc(h,length(_to));          // advancing cursor to search in the rest of the string
         end
      else
          begin
          delete(_in,h-1,1);          // removing control char and advancing the cursor
          inc(h);
          end;
      l:=pos(_what,copy(_in,h+1,length(_in)-h));    // seaching for a new instance in the rest of str
      if (l=0) then
         h:=0
      else
          h:=h+l;
      end;
substr:=_in;
end;

// --- la fonction, qui de'finit la condition de la re'action
// --- bool function determines criteria of the equation string recognition
function imcom_is_eqn(s : string) : boolean;
begin
imcom_is_eqn:=false;
if not(pos('//',trim(s))=1) then       // filtering commented lines
   if not(pos('!',trim(s))=1) then
      if (pos('<',s)<pos('>',s)) then  // some distinct markers
         if (pos('{%',s)>0) then          
            imcom_is_eqn:=true;
end;

// --- bool function determines criteria of the species string recognition
function imcom_is_spc(s : string) : boolean;
begin
imcom_is_spc:=false;
if not(pos('{',trim(s))=1) then        // filtering commented lines
   if not(pos('#',trim(s))=1) then     //   and inline keys
      if (pos('=',s)<pos(';',s)) then  // some distinct markers
         if (pos('{@',s)>0) then          
            imcom_is_spc:=true;
end;



// --- reading and parsing equation file + checks for TSL given in CFG
procedure imcom_read_spc(fname : string);
var i, j, k  :  integer;
    a, s, siecntsl : ansistring;
    err : boolean;
    f : text;
begin

writeln('species file: ',fname);  // a ext

// error flag
err:=false;

imcom_check_files_exist([fname]);

// initializing
fillchar(spcfile,sizeof(eqnfile),0);
fillchar(spc,sizeof(spc),0);
_spc:=0; _spcfile:=0;

// species of interesting element in composition, but not in tsl
siecntsl:='';

// reading lines, searching for TSLs, plus pot. missed species
assign(f,fname);
reset(f);
while not(eof(f)) do
      begin
      readln(f,s);

      inc(_spcfile);
      spcfile[_spcfile].line:=s;

      if imcom_is_spc(s) then
	 begin

	 inc(_spc);
	 spcfile[_spcfile].ispc:=true;
	 spcfile[_spcfile].nspc:=_spc;

// - processing species:
	 with spc[_spc] do

	      begin

              // getting caption
	      capt:=imcom_ext4marks(s,'{@','}');

	      // couperons les commentaires dans les {}
              // cutting comments in curlies
              while (pos('{',s)>0) do
                    delete(s,pos('{',s),max(0,pos('}',s)-pos('{',s))+1);

              spec:=trim(copy(s,1,pos('=',s)-1));

              // getting composition
              s:=imcom_ext4marks(s,'=',';');
              comp:=s;
              s:=delchars(s,' ')+'+';            // a marker for the last element detection
              
              mass:=0.0;

              while (s<>'') do
                    begin
                    
                    // getting the qty number before the element
                    a:='';
                    while (s[1] in ['0'..'9']) do
                          begin
                          a+=s[1];
                          delete(s,1,1);
                          end;
                    j:=strtointdef(a,1);  // supposing no number = 1

                    // getting the element
                    a:='';
                    while not(s[1]='+') do
                          begin
                          a+=s[1];
                          delete(s,1,1);
                          end;
                    delete(s,1,1);

                    // calculating mass
                    err:=true;
                    for i:=1 to _elems do
                        if (upcase(a)=upcase(elems[i].name)) then
                           begin
                           mass:=mass+j*elems[i].mass;
                           err:=false;
                           // checking if the species has the interesting element, but not in TSL
                           if not(in_tsl(spec)) then
                              if (upcase(elems[i].name)=upcase(isoatom)) then
                                 siecntsl+=spec+' ';
                           end;
                    
                    if (err) then
                       writeln('warning: for species '+spec+' element '+a+' is not known. this may affect molecule mass, => RMA <!>');      // imcom_error

                    // composing isotopic substitutions
                    for i:=1 to _isos do
                        begin
                        icomp[i]+=' + ';

                        if (upcase(a)=upcase(isoatom)) then 
                           begin
                           qatm:=j;                            // storing number of isotope atoms (for checking later)
                           
                           if (j>1) then                          // multi-atom substitution
                              if (i>1) then
                                 begin
                                 icomp[i]+=clsname[i]+' + ';      // mixture of minor and major(s)
                                 if ((j-1)>1) then icomp[i]+=inttostr(j-1);
                                 icomp[i]+=clsname[1];
                                 end
                              else
                                  begin
                                  if (j>1) then icomp[i]+=inttostr(j);  // only major(s)
                                  icomp[i]+=clsname[1];
                                  end
                           else                                   // one-atom molecule substitution
                               icomp[i]+=clsname[i];
                               
                           // constructing caption: atom with superscripted class
{                          icapt[i]:='^<'+clsname[i]+'>'; }
                           end
                        else
                            begin                                 // not a substituted atom
                            if (j>1) then icomp[i]+=inttostr(j);   // atom count
                            icomp[i]+=a;
                            end;

                        // adding atom to the composition
{                        icapt[i]+=upcase(a);
                        if (j>1) then
                           icapt[i]+='_<'+inttostr(j)+'>';}
                        
                        end;
                     
                    // linking spc record to the TSL record
                    tslno:=no_tsl(spec);
                    if (tslno>0) then
                       begin
                       tsl[tslno].nspc:=_spc;
                       tsl[tslno].mass:=mass;
                       end;
                                        
                    end;                    
              
              for i:=1 to _isos do
                  begin
                  delete(icomp[i],1,3);          // cut first " + "
                  icapt[i]:=capt; 
                  // trying to supstitute isotope
                  insert('^<'+clsname[i]+'>',icapt[i],max(pos(isoatom,capt),1));
                  // mecca spc-style caption brace
                  icapt[i]:='{@'+icapt[i]+'}';
                  end;
              
              end;
         end;

      end; // not(eof)

close(f);

// un peu d'info
// a bit of information
writeln(' #',_spc,' species found');
writeln(' #',_tsl,' total in TSL');
writeln;
if (siecntsl<>'') then
   writeln(' warning: following species contain ',isoatom,' element, but not found in TSL: ',siecntsl,_LF);

// atom quantities checks
siecntsl:='';
for i:=1 to _tsl do
    if not(tsl[i].qatm>0) then  // assigning atoms count from SPC if not defined before through cfg
       begin
       tsl[i].qatm:=spc[tsl[i].nspc].qatm;
       siecntsl+=tsl[i].spec+'('+inttostr(tsl[i].qatm)+isoatom+') ';
       end;
if (siecntsl<>'') then
   writeln(' species that have atomic content picked from SPC: ',siecntsl,_LF);

siecntsl:='';
for i:=1 to _tsl do
    if not(tsl[i].qatm>0) then  // assigning atoms count = 1 for species not found both in SPC and TSL
       begin
       tsl[i].qatm:=1;
       spc[tsl[i].nspc].qatm:=1;
       siecntsl+=tsl[i].spec+' ';
       end;
if (siecntsl<>'') then
   writeln(' species with atomic content assigned to unity (not found in TSL/SPC): ',siecntsl,_LF);

siecntsl:='';
for i:=1 to _tsl do
    if (spc[tsl[i].nspc].qatm<>tsl[i].qatm) then
       siecntsl+=tsl[i].spec+' ('+inttostr(tsl[i].qatm)+'/'+inttostr(spc[tsl[i].nspc].qatm)+') ';
if (siecntsl<>'') then
   writeln(' warning: following species have different ',isoatom,' content [using from TSL] (TSL/SPC) : ',siecntsl,_LF);

// masses check
siecntsl:='';
for i:=1 to _spc do
    if ((spc[i].mass < 1.0) or (spc[i].mass > 500.0)) then
       siecntsl+=spc[i].spec+' ('+floattostr(spc[i].mass)+') ';
if (siecntsl<>'') then
  writeln(' following species have strange mass [may be normal due to IGNORE]: ',siecntsl,' ');
   
writeln;
writeln('read_spc: done');

writebreak;

end;

// --- lire et intepre'ter le fichier des equations
// --- reading and parsing equation file
procedure imcom_read_eqs(fname : string);

var i, j, k, l : word;
    qatm_educ, qatm_prod : real; // q-ty of atoms on both sides
    r, rm0, rmS : real;
    s, a, rej_dbl_eqs : ansistring;
    f : text;
    psign : integer;
    ex_prec, ex_prod : boolean;  // tagged-species exist on left/right side flags
    err, dbl, fss : boolean; 	 // error flag, doubled reaction flag, source specification flag

begin

//writeln('fichier des equations: ',fname);  // a ext
writeln('equations file: ',fname);  // a ext

// error flag
err:=false;

imcom_check_files_exist([fname]);

// lisons de l'info des reactions
// reading reactions info
assign(f,fname);
reset(f);

fillchar(eqnfile,sizeof(eqnfile),0);
fillchar(eqs,sizeof(eqs),0);
_eqs:=0; _eqnfile:=0; nooftagreac:=0;

// reading eqn file
while not(eof(f)) do
      begin
      readln(f,s);

      inc(_eqnfile);
      eqnfile[_eqnfile].line:=s;

      // if there were equations in cfg to include, adding them to the top (foobar kpp)
      if (pos('#EQUATIONS',upcase(s))>0) then
         if (_addeqs>0) then
            begin
            inc(_eqnfile);
            eqnfile[_eqnfile].line:='{------ [ '+tagname+' ] - additional reactions ----------------------------------}';

            for i:=1 to _addeqs do
                begin
                inc(_eqnfile);
                eqnfile[_eqnfile].line:=addeqs[i];
                end;
            end;             
      end;
// closing eqn file
close(f);
      
// parsing line-by-line
for l:=1 to _eqnfile do
    begin
    s:=eqnfile[l].line;
    if imcom_is_eqn(s) then
       begin

       inc(_eqs);
       eqnfile[l].iseq:=true;
       eqnfile[l].eqno:=_eqs;

// - processing equation:
       with eqs[_eqs] do

            begin

            abbr:=imcom_ext4marks(s,'<','>');
            phys:=copy(s,pos(':',s)+1,length(s)-pos(s,':'));

            // "ever tagged" number: stored in the intermediate and final eqn by imdouble
            etag:=strtointdef(imcom_ext4marks(s,'{'+_etagspsy,'}'),0);

            // couperons les commentaires dans les {}
            // cutting comments in curlies
            while (pos('{',s)>0) do
                  delete(s,pos('{',s),max(0,pos('}',s)-pos('{',s))+1);

            // identifying educt(s)
            if ((pos('+',s)<pos('=',s)) and (pos('+',s)>0)) then
               begin
	       educ[1]:=trim(imcom_ext4marks(s,'>','+'));
	       educ[2]:=trim(imcom_ext4marks(s,'+','='));
               _educ:=2; 
	       end
            else
	        begin
		educ[1]:=trim(imcom_ext4marks(s,'>','='));
		educ[2]:='';
                _educ:=1; 
		end;

	    // couperons les co^te's gaushe
	    // cutting left side
            delete(s,1,pos('=',s));
	    // et droit
            // and right
	    delete(s,pos(':',s),length(s)-pos(':',s)+1);

	    s:=trim(s);
            if (length(s)>0) then
               if not(s[1] in ['+','-']) then s:='+'+s;  // adding a default '+' to the first product

            //writeln('}',s);

            i:=1;
	    _prod:=0;
            fillchar(prod,sizeof(prod),0);
	    while (i<length(s)) do
		  begin
		  inc(i);
                  case s[i-1] of      // last passed "+" or "-" belongs to the current product
                       '+' : psign:=1;
                       '-' : psign:=-1;
                  end; {case}

                  if (not(s[i] in ['+','-'])) then
                     prod[_prod+1]+=s[i];

                  if ((s[i] in ['+','-']) or (i=length(s))) then
                     begin
                     inc(_prod);
                     delete(s,1,length(prod[_prod])+1); s:=trim(s);
                     prod[_prod]:=trim(prod[_prod]);
                     //writeln('>',prod[_prod]);
                     //writeln(']',s);
      
                     // if the product has a stoi coeff
                     if (pos(' ',prod[_prod])>0) then
                        begin
                        stoi[_prod]:=strtofloat(copy(prod[_prod],1,pos(' ',prod[_prod])-1));
                        delete(prod[_prod],1,pos(' ',prod[_prod]));
                        end
                     else
                         stoi[_prod]:=1;

                     stoi[_prod]:=stoi[_prod]*psign;
                     i:=1;
                     end;
      
                   end;
      
	    // la specification
            // source specification information
	    eqs[_eqs].isrc:=false;
	    for j:=1 to _src do
		  if (src[j].abbr=eqs[_eqs].abbr) then
		     begin
		     // flag as "has a source specification"
		     eqs[_eqs].isrc:=true;
		     eqs[_eqs].nsrc:=j;
		     src[j].eqno:=_eqs;
		     end;

	    itag:=false;	// determined further
	    ntag:=0;
            
            iiex:=false;      // not an isotope exchange r-n
            niex:=0;

	    end;

// - EO initial processing

       end
    else
        eqnfile[l].iseq:=false;
    end;

// additional processing
// + un peu d'info:


rej_dbl_eqs:='';

writeln('#',_eqs,' equations (showing only those containing species to tag):');
for i:=1 to _eqs do
    with eqs[i] do
	 begin

         // checking if the reaction in the eqn is already doubled
	 dbl:=false;
	 for j:=1 to _eqs do   // if among all of eqs. tags found one that matches to current within drsyntax => doubled
	     if (pos( substr(drsyntax,'@',eqs[j].abbr), eqs[i].abbr )>0 ) then
	        begin
		dbl:=true;
        	rej_dbl_eqs:=rej_dbl_eqs+abbr+' ';
		break;
		end;
	 if (dbl) then continue;

	 ex_prec:=false; ex_prod:=false;
	 qatm_educ:=0; qatm_prod:=0;

	 s:=format('%6s',[abbr])+': '; // write(abbr,': ');

	 if (in_tsl(educ[1])) then
	    begin
	    s:=s+educ[1]+' '; //write(educ[1],' ');
	    ex_prec:=true;
	    qatm_educ+=tsl[no_tsl(educ[1])].qatm;
	    end;
	 if (in_tsl(educ[2])) then
	    begin
	    s:=s+educ[2]+' '; //write(educ[2],' ');
	    ex_prec:=true;
	    qatm_educ+=tsl[no_tsl(educ[2])].qatm;
	    end;

	 s:=s+'-> '; //write('-> ');

	 for j:=1 to _prod do
	     if (in_tsl(prod[j])) then
		begin
		if stoi[j]<>1 then s:=s+floattostr(stoi[j])+'*'; // write(floattostr(stoi[j]),'*');
		s:=s+prod[j]+' '; //write(prod[j],' ');
		ex_prod:=true;
		qatm_prod:=qatm_prod+stoi[j]*tsl[no_tsl(prod[j])].qatm;
		end;

	 // si l'equation n'a pas les molecules marques - passons
	 if (not(ex_prec) and not(ex_prod)) then
	    continue;

         // reaction is tagged:

         itag:=true;	      // set "is tagged" flag
         etag:=etag+1;        // set "is ever tagged" flag, increase tot. number of tagging times
                              //  (relies on the hope that the PT abbreviation is equal for different configurations!)
	 inc(nooftagreac);
	 ntag:=nooftagreac;
	
	 if2t:=(in_tsl(educ[1]) and in_tsl(educ[2]));  // both educts are tagged
	 if2s:=(educ[1]=educ[2]);                      // self-reacting educts

         // checking budgeting
         for j:=1 to 2 do
             if (in_bsl(educ[j])) then bsl[no_bsl(educ[j])].iloss:=true;
	 for j:=1 to _prod do
	     if (in_bsl(prod[j])) then bsl[no_bsl(prod[j])].iprod:=true;

	 // output reaction info
	 write('#',ntag:3,'/',i:3,' @',etag:1,': ',format('%-40s',[s]));

	 // issue a warning if amount of consumed atoms is not equal to amount of produced
	 if (abs(qatm_educ-qatm_prod)>1E-05) then
	    write(' // WARN: ',isoatom,' atom flow is not conservative: ',
	          formatfloat('##0.###',qatm_educ), 
                  '->',
                  formatfloat('##0.###',qatm_prod));

	 // issue a warning if a species comes from/to nothing
	 if (ex_prec=not(ex_prod)) then
	    if (ex_prec) then
	       write(' // WARN: no product info (may be just a destruction)')
	    else
		if not(eqs[i].isrc) then    // without the source specification? o-la-la
                   begin
{$IFNDEF IGNORE_NOSRC}
	           writeln(' // <!> ERR: no educt info (no source specification) -- cannot tag this reaction');
                   eqs[i].itag:=false; continue;
//		   err:=true;
{$ENDIF}
		   end
                else
                    write(' // WARN: no educt info from eqn (though specified in the configuration)');
                
         // checking specification
	 if (eqs[i].isrc) then
            begin
            s:='';
            if (src[eqs[i].nsrc].def=0) then            // if there is no default source specified,
               for j:=1 to _prod do                     // checking all species to have a source
                   if (in_tsl(prod[j])) then
                      begin
                      fss:=false;
                      for k:=1 to src[eqs[i].nsrc]._trans do
                          if ( no_trans_dst(eqs[i].nsrc,k,prod[j])>0 ) then
                              begin fss:=true; break; end;
                      if not(fss) then s+=prod[j]+' ';
                      end
                   else
            else                    // creating links default -> for the species without specified source
                with src[eqs[i].nsrc] do
                     begin
                     for j:=1 to _prod do                      // checking all species
                         if (in_tsl(prod[j])) then
                            begin
                            fss:=false;
                            for k:=1 to _trans-1 do            // -1 since we've created a new source
                                if ( no_trans_dst(eqs[i].nsrc,k,prod[j])>0 ) then
                                   begin fss:=true; break; end;
                            if not(fss) then 
                               with trans[src[eqs[i].nsrc].def] do
                                    begin
                                    inc(_dst);
                                    dst[_dst].spec:=prod[j];
                                    dst[_dst].stoi:=1.0;       // so far, corrected further if necessary
                                    end;
                            end;
                     end;
            if (s<>'') then
               begin
               write(' // <!> ERR: no educt info for '+s+'(check eqn and configuration sources specification) -- reaction may be tagged improperly');
               //eqs[i].itag:=false; continue;
               //err:=true;
               end
            else
                // checking "isotopic stoichiometry"
                with src[eqs[i].nsrc] do
                     begin
                     
                     fss:=true;
                     while (fss) do
                           begin
 
                           // finding destination species not yet checked
                           fss:=false;
                           for j:=1 to _trans do
                               begin
                               for k:=1 to trans[j]._dst do
                                   if ( pos(trans[j].dst[k].spec,s)=0 ) then
                                      begin
                                      a:=trans[j].dst[k].spec;
                                      fss:=true;
                                      break;
                                      end;
                               if (fss) then break;
                               end;
                           
                           if (fss) then
                              begin
                              r:=0; // qty. of molecules produced for a current destination (a)
                              for j:=1 to _trans do
                                  if ( no_trans_dst(eqs[i].nsrc,j,a)>0 ) then
                                     r:=r+trans[j].dst[no_trans_dst(eqs[i].nsrc,j,a)].stoi;
 
                              // weighting excess to unity
                              if (r>1.0) then
                                 for j:=1 to _trans do
                                     if ( no_trans_dst(eqs[i].nsrc,j,a)>0 ) then
                                        with trans[j].dst[no_trans_dst(eqs[i].nsrc,j,a)] do
                                             stoi:=stoi/r;
 
                              s+=a; // remember processed spec
                              end;
                           
                           end;
 
                     s:='';          
                     end;
            
            end;

	 writeln;
	 end;
j:=0; for i:=1 to _eqs do if (eqs[i].itag) then inc(j);
writeln('number of equations to tag: ',j);

{$IFDEF USE_DKRATE}
// is rate optimisatino is used, bubblesort comparison & correction of eqs. abbr (they need to be unique)
l:=0; s:='';
for i:=1 to _eqs-1 do
    begin
    
    // most time-taking part, showing runner
//    if ((i+j) mod (p div 32))=0 then 
  //     begin write(runner[(l mod length(runner))+1],#8); inc(l); end;

    for j:=i+1 to _eqs do
        if (eqs[j].itag and eqs[i].itag) then
           if (upcase(eqs[j].abbr) = upcase(eqs[i].abbr)) then
              begin
              inc(l);
              eqs[j].abbr:=eqs[j].abbr+'_dbl_'+inttostr(j);
              s+=eqs[j].abbr+' ';
              end;
    end;
write('corrected non-unique <abbr> reactions: ');
if (l=0) then writeln('none')
         else writeln('(',l,') '+s);
{$ENDIF}

writeln;

// isotope exchange reactions in the current mech
write('isotope exchange reactions (as for selected mechanism): ');
s:='';
// finding th last eq, -> l
l:=1; while (not(eqnfile[l].eqno=_eqs) and (l<=_eqnfile)) do inc(l); inc(l);
for i:=1 to _iex do
    with iex[i] do
         if (is_usedspec(tsl[exspec[1]].spec) and is_usedspec(tsl[exspec[2]].spec)) then
            begin
            // inserting a new line to the reactions list
            for j:=_eqnfile downto l do
                eqnfile[j+1]:=eqnfile[j];
            eqnfile[l].line:='';
            inc(_eqnfile);
            
            // adding new reaction
            inc(_eqs);
            eqs[_eqs].abbr:=abbr;
            eqs[_eqs].educ[1]:=tsl[exspec[1]].spec;
            eqs[_eqs].educ[2]:=tsl[exspec[2]].spec;
            eqs[_eqs]._prod:=2;
            eqs[_eqs].prod[1]:=tsl[exspec[2]].spec;
            eqs[_eqs].prod[2]:=tsl[exspec[1]].spec;
            eqs[_eqs].stoi[1]:=1.0;
            eqs[_eqs].stoi[2]:=1.0;
            
            eqs[_eqs].phys:=phys;
            
            eqs[_eqs].isrc:=false;
            eqs[_eqs].nsrc:=0;
            
            eqs[_eqs].iiex:=true;
            eqs[_eqs].niex:=i;
            
            inc(nooftagreac);
            eqs[_eqs].itag:=true;
            eqs[_eqs].etag:=1;    // since the IEX appears once (expected), we do not include it in the dbl_k* stuff
            eqs[_eqs].ntag:=nooftagreac;
            
            // reactions file link
            eqnfile[l].line:='{ isotope exchange r-n '+abbr+' is to appear here }';
            eqnfile[l].iseq:=true;
            eqnfile[l].eqno:=_eqs;
            
            s+=abbr+' ';
            imec:=true;
            
            inc(l); // prepare for the next one
            end;
if (s='') then s:='none';
writeln(s);
writeln;

// making IESL
_iesl:=0; s:=''; a:='';
for i:=1 to _iex do
    if (iex[i].imec) then
       for j:=1 to 2 do
           if (pos('>'+tsl[iex[i].exspec[j]].spec+'<',s)=0) then
              begin
              inc(_iesl);
              iesl[_iesl]:=iex[i].exspec[j];
              s+='>'+tsl[iex[i].exspec[j]].spec+'<';
              a+=tsl[iex[i].exspec[j]].spec+' ';
              end;
writeln('species experiencing isotopic exchange in selected mechanism (#',_iesl,'): ',a);
writeln;

// source specification applied to the current reactions list
writeln('source specification (as for selected mechanism):');
for i:=1 to _eqs do
    if eqs[i].isrc then
       with src[eqs[i].nsrc] do
            begin
            write(abbr:6,': ');

            for j:=1 to _trans do
                begin
                if (trans[j].weight<>1.0) then
                    write(formatfloat('##0.#####',trans[j].weight)+' ');
                write(trans[j].src,' ->');
                for k:=1 to trans[j]._dst do
                    begin
                    write(' ');
                    if (trans[j].dst[k].stoi<>1.0) then
                       write(formatfloat('##0.#####',trans[j].dst[k].stoi)+' ');
                    write(trans[j].dst[k].spec);
                    end;
                if (j<_trans) then write(', ');
                end;
            writeln;
            end;
writeln;

// rejected "doubled" reactions list
if (length(rej_dbl_eqs)>0) then
   writeln('rejected "doubled" equations: ',rej_dbl_eqs,_LF);

// making UTSL
_utsl:=0; s:='';
for i:=1 to _tsl do
    if (is_usedspec(tsl[i].spec)) then
       begin
       inc(_utsl);
       utsl[_utsl]:=i;
       s:=s+tsl[i].spec+' ';
       end;
writeln('species to tag within selected mechanism (#',_utsl,'): ',s);
writeln;

// indicating KIEs which are present in the current mechanism
s:='';
// regular reactions
for k:=1 to _kie do
    for i:=1 to _eqs do
      if (eqs[i].itag) then                                          // checking only eqs. to tag
        if (kie[k].abbr=eqs[i].abbr) then                            // matching reaction?
           begin
	   for l:=1 to 2 do                                          // checking regular educts
               for j:=1 to _isos do
	           if (kie[k].isot=tsl[no_tsl(eqs[i].educ[l])].isos[j]) then   // matching isotopologue?
                      begin
                      kie[k].eqno:=i;
                      kie[k].imec:=true;
                      s:=s+eqs[i].abbr+'['+kie[k].isot+'] ';
                      end;
           if (eqs[i].isrc) then with src[eqs[i].nsrc] do            // checking source substitution
              for l:=1 to _trans do                                  // e cycles through source specification
                  if (trans[l]._dst>0) then                          // avoiding sources without destination
                     for j:=1 to _isos do
                         if (kie[k].isot=tsl[no_tsl(trans[l].src)].isos[j]) then
                            begin
                            kie[k].eqno:=i;
                            kie[k].imec:=true;
                            s:=s+eqs[i].abbr+'['+kie[k].isot+'] ';
                            end;
           end;

writeln('reactions with KIE within selected mechanism: ',s);
// checking reactions falling into the RMA selection
write('reactions with RMA KIE assigned: ');
for i:=1 to _eqs do
  if (eqs[i].itag) then                                               // checking only eqs. to tag
    for j:=1 to rmakie._include do
        if (iswild(eqs[i].abbr, rmakie.include[j], true)) then        // in case r-n abbrev. suits the wildcard given in RMA-include
           begin
           dbl:=false;                                                // using this var as temporary boolean flag
           for k:=1 to rmakie._exclude do
               if (iswild(eqs[i].abbr, rmakie.exclude[k], true)) then // checking if r-n abbrev. in the exclude-list
                  dbl:=true;
           for k:=1 to _kie do
               if (kie[k].abbr=eqs[i].abbr) then                      // anyway excluding reactions with already specified KIEs
                  dbl:=true;
           if not(dbl) then                                           // calculating and adding RMA KIE for the reaction
              begin
              write(eqs[i].abbr,' ');
              
              // adding RMA KIE to the regular kie list, he-he
              
              for l:=1 to 2 do
                  if (in_tsl(eqs[i].educ[l])) and                     // is an active spec?
                     not((eqs[i].educ[1]=eqs[i].educ[2]) and (l=2)) then   // avoiding doubling
                     for k:=2 to _isos do
                         begin
                         
                         inc(_kie);
                         with kie[_kie] do
                              begin
                              abbr:=eqs[i].abbr;
                              eqno:=i;
                              imec:=true;
                              isot:=tsl[no_tsl(eqs[i].educ[l])].isos[k];
                              
                              // checking division by 0
                              if ( ((spc[no_spc(eqs[i].educ[l])].mass)  *maxr(1,spc[no_spc(eqs[i].educ[3-l])].mass))<1E-100 ) or
                                 ( ((spc[no_spc(eqs[i].educ[l])].mass+r)*maxr(1,spc[no_spc(eqs[i].educ[3-l])].mass))<1E-100 ) then
                                 begin
                                 writeln('-> warn: RMA KIE calculation for ',eqs[i].educ[l],'+',eqs[i].educ[3-l],
                                         ' (',tsl[no_tsl(eqs[i].educ[l])].isos[k],') most likely causes division by zero (calculating rm0/rmS); check species and cfg');
                                 rmS:=1; rm0:=1;
                                 end
                              else
                                  // calculating KIE using reduced masses (regular and substituted)
                                  begin
                                  rm0:=((spc[no_spc(eqs[i].educ[l])].mass)  +maxr(1,spc[no_spc(eqs[i].educ[3-l])].mass))/
                                       ((spc[no_spc(eqs[i].educ[l])].mass)  *maxr(1,spc[no_spc(eqs[i].educ[3-l])].mass));
                                  r:=-isomass[1]+isomass[k];
                                  rmS:=((spc[no_spc(eqs[i].educ[l])].mass+r)+maxr(1,spc[no_spc(eqs[i].educ[3-l])].mass))/
                                       ((spc[no_spc(eqs[i].educ[l])].mass+r)*maxr(1,spc[no_spc(eqs[i].educ[3-l])].mass));
                                  if ( rm0 < 1E-100 ) then
                                     begin
                                     writeln('-> warn: RMA KIE calculation for ',eqs[i].educ[l],'+',eqs[i].educ[3-l],
                                         ' (',tsl[no_tsl(eqs[i].educ[l])].isos[k],') most likely causes division by zero (rm0 = 0); check species and cfg');
                                     rmS:=1; rm0:=1;
                                     end;
                                  end;
                              expr:='*'+floattostr(sqrt(rmS/rm0));
                              end;
                         end;

              end;
           end;
writeln;
writeln;


if (err) then
   imcom_error('read_eqs: errors detected, check the output and reactions file')
else
    writeln('read_eqs: done');

writebreak;

end;



// --- make_addtracdef --------------------------------------------------------
// creates additional tracers definition file to link with MESSy
procedure imcom_make_addtracdef(origTDname, addTDname : string);

// this function substitutes a certain column value with another
// style is similar to tracdef.tex, i.e. 
// 'TRACER'  & col#2    & col#3  & etc.
// = column separators are '&' and '\'
function sub_column(src : string; colno : integer; _repl : string) : string;
var p, n, w : integer;
    s : string;
begin
s:=src;
// searching for a column and getting its width
p:=0; w:=0; n:=0;
while ((p<length(src)) and (n<colno)) do
      begin
      inc(p);                      // points to the end of col.
      inc(w);                      // curr. col width
      if (s[p] in ['&','\']) then
         begin
         inc(n);                   // passed column no.
         if (n<colno) then w:=0;   // resetting column width
         end;
      end;
if (n=colno) then                  // we found the col.!
   begin
   dec(w); dec(p,w);
   delete(s,p,w);                  // removing old cont., putting new
   insert(format('%-'+inttostr(w)+'s',[_repl]),s,p);
   end;
sub_column:=s;
end;

var f : text;
    tdfile : array[1..max_spc*2] of string;    // careful here with long strings in *.tex !!!
   _tdfile : integer;                          // # of lines in the file
    i, j, k : integer;
    otrac : nstr;
    s, ssave : string;

const rmolarmasscol = 17;                      // # of the column for the Rmolarmass
      mediumcol = 3;                           //    ------- " -------    media
      vinicol = 4;                             //    ------- " -------    vini
      totalcol = 24;                           // total # of the columns in tex
      off_col_beg = 6; off_col_end = 12;       // columns to put OFF value for totals

begin

write('imcom_make_addtracdef(',origTDname,',',addTDname,'): ');

// reading original file to get an example from
imcom_check_files_exist([origTDname]);
assign(f,origTDname); reset(f);
_tdfile:=0; fillchar(tdfile,sizeof(tdfile),0);
while not(eof(f)) do
      begin
      inc(_tdfile);
      readln(f,tdfile[_tdfile]);
      end;
close(f);

{$IFDEF ADDTRACDEF_H2O}
// adding 'fake' water tracer to create doubled tracers
if (in_tsl('H2O')) then
   begin
   inc(_tdfile);
//                     %--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//                     basename      & sub  & medium  & vini       & I_force& I_ad  & I_con & I_vdi  & I_wet& I_dry& I_sedi& I_scav & I_mix& I_force& I_inte& I_time& R_molarmass         & R_henry   &R_dryr& I_       S_     I_     R_        I_    \\
//                                   & name &         &            & _init  & vect  & vect  & ff     & dep  & dep  &       &        &      & _col   & grate & filter&                     &           &eac_sf& method & model& mode & density & sol   \\
//                     %-H2O----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
   tdfile[_tdfile]:='''H2O''         &      & AIR     &            &        & OFF   & OFF   & OFF    &      &      &       & OFF    &      &        & ON    &       & MH*2._dp+MO         &           &      &        &      &      &         &       \\'
   end;
{$ENDIF}

// opening output file handle
assign(f,addTDname); rewrite(f);

writeln(f,'% this file contains additional tracer definitions created by imtag/double');
writeln(f,'% configuration: ',cfgname);
writeln(f,'% created: ',datetimetostr(now));
writeln(f,'%');

// copying first 4 lines (supp. to be the table header)
for i:=1 to min(4,_tdfile) do
    writeln(f,tdfile[i]);

ssave:='';

// processing sequence: finding a spec, if it is used in the tagging, adding new tracers according to the classes
for i:=1 to _tdfile do
    if (pos('%',tdfile[i])=0) then         // skipping commented lines
       begin

       // getting original tracer name
       otrac:=extractword(1,tdfile[i],_delims);
       while (pos('''',otrac)>0) do
             delete(otrac,pos('''',otrac),1);
       otrac:=trim(otrac);

       if (in_tsl(otrac) and is_usedspec(otrac)) then
          begin

          // saving an example of a tracer def. line
          ssave:=tdfile[i];

          // headers for the tagging tracers
          s:='%--'+otrac+'-';
          while (length(s)<length(tdfile[i])) do s+='-';
          writeln(f,s);

          // isotopologues
          for j:=1 to _isos do
              begin
              // tracer name
              s:=sub_column(tdfile[i], 1, ''''+tsl[no_tsl(otrac)].isos[j]+'''');
              // removing initial conc. value
              s:=sub_column(s, vinicol, ' ');
              // changing Rmolarmass (if isotope masses were specified)
              if (isomass[j]>0) then
                 s:=sub_column(s, Rmolarmasscol, ' '+floattostr( spc[tsl[no_tsl(otrac)].nspc].mass-isomass[1]+isomass[j] )+' ');
              writeln(f,s);
              end;
          end;
       
       // budgeting tracers (PTs)
       if (in_bsl(otrac)) then
          with bsl[no_bsl(otrac)] do
               for j:=1 to _isos do
               begin

               // loss PT tracer name
               s:=sub_column(tdfile[i], 1, ''''+substr(ptlsyntax,'@',clsname[j]+bsl[no_bsl(otrac)].spec)+'''');

               // removing all initial data (except the medium)
               for k:=vinicol to totalcol do
                   s:=sub_column(s, k, ' ');
               // keeping Rmolarmass
               // turning off all I_ parameters
               for k:=off_col_beg to off_col_end do
                   s:=sub_column(s, k, ' OFF');
       
               if (iloss) then
                  writeln(f,s);

               if (iprod) then
                  begin
                  // production PT tracer name
                  s:=sub_column(s, 1, ''''+substr(ptpsyntax,'@',clsname[j]+bsl[no_bsl(otrac)].spec)+'''');
                  writeln(f,s);
                  end;
              end;
       
       end;

// ----------- adding totals tracers --------------
s:='%--'+cfgname+' totals---';
while (length(s)<length(ssave)) do s+='-';
writeln(f,s);

// total {%ATOM} tracer: turning off all processes and assigning mass of the single isotope atom
if (ssave<>'') then
   begin

   // tracer name
   s:=sub_column(ssave, 1, ''''+cfgname+'T'+isoatom+'''');

   // removing all initial data (except the medium)
   for k:=vinicol to totalcol do
       s:=sub_column(s, k, ' ');

   // changing Rmolarmass    // maxr is to assure that there is no zero-mass
   s:=sub_column(s, Rmolarmasscol, ' '+floattostr( maxr(elems[-1].mass,elems[isoelem].mass) )+' ');

   // turning off all I_ parameters
   for k:=off_col_beg to off_col_end do
       s:=sub_column(s, k, ' OFF');
      
   writeln(f,s);
       
   // "isotopologues"
   for j:=1 to _isos do
       begin
        
       // tracer name
       s:=sub_column(ssave, 1, ''''+cfgname+'T'+clsname[j]+'''');

       // removing all initial data (except the medium)
       for k:=vinicol to totalcol do
           s:=sub_column(s, k, ' ');

       // changing Rmolarmass    // maxr is to assure that there is no zero-mass
       s:=sub_column(s, Rmolarmasscol, ' '+floattostr( maxr(elems[-1].mass,isomass[j]) )+' ');

       // turning off all I_ parameters
       for k:=off_col_beg to off_col_end do
           s:=sub_column(s, k, ' OFF');

       writeln(f,s);
       end;

   // production/loss PTs
   

   end
else
    writeln(' could not find any suitable tracer definition line. totals are not created. ');

// last row lined (cosmetic)
s:='%--';
while (length(s)<length(tdfile[1])) do s+='-';
writeln(f,s);

close(f);
writeln('done');

end;



// - util ---------------------------------------------------------------------

// -- function to wrap the lines, (un)conditionally by &
			   // auto &   conditional indents
function wrap(a : ansistring; aindent, cindent : word) : ansistring;
var s : ansistring;
    c, p : integer;
    w : boolean;
    lead, blank : string;
begin

s:='';
c:=1;
w:=false;

setlength(blank,aindent+cindent);
fillchar(blank[1],aindent+cindent,#32);
//a:=copy(blank,1,aindent)+trimleft(a);        // first line indent

while (c<length(a)) do
      begin

      p:=0;

      lead:=copy(blank,1,aindent);

      while ((c<length(a)) and not(w)) do
	    begin
	    if (a[c] in ['+','-',',']) then p:=c;       // linebreak allowed at "+" and ',' only
	    if ((c>=wrapmargin) and (p>0)) then w:=true;
	    // unconditional wrapping at ampersand
	    if (a[c]='&') then
	       begin
	       delete(a,c,1);
	       insert(' ',a,c);
	       p:=c;
	       w:=true;
	       lead:=copy(blank,1,cindent);
	       end;
	    inc(c);
	    end;

      if not(c<length(a)) then
	 begin
	 s:=s+a;
	 break;
	 end;

      c:=1; w:=false;

      s:=s+copy(a,1,p)+' &'+_LF;
      delete(a,1,p);
      a:=trimleft(a);
      while (a[1]='&') do
            delete(a,1,1);     // avoid exorbitant ampersands from code generator
      insert(lead,a,1);
      end;

wrap:=s;

end;


// - merged subroutines --------------------------------------------------------


// imcom_condition -------------------------------------------------------------
// returns a certain condition comparison result
function imcom_condition(c, v : string) : boolean;
var z, n : boolean;
    i : integer;
begin

z:=false;

if c[1]='!' then                                    // negative condition
   begin n:=true; delete(c,1,1); end
else n:=false;

c:=uppercase(c);
v:=trim(v);

if c='CASE' then {$IFDEF TAG} z:=(v='TAG') {$ENDIF}
                 {$IFDEF DBL} z:=(v='DBL') {$ENDIF} // whether we do tagging or doubling
   else
if c='ATOM' then z:=iswild(isoatom,v,true) else     // atom is
if c='CONF' then z:=iswild(cfgname,v,true) else    // configuration is ...
if c='TAG'  then z:=iswild(tagname,v,true) else     // tagging name
if c='ISO'  then z:=(_isos=strtointdef(v,-1)) else  // number of classes is equal to
if c='KIE'  then z:=(_kie>0) else                   // KIE present in the system
if c='SRC'  then z:=(_src>0) else                   // specification of sources is present
if c='REAC' then
   begin
   for i:=1 to _eqs do
       if eqs[i].itag then
          if iswild(eqs[i].abbr,v,true) then
	     z:=true;
   end else
if c='SPEC' then z:=is_usedspec(v) else             // if in the used species list
   begin
   writeln(' warning: imcom_condition( param: ',c,', value: ',v,' ) is not recognized in former');
   z:=false;
   end;

if (n) then z:=not(z);
imcom_condition:=z;

end;



// imcom_tagspecs (tagged species list) ----------------------------------------
// outputs the tagged species names list according to specimen with @, #/$ (class #/name)
function imcom_make_tagspecs_list(pref : string) : ansistring;
var i, k : word;
    a : ansistring;
begin

a:='       ';
// tracers names, no cycling throug classes
if ((pos('#',pref)+pos('$',pref))=0) then
   for i:=1 to _utsl do
       a:=a+''''+format('%-'+inttostr(sizeof(nstr)-1)+'s',[substr(pref,'@',tsl[utsl[i]].spec)])+''', '
else // or cycling, if marker #/$ is found
    for k:=1 to _isos do
        begin
        for i:=1 to _utsl do
            a:=a+''''+format('%-'+inttostr(sizeof(nstr)-1)+'s',
               [ substr(substr(substr(pref,'@',tsl[utsl[i]].spec),'#',inttostr(k)),'$',clsname[k]) ])+''', ';
        end;
setlength(a,length(a)-2);
a:=a+' &';

imcom_make_tagspecs_list:=wrap(a,7,7);
end;


// -----------------------------------------------------------------------------
// outputs an array of indices of species acting in reactions with KIE
// prefix: ind_d for doubling, ind_t for tagging
function imcom_make_kie_iex_relspecs(reftrac, tagtrac : string) : ansistring;
var i, j, k, r, s : word;
    a, b, c, out : string;
begin

out:='';

// KIE part

// making list of reactions with KIE in the mech
r:=0; s:=0; k:=0; a:=''; b:=''; c:='';
for i:=1 to _kie do
    if (kie[i].imec) then
       if (pos(kie[i].abbr,b)=0) then
          begin
          a+=substr(reftrac,'@',substr(ptsyntax,'@',kie[i].abbr))+', ';
          b+='>'+kie[i].abbr+'<:';
          inc(k);
          end;
setlength(a,length(a)-2); a:='    (/ '+a+' /)';  // cut last comma

if (r=0) then out+='! there are no KIEs in selected mechanism, empty arrays are for the compatibility'+_LF;
{$IFDEF USE_PT}
out+='! number of KIE-escorted reactions'+_LF;
out+='  INTEGER, PARAMETER :: NKRREAC = '+inttostr(k)+_LF;
if (r>0) then                                    // no KIE in the system = zero-sized array (for compatibility)
   out+='  INTEGER, PARAMETER :: KRREAC(NKRREAC) = &'+_LF+wrap(a,4,4)
else
    out+='  INTEGER, PARAMETER :: KRREAC(NKRREAC+1) = (/ NKRREAC /)';
out+=_LF;
{$ENDIF}
a:=''; s:=0;
// inspecting r-ns list for tagged species in KIE
for i:=1 to _eqs do
    with eqs[i] do
	 if (pos(abbr,b)>0) then   // if in KIE list
	    begin
	    for j:=1 to 2 do
		if (in_tsl(educ[j])) and (pos('>'+educ[j]+'<',c)=0) then
		   begin
		   a+=substr(tagtrac,'@',educ[j])+', ';
                   c+='>'+educ[j]+'<';
		   inc(s);
		   end;
	    for j:=1 to _prod do
		if (in_tsl(prod[j])) and (pos('>'+prod[j]+'<',c)=0) then
		   begin
		   a+=substr(tagtrac,'@',prod[j])+', ';
                   c+='>'+prod[j]+'<';
		   inc(s);
		   end;
	    end;

setlength(a,length(a)-2); a:='    (/ '+a+' /)'+_LF;  // cut last comma + in braces

out+='! indices of KIE-related species'+_LF;
out+='  INTEGER, PARAMETER :: NKRSPEC = '+inttostr(s)+_LF;
if (s>0) then		   // no KIE in the system = zero-sized array (for compatibility)
   out+='  INTEGER, PARAMETER :: KRSIND(NKRSPEC) = &'+_LF+wrap(a,4,4)
else
    out+='  INTEGER, PARAMETER :: KRSIND(NKRSPEC+1) = (/ NKRSPEC /)'+_LF;


out+=_LF;
// isotope exchange part

// counting active IEX in the mech
r:=0; s:=0; a:=''; b:=''; c:='';
for i:=1 to _iex do
    if (iex[i].imec) then
       begin
       b+=substr(reftrac,'@',substr(ptsyntax,'@',iex[i].abbr))+', ';
       inc(r);
       end;

for i:=1 to _iesl do
    a+=substr(tagtrac,'@',tsl[iesl[i]].spec)+', ';

setlength(a,length(a)-2); a:='    (/ '+a+' /)';  // cut last comma + in braces
setlength(b,length(b)-2); b:='    (/ '+b+' /)';

if (r=0) then out+='! there is no isotope exchange in selected mechanism, empty arrays are for the compatibility'+_LF;
{$IFDEF USE_PT}
out+='! number of isotope exchange reactions'+_LF;
out+='  INTEGER, PARAMETER :: NIXREAC = '+inttostr(r)+_LF;
if (r>0) then                                    // no KIE in the system = zero-sized array (for compatibility)
   out+='  INTEGER, PARAMETER :: IXREAC(NIXREAC) = &'+_LF+wrap(b,4,4)
else
    out+='  INTEGER, PARAMETER :: IXREAC(NIXREAC+1) = (/ NIXREAC /)';
out+=_LF;
{$ENDIF}
out+='! indices of IEX-related species'+_LF;                                           // '+isoatom+'
out+='  INTEGER, PARAMETER :: NIXSPEC = '+inttostr(_iesl)+_LF;                                      // '+isoatom+'
if (_iesl>0) then		   // no KIE in the system = zero-sized array (for compatibility)
   out+='  INTEGER, PARAMETER :: IXSIND(NIXSPEC) = &'+_LF+wrap(a,4,4)                           // 2x '+isoatom+'
else
    out+='  INTEGER, PARAMETER :: IXSIND(NIXSPEC+1) = (/ NIXSPEC /)'+_LF;			    // 3x '+isoatom+'


imcom_make_kie_iex_relspecs:=out;

end;

// -----------------------------------------------------------------------------
// outputs an array of quantities of isoatoms in each species
// prefix: D for doubling, T for tagging
function imcom_make_atom_qties : ansistring;
var i : word;
    a : ansistring;
const pref = {$IFDEF TAG}'T'{$ENDIF}{$IFDEF DBL}'D'{$ENDIF};
begin

a:='';
for i:=1 to _utsl do
    a:=a+inttostr(tsl[utsl[i]].qatm)+', ';
setlength(a,length(a)-2); a:='    (/ '+a+' /)';

a:=_LF+
   '! quantity of '+isoatom+' atoms in corresponding '+
   {$IFDEF TAG}'tagged'{$ENDIF}{$IFDEF DBL}'doubled'{$ENDIF}+
   ' species'+_LF+
   '  INTEGER, PARAMETER :: {%NQATOM}({%NSPEC}) = &'+_LF+
   wrap(a,4,4);

imcom_maKe_reps(a);
imcom_make_atom_qties:=a;

end;

// imcom_parse_proc ------------------------------------------------------------
// parses EQN processing code, place to handle KIE
function imcom_parse_proc(fname : string) : ansistring;
var t : text;
    a, u, p, out :  ansistring;
begin

write(_LF+'  imcom_parse_proc(',fname,'): ');

imcom_check_files_exist([fname]);

assign(t,fname);    // processing file
reset(t);

out:='';            // expected output
while not(eof(t)) do
      begin

      readln(t,a);
      imcom_make_reps(a);

      if pos('{>',a)>0 then
	 begin
	 // get control property and value letter    (e.g. ATOM:C or ISO:2 or SPEC:Ch3Br etc.)
	 p:=upcase(trim(imcom_ext4marks(a,'{>',':')));
         u:=trim(imcom_ext4marks(a,':','}'));

         if not(imcom_condition(p,u)) then
	    repeat
		  readln(t,a);
	    until ( ((pos(('{<'),a)>0) and (pos(uppercase(p),uppercase(a))>0)) or (eof(t)) );

	 end
      else
	  if pos('{<',a)>0 then
	     begin {do nothing} end
	  else
	      out:=out+a+_LF;
      end;

close(t);

writeln('done');

imcom_parse_proc:=out;

end;


// imcom_parse_eq_str_arrlen ---------------------------------------------------
// parses given string and expands with spaces (or shrinks) any string 
// expression occurence (i.e. 'I12DOsa') to the length of the type nstr
// (made foe fortran compilers compatibility
function imcom_parse_eq_strarr_len(s : ansistring) : ansistring;
var o : ansistring;
    i, l, z : longint;
    m : boolean;
    
begin

z:=sizeof(nstr)-1;
o:='';
m:=false; l:=0;
for i:=1 to length(s) do
    begin
    
    if s[i]='''' then 
       begin
       m:=not(m);       // m is a flag "inside of the string"
       if m then l:=0   // l is a length of the element
            else
                while (l<=z) do
                      begin o+=' '; inc(l); end;
               
       end;
       
    if (l<=z) or (s[i]='''') or not(m) then
       o+=s[i];
    if (m) then inc(l);
    
    end;

imcom_parse_eq_strarr_len:=o;

end;


// --- update_ptracs -----------------------------------------------------------
// adds tagged reactions abbrs. to the inter-conf. tagged reacs. list
procedure imcom_update_ptracs;

{$IFDEF USE_PT}
procedure add_ptrac(abbr : string);
begin
if (pos(':'+abbr+'>',ptracs_intr)=0) then
   begin
   ptracs_intr+=':'+abbr+'>';           // adding to the PTs list
   inc(_ptracs_intr);
   end;
if (pos(':'+abbr+'>',ptracs_conf)=0) then
   begin
   ptracs_conf+=':'+abbr+'>';           // adding to the PTs list
   inc(_ptracs_conf);
   end;
end;

var i, j, k : integer;
    has_pt : boolean;                   // flag showing if the eqn has already a PT
begin

for i:=1 to _eqs do
    if (eqs[i].itag) then               // if tagged, should have a passive tracer added
       begin
       has_pt:=false;
       for j:=1 to eqs[i]._prod do
           if (eqs[i].prod[j]=substr('@',eqs[i].abbr,ptsyntax)) then
              begin has_pt:=true; break; end;
       if not(has_pt) then    
          add_ptrac(eqs[i].abbr);
       end;

// additional tracers for KIE monitoring
{$IFDEF USE_PT_KIE}
for k:=1 to _kie do
    if (kie[k].imec) then       // if KIE exist in this configuration
       with kie[k] do
            for j:=1 to _isos do
 	        if not( in_tsl(eqs[kie[k].eqno].educ[1]) and in_tsl(eqs[kie[k].eqno].educ[2]) ) then    // not a quadrupl. reaction
		   add_ptrac(abbr+cfgname+clsname[j])
                else
 {$IFNDEF DBL_EXPL}
		    begin	    // in case both educts are tagged (quadrupled equation = quad. kie PTs)
		    add_ptrac(kie[k].abbr+cfgname+clsname[j]+'i1');
		    add_ptrac(kie[k].abbr+cfgname+clsname[j]+'i2');
                    end;
 {$ELSE}
		    for i:=1 to _isos do  // in case of explicit doubling (_iso-replicated equations with KIE)
		        add_ptrac(kie[k].abbr+cfgname+clsname[j]+'e'+inttostr(i));
 {$ENDIF}
{$ENDIF}

writeln('imcom_update_ptracs: done [',_ptracs_conf,' configuration, ',_ptracs_intr,' total PTs]');
{$ELSE}  // USE_PT
begin
{$ENDIF} // USE_PT
end;


// imcom_ptracs_list (returns string with the passive tracers list -------------
// outputs the passive tracers list according to specimen with @, $
function imcom_make_ptracs_list(ptstr, sample : string) : ansistring;
var tmp, smp, out : ansistring;
    no : integer;
begin
out:='';
tmp:=ptstr;
no:=1;
while (length(tmp)>0) do
      begin
      // one ptrac entry is like :G4410>
      smp:=copy2symbdel(tmp,':');   // rem ':'
      smp:=copy2symbdel(tmp,'>');   // copy to '>'

      smp:=substr(sample,'@',smp);
      smp:=substr(smp,'$',inttostr(no));

      out+=smp;
      inc(no);
      end;
out:=trim(out);                // cut leading spaces
imcom_make_ptracs_list:=out;
end;

// imcom_make_ptracs (returns string with the passive tracers indices defs) ----
// in: boolean indicating if the list is created for the inter. former
function imcom_make_ptracs(is_intr : boolean) : ansistring;
var i, j,  _pts : integer;
    a, out, pts : ansistring;
    m : nstr;
const l : char = {$IFDEF TAG}'T'{$ENDIF}{$IFDEF DBL}'D'{$ENDIF};
begin

out:='';

{$IFDEF USE_PT}
if (is_intr) then
   begin _pts:=_ptracs_intr; pts:=ptracs_intr; m:=''; end
else
    begin _pts:=_ptracs_conf; pts:=ptracs_conf; m:=isoatom; end;

out+='! number of tagged reactions'+_LF;
out+='  INTEGER, PARAMETER :: N'+l+m+'REAC = '+inttostr(_pts)+_LF;
out+='! added passive tracers indices'+_LF;
out+='  INTEGER, PARAMETER :: '+l+'RPT'+m+'IND(N'+l+m+'REAC) = &'+_LF;
out+='    (/ ';
a:=imcom_make_ptracs_list(pts,' '+substr(sisyntax,'@',ptsyntax)+',');
setlength(a,length(a)-1); // cut last comma 
out+=wrap(a,7,7);
out+=' /)'+_LF;

// for the interconfiguration former the internal reaction numbers can be added
if (is_intr) then
   begin
   j:=0; a:='';
   for i:=1 to _eqs do
       if (eqs[i].etag>0) then
          a+=inttostr(i)+', ';

   setlength(a,length(a)-2); // cut last comma 

   out+=_LF+'! tagged reactions internal nos.'+_LF;
// hopefully, N#RININD equals to N#REAC, otherwise diagnose internal problem!
// out+='  INTEGER, PARAMETER :: N'+l+'RININD = '+inttostr(j)+_LF;
// out+='  INTEGER, PARAMETER :: '+l+'RININD(N'+l+'RININD) = &'+_LF;
   out+='  INTEGER, PARAMETER :: '+l+'RININD(N'+l+m+'REAC) = &'+_LF;
   out+='    (/ '+wrap(a,7,7)+' /)'+_LF;
   end;

{$ENDIF}

imcom_make_ptracs:=out;
end;

// *** OUTDATED, TO BE REMOVED ****
// imcom_resetPTs --------------------------------------------------------------
// makes PTs (production tracers) initialization (reset to zero) code
// using specimens for index-name and value-expression (like ind_@ and C(ind_@)
function imcom_make_resetPTs(valpref, indpref : string) : ansistring;
var i : word;
    a : ansistring;
{$IFDEF USE_PT_KIE}
    b : ansistring;
    j, e : word;
 {$IFDEF DBL_EXPL}
    x : word;
 {$ENDIF}
{$ENDIF}

begin
{$IFDEF USE_PT}

if indpref='' then indpref:='ind_'+ptsyntax;
if valpref='' then valpref:='C('+indpref+')';

a:='';

// unset USE_IF_IN_RESETPT to optimize (attention!)

a:=a+'! - [imtag/double] generated ------------------------------------------------'+_LF+_LF;
a:=a+'! production tracers initialization ('+indpref+', '+valpref+')'+_LF+_LF;

{$IFNDEF DBL_EXPL}
for i:=1 to _eqs do
    if (eqs[i].itag) then
       begin
       a:=a+'    ';
{$IFDEF USE_IF_IN_RESETPT}
       a:=a+'IF ('+substr(indpref,'@',eqs[i].abbr)+' /= 0) ';
{$ENDIF}
       a:=a+substr(valpref,'@',eqs[i].abbr)+' = 0.0_dp'+_LF;
       end;
{$ENDIF}

{$IFDEF USE_PT_KIE}        // if monitoring KIE, all isotopologues (only for doubling)
a:=a+_LF+'  ! reactions with KIE'+_LF;
b:='';                         // added reactions list
for i:=1 to _kie do
    for e:=1 to _eqs do
	if (eqs[e].itag) then
	   if (eqs[e].abbr=kie[i].abbr) and (pos('>'+kie[i].abbr+'<',b)=0) then
	      begin
	      for j:=1 to _isos do
		  if not(in_tsl(eqs[e].educ[1]) and in_tsl(eqs[e].educ[2])) then
		     begin
		     a:=a+'    ';
 {$IFDEF USE_IF_IN_RESETPT}
		     a:=a+'IF ('+substr(indpref,'@',kie[i].abbr+clsname[j]{+kie[i].isot}+' /= 0) ';
 {$ENDIF}
		     a:=a+substr(valpref,'@',kie[i].abbr+clsname[j]{+kie[i].isot})+' = 0.0_dp'+_LF;
		     end
		  else
 {$IFNDEF DBL_EXPL}
		      begin	    // in case both educts are tagged (quadrupled equation = quad. kie PTs)
		      a:=a+'    ';
  {$IFDEF USE_IF_IN_RESETPT}
		      a:=a+'IF ('+substr(indpref,'@',kie[i].abbr+clsname[j]+'i1')+' /= 0) ';
  {$ENDIF}
		      a:=a+substr(valpref,'@',kie[i].abbr+clsname[j]+'i1')+' = 0.0_dp'+_LF;

		      a:=a+'    ';
  {$IFDEF USE_IF_IN_RESETPT}
		      a:=a+'IF ('+substr(indpref,'@',kie[i].abbr+clsname[j]+'i2')+' /= 0) ';
  {$ENDIF}
		      a:=a+substr(valpref,'@',kie[i].abbr+clsname[j]+'i2')+' = 0.0_dp'+_LF;
		      end;
 {$ELSE}
		      for x:=1 to _isos do  // in case of explicit doubling (_iso-replicated equations with KIE)
			  begin
			  a:=a+'    ';
  {$IFDEF USE_IF_IN_RESETPT}
			  a:=a+'IF ('substr(indpref,'@',kie[i].abbr+clsname[j]+'e'+inttostr(x))+' /= 0) ';
  {$ENDIF}
			  a:=a+substr(valpref,'@',kie[i].abbr+clsname[j]+'e'+inttostr(x))+' = 0.0_dp'+_LF;
			  end;
 {$ENDIF}
	      b:=b+'>'+kie[i].abbr+'<';
	      end;
{$ENDIF}

imcom_make_resetPTs:=a;
{$ENDIF}
end;


// imcom_make_tracdecl ---------------------------------------------------------
// outputs tracers declaration code
function imcom_make_tracdecl(tagtrac : string) : ansistring;

var i, j, s : word;
    a, b, out : ansistring;

const l : char = {$IFDEF TAG}'T'{$ENDIF}{$IFDEF DBL}'D'{$ENDIF};

begin

// default "tagged" tracers naming
if tagtrac='' then tagtrac:={$IFDEF TAG}'ind_t@'{$ENDIF}{$IFDEF DBL}'ind_t@'{$ENDIF};

out:='! data declaration'+_LF;
out+=_LF;
out+='! number of tagged species, number of classes (isotopologues)'+_LF;
out+='  INTEGER, PARAMETER :: {%NSPEC} = '+inttostr(_utsl)+
                           ', {%NCLASS} = '+inttostr(_isos)+
                           ', {%NISO} = '+inttostr(_isos)+_LF;
out+=_LF;
out+='! isotopologues tracers indices'+_LF;
for s:=1 to _utsl do
    with tsl[utsl[s]] do
	 begin
	 out+='  INTEGER, PARAMETER :: '+substr(tagtrac,'@',spec)+' = '+inttostr(s);
	 if (ifix) then out+='          ! indicated as fixed';
         out+=_LF;
         end;
out+=_LF;

{$IFDEF ADD_DUMIND}
out+='! dummy indices of species present TSL but not in the mechanism (resolves dependency problems)'+_LF;
for s:=1 to _tsl do
    if (not(is_usedspec(tsl[s].spec))) then
       out+='  INTEGER, PARAMETER :: '+substr(tagtrac,'@',tsl[s].spec)+' = 0'+_LF;
out+=_LF;
{$ENDIF}


// --- species indices ---
{$IF DEFINED(DBL) OR DEFINED(CRT_INTEQN)}
out+='! corresponding to regular(0) <-> doubled (1, 2, ...) species indices '+_LF;
out+='  INTEGER, PARAMETER :: {%RSIND}({%NSPEC},0:{%NCLASS}) = RESHAPE( &'+_LF;

a:='    (/ ';

// regulair
for i:=1 to _utsl do
    a:=a+substr(sisyntax,'@',tsl[utsl[i]].spec)+', ';
// en toute, esthetique
a:=a+'&';

// isotopologues
for j:=1 to _isos do
    begin
    for i:=1 to _utsl do
	a:=a+substr(sisyntax,'@',tsl[utsl[i]].isos[j])+', ';

    // en toute, esthetique
    a:=a+'&';
    end;
setlength(a,length(a)-3);  // cut last comma and ampersand
a:=a+' /), (/ {%NSPEC}, 1+{%NCLASS} /) )';

out+=wrap(a,7,7)+_LF;
out+=_LF;


// --- totals ---
out+='! (regular+doubled) totals indices'+_LF;
out+='  INTEGER, PARAMETER :: {%TDIND}(0:{%NCLASS}) = &'+_LF;
// regulair
a:='    (/ '+substr(sisyntax,'@',cfgname+'T'+isoatom);
// isotopiques
for j:=1 to _isos do
    a:=a+', '+substr(sisyntax,'@',cfgname+'T'+clsname[j]);
// en toute, esthetique
a:=a+' /)';
out+=wrap(a,7,7);

{$ELSE}
out+='! corresponding regular <-> tagged species indices (0:0 for a compatibility)'+_LF;
out+='  INTEGER, PARAMETER :: {%RSIND}({%NSPEC},0:0) = RESHAPE( &'+_LF;

a:='';
for i:=1 to _utsl do
    a:=a+substr(sisyntax,'@',tsl[utsl[i]].spec)+', ';
setlength(a,length(a)-2);

out+='    (/ '+wrap(a,7,7)+' /), (/ {%NSPEC}, 1 /) & )'+_LF;
{$ENDIF}

out+=_LF+imcom_make_atom_qties+_LF;

{$IFDEF USE_PT}
out+=_LF+imcom_make_ptracs(false)+_LF;
{$ENDIF}

{$IFDEF USE_KRSIND}
out+=_LF+imcom_make_kie_iex_relspecs(sisyntax,tagtrac)+_LF;
{$ENDIF}

imcom_make_reps(out);
imcom_make_tracdecl:=out;

end;


// imcom_make_specslist ---------------------------------------------------------
// outputs species list from according to a sample
function imcom_make_specslist(sample : string) : ansistring;

var i, j, s : word;
    a, b, out : ansistring;

begin
out:='';

b:=sample;
imcom_make_reps(b);

for i:=1 to _utsl do
    begin
    a:=substr(b,'$',inttostr(i));
    a:=substr(a,'@',tsl[utsl[i]].spec);
    out+=a;
    end;
    
imcom_make_specslist:=out;
end;



// imcom_x0 ----------------------------------------------------
// code for x0 initialization procedure
function imcom_make_x0(classno, expstr : string) : ansistring;
var i, c, cmin, cmax : integer;
    out, a : ansistring;
begin

// getting the class # (Nth - for Nth minor isotopologue or class)
if (pos('#',classno)>0) then
   begin cmin:=1; cmax:=_isos; end
else
    begin
    cmin:=strtointdef(trim(classno),1);         // default = 1
    cmax:=cmin;
    end;

out:='  ! TODO: specify the initial values here:'+_LF;
out+=_LF;

for c:=cmin to cmax do
    begin
    
    if (c>_isos) then
       begin
       write('<!> ');
       imcom_make_x0:='  ! imcom_make_x0('+trim(classno)+','+expstr+'): minor class # exceeds number of given in this configuration <may be normal>';
       exit;
       end;

    if (c=1) and (_isos>1) then
       begin
       write('<!> ');
       out+='  ! imcom_make_x0('+trim(classno)+','+expstr+'): requested class #1 for configuration with '+inttostr(_isos)+' classes <may be normal>'+_LF+_LF;
       end;

    // by default: MECCA indices
    if (expstr='') then expstr:='INIT_'+classno+'(ind_@) = $'; // something to have by default

    out+='  ! class: '+clsname[c]+_LF;
    for i:=1 to _utsl do
        if (tsl[utsl[i]].ival[c]<>'') then
           out+=substr( substr( substr(expstr,'@',tsl[utsl[i]].spec),
                                              '$',tsl[utsl[i]].ival[c]),
                                              '#',inttostr(c))+_LF;

    out+=_LF;
    end;
    
a:='';
for i:=1 to _tsl do	  // assuming #1 - 12C, #2 - 13C, tagging 12C/13C
    if not(is_usedspec(tsl[i].spec)) then
       a:=a+'! '+tsl[i].spec+', ';
setlength(a,min(length(a)-2,0));
if (length(a)>0) then
  out+=_LF+'! following species were not found in current selected mechanism:'+_LF+wrap(a,0,2);

imcom_make_x0:=out;

end;


// - util ----------------------------------------------------------------------

// -- checks and corrects the duplicate equations
procedure imcom_check_eqn_dupes(fname, dumspec : string);

function is_dupe_eq(eq1, eq2 : ansistring) : boolean;
var sl1, sl2 : tstringlist;
    sample : ansistring;
    i, j : integer;
const _esd_ = ['+','-'];        // delimiter of the members of the side of eq

begin

sl1:=tstringlist.create;
sl2:=tstringlist.create;

// processing left side
sl1.clear; sl2.clear;

sample:=copy(eq1,1,pos('=',eq1)-1);
for i:=1 to wordcount(sample,_esd_) do
    sl1.add( extractword(i,sample,_esd_) );

sample:=copy(eq2,1,pos('=',eq2)-1);
for i:=1 to wordcount(sample,_esd_) do
    sl2.add( extractword(i,sample,_esd_) );

sl1.sort; sl2.sort;

result:=sl2.equals(sl1);

if (result) then
   begin
   
   // processing right side
   sl1.clear; sl2.clear;
   sample:=copy(eq1,pos('=',eq1)+1,length(eq1)-pos('=',eq1));
   for i:=1 to wordcount(sample,_esd_) do
       sl1.add( extractword(i,sample,_esd_) );

   sample:=copy(eq2,pos('=',eq2)+1,length(eq2)-pos('=',eq2));
   for i:=1 to wordcount(sample,_esd_) do
       sl2.add( extractword(i,sample,_esd_) );

   sl1.sort; sl2.sort;

   result:=result and sl1.equals(sl2);
   end;

//if (result) then
//   begin
//   writeln('-dupe----------');
//   writeln(eq1);
//   writeln(eq2);
//   end;
   
sl1.destroy;
sl2.destroy;
   
end;

var f, o : textfile;
    e : array[1..max_eqs*max_isos] of record 
        eq : ansistring;
        line, corr : integer;
        end;
    a : ansistring;
    l, p, i, j : integer;

const runner = '-\|/'; //-.oOo.';

begin

write('check_eqn_dupes(',fname,'): ');

assign(f,fname);
reset(f);

p:=0; l:=0;
while not(eof(f)) do
      begin
      inc(l);
      readln(f,a);
      if imcom_is_eqn(a) then
         begin
         inc(p);
         e[p].eq:=imcom_ext4marks(a,'>',':');          // copier l'equation
         e[p].eq:=uppercase(delspace(trim(e[p].eq)));  // >JE+SUIS+UNE=EQUATION+0.5MAINTENANT:
         e[p].line:=l;
         e[p].corr:=0;
         end;
      end;
close(f);

// bubblesort comparison & correction
l:=1;
for i:=1 to p-1 do
    begin
    
    // most time-taking part, showing runner
    if ((i+j) mod (p div 32))=0 then 
       begin write(runner[(l mod length(runner))+1],#8); inc(l); end;

    for j:=i+1 to p do
        if is_dupe_eq(e[j].eq,e[i].eq) then
           begin
           inc(e[j].corr);
           e[j].eq+=uppercase('+'+dumspec);
           end;
    end;   

// apporter des modifications
assign(o,fname+'.$$$'); rewrite(o);
assign(f,fname); reset(f);

j:=1; // indice a e[]
l:=0; // indice a f
p:=0;
while not(eof(f)) do
      begin
      inc(l);
      readln(f,a);
      if (e[j].line=l) then
         begin
         
         if (e[j].corr>0) then
            begin
            insert('+ '+inttostr(e[j].corr)+' '+dumspec+' ',a,pos(':',a));
            inc(p);
            end;
            
         inc(j);
         end;
      writeln(o,a);
      end;
close(o); close(f);
erase(f); rename(o,fname);

if (p>0) then
   write(p,' identical equations found, corrected')
else
    write('no identical equations found');



writeln;

end;

// -- returns the parameters-defines block
function imcom_parameters : ansistring;
var s : ansistring;
    i : integer;
const ds : ansistring = '#define ';
begin

s:='! configuration parameters'+_LF+_LF;

{$IFDEF TAG}
// tagging name
s+=ds+'TAG'+_LF;
s+=ds+'tag_'+cfgname+_LF;
{$ENDIF}

{$IFDEF DBL}
// tagging name
s+=ds+'DBL'+_LF;
s+=ds+'dbl_'+cfgname+_LF;
 // explicit/implicit doubling
 {$IFDEF DBL_EXPL}
 s+=ds+'DBL_EXPLICIT';
 {$ELSE}
 s+=ds+'DBL_IMPLICIT';
 {$ENDIF}
s+=_LF;
{$ENDIF}

s+=_LF;

// abbr
s+=ds+'CONF_'+cfgname+_LF;

// atom
s+=ds;
if (isoatom<>'') then s+='ATOM_'+isoatom else s+='NO_ATOM';
s+=_LF;

// classes no
s+=ds+'CLASSES_'+inttostr(_isos)+_LF;

// classes names
for i:=1 to _isos do
    s+=ds+'CLASS_'+clsname[i]+_LF;

s+=_LF;

// kie presence
s+=ds;
if (_kie>0) then s+='KIE' else s+='NO_KIE';
s+=_LF;

// source specification presence
s+=ds;
if (_src>0) then
   s+='SRC' else s+='NO_SRC';
s+=_LF;

// isotope exchange reactions presence
s+=ds;
if (_iex>0) then
   s+='IEX' else s+='NO_IEX';
s+=_LF;

s+=_LF;

// usage of passive tracers flag
{$IFDEF USE_PT}
s+=ds+'USE_PT'+_LF;
 {$IFDEF USE_PT_KIE}
 s+=ds+'USE_PT_KIE'+_LF;
 {$ENDIF}
{$ENDIF}

// usage of reaction numbers flag
{$IFDEF USE_RN}
s+=ds+'USE_RN'+_LF;
{$ENDIF}

if (cparams<>'') then
   begin
   i:=1;
   s+=_LF+'! user configuration parameters'+_LF;
   while (extractword(i,cparams,_delims)<>'') do
         begin
         s+=_LF+ds+extractword(i,cparams,_delims);
         inc(i);
         end;
   s+=_LF;
   end;

// replacements made
s+=_LF+'! replacements:'+_LF+'!';
for i:=1 to _imcom_reps do
    s+=_LF+'! '+imcom_reps[i,1]+' is '+imcom_reps[i,2];

imcom_parameters:=s;

end;

// - inter-configuration part --------------------------------------------------

// maximum number of different configurations allowed
const max_conf = 5;

var conf : array[1..max_conf] of string;  // configuration-ID
   _conf : integer;                       // no. of configurations

// imcom_make_configslist ------------------------------------------------------
// make a list of samples with configurations names (for inter-conf. modules)
function imcom_make_configslist(sample : ansistring) : ansistring;
var i : integer;
    out : ansistring;
begin
out:='';
for i:=1 to _conf do
    out+=substr(sample,'@',conf[i])+_LF;
setlength(out,length(out)-length(_LF));    
imcom_make_configslist:=out;
end;


// -----------------------------------------------------------------------------
// imcom_init: initialization of some inter-configuration parameters (to safe 0s)
procedure imcom_init;
begin
_conf:=0;
_form_intr:=0;
{$IFDEF USE_PT}
   _ptracs_intr:=0; ptracs_intr:='';
{$ENDIF}
end;



// EOF
